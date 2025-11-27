{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------
-- DATUM & REDEEMER
------------------------------------------------------------

data ProposalDatum = ProposalDatum
    { pdActionsHash  :: BuiltinByteString
    , pdSnapshotSlot :: POSIXTime
    , pdQuorum       :: Integer
    , pdForVotes     :: Integer
    , pdAgainstVotes :: Integer
    , pdEndSlot      :: POSIXTime
    , pdExecuted     :: Bool
    }
PlutusTx.unstableMakeIsData ''ProposalDatum

data VoteChoice = For | Against
PlutusTx.unstableMakeIsData ''VoteChoice

data GovernorRedeemer = CastVote VoteChoice Integer -- weight
                      | ExecuteProposal
PlutusTx.unstableMakeIsData ''GovernorRedeemer

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE unPOSIXTime #-}
unPOSIXTime :: POSIXTime -> Integer
unPOSIXTime (POSIXTime n) = n

{-# INLINABLE currentSlot #-}
currentSlot :: ScriptContext -> Integer
currentSlot ctx =
    case ivFrom (txInfoValidRange $ scriptContextTxInfo ctx) of
        LowerBound (Finite t) _ -> unPOSIXTime t
        _ -> traceError "invalid range"

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: ProposalDatum -> GovernorRedeemer -> ScriptContext -> Bool
mkValidator dat redeemer ctx =
    case redeemer of

        -- CAST VOTE
        CastVote choice weight ->
            traceIfFalse "proposal ended" (currentSlot ctx <= unPOSIXTime (pdEndSlot dat)) &&
            traceIfFalse "weight must be positive" (weight > 0)

        -- EXECUTE
        ExecuteProposal ->
            traceIfFalse "proposal not ended" (currentSlot ctx > unPOSIXTime (pdEndSlot dat)) &&
            traceIfFalse "already executed" (not (pdExecuted dat)) &&
            traceIfFalse "quorum not reached" quorumReached
  where
    quorumReached :: Bool
    quorumReached = (pdForVotes dat >= pdQuorum dat)

------------------------------------------------------------
-- UNTYPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    let dat = unsafeFromBuiltinData @ProposalDatum d
        red = unsafeFromBuiltinData @GovernorRedeemer r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkUntyped ||])

------------------------------------------------------------
-- HASH + ADDRESS
------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        bs       = SBS.fromShort short
        builtin  = Builtins.toBuiltin bs
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript CS.PlutusScriptV2 plutusScript)

        addr :: CS.AddressInEra CS.BabbageEra
        addr = C.makeShelleyAddressInEra
                 network
                 (C.PaymentCredentialByScript scriptHash)
                 C.NoStakeAddress
    in T.unpack (C.serialiseAddress addr)

------------------------------------------------------------
-- FILE WRITING + MAIN
------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path v = do
    LBS.writeFile path (Serialise.serialise v)
    putStrLn $ "Validator written: " <> path

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "governor.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Governor Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "--------------------------------------"

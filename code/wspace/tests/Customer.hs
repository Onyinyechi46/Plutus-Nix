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
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.AssocMap as AssocMap

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

data PointsDatum = PointsDatum
    { pdBalance :: Integer
    }
PlutusTx.unstableMakeIsData ''PointsDatum

data TierSBT = TierSBT
    { tsLevel  :: Integer
    , tsExpiry :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''TierSBT

data RewardAction = AddPoints Integer
                  | RedeemPoints Integer
                  | UpgradeTier TierSBT
PlutusTx.unstableMakeIsData ''RewardAction

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE findPointsDatum #-}
findPointsDatum :: TxInfo -> Maybe PointsDatum
findPointsDatum info =
    case AssocMap.elems (txInfoData info) of
        []    -> Nothing
        (d:_) -> Just (unsafeFromBuiltinData (getDatum d))

{-# INLINABLE currentTime #-}
currentTime :: ScriptContext -> POSIXTime
currentTime ctx =
    case ivFrom (txInfoValidRange $ scriptContextTxInfo ctx) of
        LowerBound (Finite t) _ -> t
        _ -> traceError "invalid range"

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PointsDatum -> RewardAction -> ScriptContext -> Bool
mkValidator datum action ctx =
    case action of

      -- ADD POINTS
      AddPoints pts ->
          traceIfFalse "points must be positive" (pts > 0)

      -- REDEEM POINTS
      RedeemPoints pts ->
          traceIfFalse "not enough balance" (pdBalance datum >= pts)

      -- UPGRADE TIER
      UpgradeTier tier ->
          traceIfFalse "tier level positive" (tsLevel tier > 0) &&
          traceIfFalse "tier not expired" (currentTime ctx <= tsExpiry tier)

------------------------------------------------------------
-- UNTYPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    let dat = unsafeFromBuiltinData @PointsDatum d
        red = unsafeFromBuiltinData @RewardAction r
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
    writeValidator "rewards.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Rewards Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "--------------------------------------"

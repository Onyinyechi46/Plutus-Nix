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

data PostDatum = PostDatum
    { pdAuthor  :: PubKeyHash
    , pdToken   :: CurrencySymbol
    , pdPostRef :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''PostDatum

data SocialAction = Pay | GatedAccess
PlutusTx.unstableMakeIsData ''SocialAction

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE scriptInputContainsAsset #-}
scriptInputContainsAsset :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsAsset ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no script input"
        Just i  ->
            let v = txOutValue (txInInfoResolved i)
            in valueOf v cs tn >= 1

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PostDatum -> SocialAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        Pay ->
            traceIfFalse "script input missing token" (scriptInputContainsAsset ctx (pdToken dat) (TokenName "SOCIAL"))
        GatedAccess ->
            traceIfFalse "access requires token" (scriptInputContainsAsset ctx (pdToken dat) (TokenName "SOCIAL"))

------------------------------------------------------------
-- SAFE UNTYPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
    case (fromBuiltinData d, fromBuiltinData r, fromBuiltinData c) of
        (Just dat, Just red, Just ctx) ->
            if mkValidator dat red ctx then () else traceError "validator failed"
        _ -> traceError "invalid datum/redeemer/context"

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
    writeValidator "social-media.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Social Media Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "--------------------------------------"

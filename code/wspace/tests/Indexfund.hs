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
import PlutusTx.AssocMap as Map

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------
-- ON-CHAIN DATATYPES
------------------------------------------------------------

data Component = Component
    { cCurrency :: CurrencySymbol
    , cToken    :: TokenName
    , cWeight   :: Integer
    }
PlutusTx.unstableMakeIsData ''Component

data BasketDatum = BasketDatum
    { bComponents :: [Component]
    , bTotalWeight :: Integer
    , bVersion   :: Integer
    }
PlutusTx.unstableMakeIsData ''BasketDatum

data BasketAction = MintBasketAction Integer
                  | RedeemBasketAction Integer
                  | RebalanceAction BasketDatum
PlutusTx.unstableMakeIsData ''BasketAction

------------------------------------------------------------
-- HELPERS
------------------------------------------------------------

{-# INLINABLE sumWeights #-}
sumWeights :: [Component] -> Integer
sumWeights comps = foldr (\c acc -> cWeight c + acc) 0 comps

{-# INLINABLE scale #-}
scale :: Integer -> Integer -> Integer -> Integer
scale a num denom = divide (a * num) denom

{-# INLINABLE findDatumInInfo #-}
findDatumInInfo :: TxInfo -> Maybe BasketDatum
findDatumInInfo info =
    case Map.elems (txInfoData info) of
        []    -> Nothing
        (d:_) -> Just (unsafeFromBuiltinData (getDatum d))

------------------------------------------------------------
-- MINTING POLICY
------------------------------------------------------------

{-# INLINABLE baskTokenName #-}
baskTokenName :: TokenName
baskTokenName = TokenName "BASK"

{-# INLINABLE mkBasketPolicy #-}
mkBasketPolicy :: BuiltinData -> BuiltinData -> ()
mkBasketPolicy _ ctxBuiltin =
    let ctx :: ScriptContext
        ctx = unsafeFromBuiltinData ctxBuiltin

        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownCS :: CurrencySymbol
        ownCS = ownCurrencySymbol ctx

        mintedAmount :: Integer
        mintedAmount = valueOf (txInfoMint info) ownCS baskTokenName

        mDatum :: Maybe BasketDatum
        mDatum = findDatumInInfo info
    in case mDatum of
        Nothing -> traceError "basket datum not present"
        Just bd ->
            if mintedAmount > 0
                then
                    let comps = bComponents bd
                        totalW = if bTotalWeight bd /= 0 then bTotalWeight bd else sumWeights comps

                        checkComp comp =
                            let neededTotal = Main.scale mintedAmount (cWeight comp) totalW
                                sumInputsValue = foldr (\inp acc -> acc + valueOf (txOutValue $ txInInfoResolved inp) (cCurrency comp) (cToken comp)) 0 (txInfoInputs info)
                            in traceIfFalse "component deposit insufficient" (sumInputsValue >= neededTotal)
                    in if foldr (\c acc -> checkComp c && acc) True comps then () else error ()
                else if mintedAmount < 0
                    then
                        let burnAmt = negate mintedAmount
                            comps = bComponents bd
                            totalW = if bTotalWeight bd /= 0 then bTotalWeight bd else sumWeights comps

                            checkReturn comp =
                                let neededTotal = Main.scale burnAmt (cWeight comp) totalW
                                    sumOutputsValue = foldr (\o acc -> acc + valueOf (txOutValue o) (cCurrency comp) (cToken comp)) 0 (txInfoOutputs info)
                                in traceIfFalse "component payout insufficient" (sumOutputsValue >= neededTotal)
                        in if foldr (\c acc -> checkReturn c && acc) True comps then () else error ()
                    else traceError "zero minted"

{-# INLINABLE mkPolicySerialised #-}
mkPolicySerialised :: BuiltinData -> BuiltinData -> ()
mkPolicySerialised = mkBasketPolicy

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicySerialised ||])

------------------------------------------------------------
-- BASKET VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkBasketValidator #-}
mkBasketValidator :: BasketDatum -> BasketAction -> ScriptContext -> Bool
mkBasketValidator dat action ctx =
    case action of
        MintBasketAction amt -> traceIfFalse "mint must be positive" (amt > 0)
        RedeemBasketAction amt -> traceIfFalse "redeem must be positive" (amt > 0)
        RebalanceAction newDatum ->
            traceIfFalse "version must increase" (bVersion newDatum > bVersion dat)

{-# INLINABLE mkBasketValidatorUntyped #-}
mkBasketValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBasketValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @BasketDatum d
        red = unsafeFromBuiltinData @BasketAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkBasketValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkBasketValidatorUntyped ||])

------------------------------------------------------------
-- SCRIPT HASH / ADDRESS
------------------------------------------------------------

{-# INLINABLE plutusValidatorHash #-}
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

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
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "basket-validator.plutus" validator

    let vh     = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator

    putStrLn "\n--- Basket Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Basket validator generated successfully."

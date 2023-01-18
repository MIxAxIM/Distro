
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module  Regulator.Contract
            (   regulatorMintingPolicy
            )
                where

import           Plutus.V1.Ledger.Address  (Address, scriptHashAddress,
                                            toPubKeyHash)
import           Plutus.V1.Ledger.Crypto   (PubKeyHash)
import           Plutus.V1.Ledger.Scripts  (Datum (..))
import           Plutus.V1.Ledger.Value    (assetClass, flattenValue, valueOf)
import           Plutus.V2.Ledger.Contexts (ScriptContext (..),
                                            TxInInfo (txInInfoResolved),
                                            TxInfo (..), TxOut (..),
                                            ownCurrencySymbol, txSignedBy)
import           Plutus.V2.Ledger.Tx       (OutputDatum (..))
import           PlutusTx                  (BuiltinData, unsafeFromBuiltinData)
import           PlutusTx.Prelude          (Bool (False), Maybe (Just), any,
                                            error, filter, otherwise,
                                            traceError, traceIfFalse, ($), (&&),
                                            (==))

import           Distro.DataTypes          (DistroDatum (..), PhaseOneInfo (..),
                                            PhaseTwoInfo (..))
import           Regulator.DataTypes       (DevTeamAddresses (..),
                                            RegulatorAction (..),
                                            RegulatorParams (..))

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINABLE regulatorMintingPolicy #-}
regulatorMintingPolicy :: RegulatorParams -> BuiltinData -> BuiltinData -> ()
regulatorMintingPolicy
    RegulatorParams
        {   phaseOneInfo = PhaseOneInfo{..}
        ,   phaseTwoInfo = PhaseTwoInfo{..}
        ,   devTeamAddresses = DevTeamAddresses{..}
        , ..
        }
    rawRedeemer
    rawCTX

    -----------------------------------------------------------------------------------------
    -- |                                   GENESIS MINT                                    |--
    -----------------------------------------------------------------------------------------
    |   GenesisMint <- redeemer
    ,   DistroDatum
            happyToken
            (PhaseOneInfo
                firstDevPhaseOneClaimAmount'
                firstDevDidPhaseOne'
                secondDevPhaseOneClaimAmount'
                secondDevDidPhaseOne'
                dateOfPhaseOne')
            (PhaseTwoInfo
                firstDevPhaseTwoClaimAmount'
                firstDevDidPhaseTwo'
                secondDevPhaseTwoClaimAmount'
                secondDevDidPhaseTwo'
                dateOfPhaseTwo') <- getTxOutInlineDatum txOutToDistro
    ,   traceIfFalse "Only 1 Happy Token Must Be Minted"            isTokenMintedCorrectly
    ,   traceIfFalse "Happy Token Must Be At Distro SC Output"  $   valueOf (txOutValue txOutToDistro) (ownCurrencySymbol ctx) happyTokenName == 1
    ,   traceIfFalse "Happy Token Info Mismatched"              $   happyToken ==  assetClass (ownCurrencySymbol ctx) happyTokenName

    ,   traceIfFalse "firstDevPhaseOneClaimAmount Mismatched"   $ firstDevPhaseOneClaimAmount   ==  firstDevPhaseOneClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseOne Mismatched"           $ firstDevDidPhaseOne           ==  firstDevDidPhaseOne'
    ,   traceIfFalse "secondDevPhaseOneClaimAmount Mismatched"  $ secondDevPhaseOneClaimAmount  ==  secondDevPhaseOneClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseOne Mismatched"          $ secondDevDidPhaseOne          ==  secondDevDidPhaseOne'
    ,   traceIfFalse "dateOfPhaseOne Mismatched"                $ dateOfPhaseOne                ==  dateOfPhaseOne'

    ,   traceIfFalse "firstDevPhaseTwoClaimAmount Mismatched"   $ firstDevPhaseTwoClaimAmount   ==  firstDevPhaseTwoClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseTwo Mismatched"           $ firstDevDidPhaseTwo           ==  firstDevDidPhaseTwo'
    ,   traceIfFalse "secondDevPhaseTwoClaimAmount Mismatched"  $ secondDevPhaseTwoClaimAmount  ==  secondDevPhaseTwoClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseTwo Mismatched"          $ secondDevDidPhaseTwo          ==  secondDevDidPhaseTwo'
    ,   traceIfFalse "dateOfPhaseTwo Mismatched"                $ dateOfPhaseTwo                ==  dateOfPhaseTwo'
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                              FIRST DEVELOPER MINT                                |--
    -----------------------------------------------------------------------------------------
    |   FirstDeveloperAction firstDevAddress' <- redeemer
    ,   traceIfFalse "First Developer Must Sign"                $ txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)
    ,   traceIfFalse "First Developer Address Mismatch"         $ firstDevAddress == firstDevAddress'
    ,   traceIfFalse "Distro Contract Must Be Present at Tx"    isDistroSCPresent
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                              SECOND DEVELOPER MINT                               |--
    -----------------------------------------------------------------------------------------
    |   SecondDeveloperAction secondDevAddress' <- redeemer
    ,   traceIfFalse "Second Developer Must Sign"               $ txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)
    ,   traceIfFalse "Second Developer Address Mismatch"        $ secondDevAddress == secondDevAddress'
    ,   traceIfFalse "Distro Contract Must Be Present at Tx"    isDistroSCPresent
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                                 DEBUGGER ACTION                                  |--
    -----------------------------------------------------------------------------------------
    |   RegulatorDebuggerAction <- redeemer
    ,   traceIfFalse "Regulator Debugger Must Sign"  $ txSignedBy ctxTxInfo regulatorDebuggerPKH
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                                 ANY OTHER CASE                                   |--
    -----------------------------------------------------------------------------------------
    |   otherwise   =   error()

            where

                redeemer :: RegulatorAction
                redeemer = unsafeFromBuiltinData @RegulatorAction rawRedeemer

                ctx :: ScriptContext
                ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..}}  = unsafeFromBuiltinData @ScriptContext rawCTX

                ctxTxInfo :: TxInfo
                ctxTxInfo = scriptContextTxInfo ctx

                getPaymentPKH :: Address -> PubKeyHash
                getPaymentPKH address
                    | Just pkh <- toPubKeyHash address = pkh
                    | otherwise = traceError "No PubKeyHash"

                getTxOutInlineDatum :: TxOut -> DistroDatum
                getTxOutInlineDatum tx
                    | ( OutputDatum ( Datum inline )) <- txOutDatum tx = unsafeFromBuiltinData @DistroDatum inline
                    | otherwise = traceError "No Inline Datum"

                isTokenMintedCorrectly :: Bool
                isTokenMintedCorrectly
                    | [(cs,_,amt)] <- flattenValue txInfoMint = cs == ownCurrencySymbol ctx && amt == 1
                    | otherwise = False

                txOutToDistro :: TxOut
                txOutToDistro
                    | [o] <- filter (\o -> txOutAddress o == scriptHashAddress distroContractVH ) txInfoOutputs = o
                    | otherwise = traceError "No TxOut To Distro SC"

                isDistroSCPresent :: Bool
                isDistroSCPresent =
                    any (\i -> txOutAddress (txInInfoResolved i) == scriptHashAddress distroContractVH) txInfoInputs


{-============================================== END OF MINTING POLICY SECTION =================================================-}

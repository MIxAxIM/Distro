
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
    -- |                                   GENESIS MINT                                   |--
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

    ,   isTokenMintedCorrectly
    ,   valueOf (txOutValue txOutToDistro) (ownCurrencySymbol ctx) happyTokenName == 1
    ,   happyToken      ==  assetClass (ownCurrencySymbol ctx) happyTokenName

    ,   firstDevPhaseOneClaimAmount   ==  firstDevPhaseOneClaimAmount'
    ,   firstDevDidPhaseOne           ==  firstDevDidPhaseOne'
    ,   secondDevPhaseOneClaimAmount  ==  secondDevPhaseOneClaimAmount'
    ,   secondDevDidPhaseOne          ==  secondDevDidPhaseOne'
    ,   dateOfPhaseOne                ==  dateOfPhaseOne'

    ,   firstDevPhaseTwoClaimAmount   ==  firstDevPhaseTwoClaimAmount'
    ,   firstDevDidPhaseTwo           ==  firstDevDidPhaseTwo'
    ,   secondDevPhaseTwoClaimAmount  ==  secondDevPhaseTwoClaimAmount'
    ,   secondDevDidPhaseTwo          ==  secondDevDidPhaseTwo'
    ,   dateOfPhaseTwo                ==  dateOfPhaseTwo'
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                              FIRST DEVELOPER MINT                                |--
    -----------------------------------------------------------------------------------------
    |   FirstDeveloperAction <- redeemer
    ,   txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)
    ,   isDistroSCPresent
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                              SECOND DEVELOPER MINT                               |--
    -----------------------------------------------------------------------------------------
    |   SecondDeveloperAction <- redeemer
    ,   txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)
    ,   isDistroSCPresent
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                                 DEBUGGER ACTION                                  |--
    -----------------------------------------------------------------------------------------
    |   RegulatorDebuggerAction <- redeemer
    ,   txSignedBy ctxTxInfo regulatorDebuggerPKH
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
                    | otherwise = error()

                getTxOutInlineDatum :: TxOut -> DistroDatum
                getTxOutInlineDatum tx
                    | ( OutputDatum ( Datum inline )) <- txOutDatum tx = unsafeFromBuiltinData @DistroDatum inline
                    | otherwise = error()

                isTokenMintedCorrectly :: Bool
                isTokenMintedCorrectly
                    | [(cs,_,amt)] <- flattenValue txInfoMint = cs == ownCurrencySymbol ctx && amt == 1
                    | otherwise = False

                txOutToDistro :: TxOut
                txOutToDistro
                    | [o] <- filter (\o -> txOutAddress o == scriptHashAddress distroContractVH ) txInfoOutputs = o
                    | otherwise = error()

                isDistroSCPresent :: Bool
                isDistroSCPresent =
                    any (\i -> txOutAddress (txInInfoResolved i) == scriptHashAddress distroContractVH) txInfoInputs


{-============================================== END OF MINTING POLICY SECTION =================================================-}

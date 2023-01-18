
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

module Distro.Contract
    (   distroValidator
    )   where

import           Plutus.V1.Ledger.Address  (Address, toPubKeyHash)
import           Plutus.V1.Ledger.Crypto   (PubKeyHash)
import           Plutus.V1.Ledger.Interval (contains, from)
import           Plutus.V1.Ledger.Scripts  (Datum (..), Redeemer (..))
import           Plutus.V1.Ledger.Value    (AssetClass (..), CurrencySymbol,
                                            assetClassValueOf, flattenValue)
import           Plutus.V2.Ledger.Contexts (ScriptContext (..),
                                            ScriptPurpose (..),
                                            TxInInfo (txInInfoResolved),
                                            TxInfo (..),
                                            TxOut (txOutDatum, txOutValue),
                                            findOwnInput, getContinuingOutputs,
                                            txSignedBy)
import           Plutus.V2.Ledger.Tx       (OutputDatum (..))
import           PlutusTx                  (BuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.AssocMap         as Map
import           PlutusTx.Prelude          (Bool (False, True), Integer,
                                            Maybe (Just, Nothing), all, error,
                                            length, not, otherwise, traceError,
                                            traceIfFalse, ($), (&&), (+), (==))

import           Distro.DataTypes          (DistroAction (..), DistroDatum (..),
                                            DistroParams (..),
                                            PhaseOneInfo (..),
                                            PhaseTwoInfo (..))
import           Regulator.DataTypes       (RegulatorAction (..))


{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE distroValidator #-}
distroValidator :: DistroParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
distroValidator DistroParams{..} rawDatum rawRedeemer rawCTX

    -----------------------------------------------------------------------------------------
    -- |                     FIRST DEVELOPER CLAIMS AT PHASE ON                          |--
    -----------------------------------------------------------------------------------------
    |   (PhaseOneClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} phaseTwoInfo ) <- (redeemer , datum)

    ,   currencySymbolOf happyToken == mintingContractCS

    ,   (DistroDatum
            happyToken'
            (PhaseOneInfo
                firstDevPhaseOneClaimAmount'
                firstDevDidPhaseOne'
                secondDevPhaseOneClaimAmount'
                secondDevDidPhaseOne'
                dateOfPhaseOne')
            phaseTwoInfo') <- getTxOutInlineDatum singleScriptOutputUTxO

    ,   FirstDeveloperAction firstDevAddress <- getMintingRedeemer mintingContractCS

                    {-|--------------------------------Minting Handling----------------------------------|-}
    ,   assetClassValueOf txInfoMint happyToken == firstDevPhaseOneClaimAmount
    ,   length (flattenValue txInfoMint) == 1

                    {-|---------------------------------Signatories--------------------------------------|-}
    ,   txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)

                    {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   from dateOfPhaseOne `contains` txInfoValidRange

                    {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   not firstDevDidPhaseOne
    ,   happyToken == happyToken'
    ,   firstDevPhaseOneClaimAmount  ==  firstDevPhaseOneClaimAmount'
    ,   not firstDevDidPhaseOne      ==  firstDevDidPhaseOne'
    ,   secondDevPhaseOneClaimAmount ==  secondDevPhaseOneClaimAmount'
    ,   secondDevDidPhaseOne         ==  secondDevDidPhaseOne'
    ,   dateOfPhaseOne               ==  dateOfPhaseOne'
    ,   phaseTwoInfo == phaseTwoInfo'

                    {-|----------------------------------Input/Output------------------------------------|-}
    ,   assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,       isNumOfInputsHasDatum txInfoInputs 1
        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                       SECOND DEVELOPER CLAIMS AT PHASE ONE                       |--
    -----------------------------------------------------------------------------------------
    |   (PhaseOneClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} phaseTwoInfo ) <- (redeemer , datum)

    ,    currencySymbolOf happyToken == mintingContractCS

    ,   (DistroDatum
            happyToken'
            (PhaseOneInfo
                firstDevPhaseOneClaimAmount'
                firstDevDidPhaseOne'
                secondDevPhaseOneClaimAmount'
                secondDevDidPhaseOne'
                dateOfPhaseOne')
            phaseTwoInfo') <- getTxOutInlineDatum singleScriptOutputUTxO

    ,   SecondDeveloperAction secondDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   assetClassValueOf txInfoMint happyToken == secondDevPhaseOneClaimAmount
    ,   length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)

                {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   from dateOfPhaseOne `contains` txInfoValidRange

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   not secondDevDidPhaseOne
    ,   happyToken == happyToken'
    ,   firstDevPhaseOneClaimAmount  ==  firstDevPhaseOneClaimAmount'
    ,   firstDevDidPhaseOne          ==  firstDevDidPhaseOne'
    ,   secondDevPhaseOneClaimAmount ==  secondDevPhaseOneClaimAmount'
    ,   not secondDevDidPhaseOne     ==  secondDevDidPhaseOne'
    ,   dateOfPhaseOne               ==  dateOfPhaseOne'
    ,   phaseTwoInfo == phaseTwoInfo'

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,       isNumOfInputsHasDatum txInfoInputs 1
        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                       FIRST DEVELOPER CLAIMS AT PHASE TWO                        |--
    -----------------------------------------------------------------------------------------
    |   (PhaseTwoClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   currencySymbolOf happyToken == mintingContractCS

    ,   (DistroDatum
            happyToken'
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
                dateOfPhaseTwo')) <- getTxOutInlineDatum singleScriptOutputUTxO

    ,   FirstDeveloperAction firstDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   if firstDevDidPhaseOne
            then assetClassValueOf txInfoMint happyToken == firstDevPhaseTwoClaimAmount
            else assetClassValueOf txInfoMint happyToken == firstDevPhaseTwoClaimAmount + firstDevPhaseOneClaimAmount
    ,   length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)

                {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   from dateOfPhaseTwo `contains` txInfoValidRange

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   not firstDevDidPhaseTwo

    ,   happyToken == happyToken'

    ,   firstDevPhaseOneClaimAmount  ==  firstDevPhaseOneClaimAmount'
    ,   if firstDevDidPhaseOne
            then firstDevDidPhaseOne        ==  firstDevDidPhaseOne'
            else not firstDevDidPhaseOne    ==  firstDevDidPhaseOne'
    ,   secondDevPhaseOneClaimAmount ==  secondDevPhaseOneClaimAmount'
    ,   secondDevDidPhaseOne         ==  secondDevDidPhaseOne'
    ,   dateOfPhaseOne               ==  dateOfPhaseOne'
    ,   firstDevPhaseTwoClaimAmount  ==  firstDevPhaseTwoClaimAmount'
    ,   not firstDevDidPhaseTwo      ==  firstDevDidPhaseTwo'
    ,   secondDevPhaseTwoClaimAmount ==  secondDevPhaseTwoClaimAmount'
    ,   secondDevDidPhaseTwo         ==  secondDevDidPhaseTwo'
    ,   dateOfPhaseTwo               ==  dateOfPhaseTwo'

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,       isNumOfInputsHasDatum txInfoInputs 1
        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                       SECOND DEVELOPER CLAIMS AT PHASE TWO                       |--
    -----------------------------------------------------------------------------------------
    |   (PhaseTwoClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,    currencySymbolOf happyToken == mintingContractCS

    ,   (DistroDatum
        happyToken'
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
            dateOfPhaseTwo')) <- getTxOutInlineDatum singleScriptOutputUTxO

    ,   SecondDeveloperAction secondDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   if secondDevDidPhaseOne
            then assetClassValueOf txInfoMint happyToken == secondDevPhaseTwoClaimAmount
            else assetClassValueOf txInfoMint happyToken == secondDevPhaseTwoClaimAmount + secondDevPhaseOneClaimAmount
    ,   length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)

                {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   from dateOfPhaseTwo `contains` txInfoValidRange

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   not secondDevDidPhaseTwo
    ,   happyToken == happyToken'
    ,   firstDevPhaseOneClaimAmount   ==  firstDevPhaseOneClaimAmount'
    ,   firstDevDidPhaseOne           ==  firstDevDidPhaseOne'
    ,   secondDevPhaseOneClaimAmount  ==  secondDevPhaseOneClaimAmount'
    ,   if secondDevDidPhaseOne
            then secondDevDidPhaseOne       ==  secondDevDidPhaseOne'
            else not secondDevDidPhaseOne   ==  secondDevDidPhaseOne'
    ,   dateOfPhaseOne                ==  dateOfPhaseOne'
    ,   firstDevPhaseTwoClaimAmount   ==  firstDevPhaseTwoClaimAmount'
    ,   firstDevDidPhaseTwo           ==  firstDevDidPhaseTwo'
    ,   secondDevPhaseTwoClaimAmount  ==  secondDevPhaseTwoClaimAmount'
    ,   not secondDevDidPhaseTwo      ==  secondDevDidPhaseTwo'
    ,   dateOfPhaseTwo                ==  dateOfPhaseTwo'

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,       isNumOfInputsHasDatum txInfoInputs 1
        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                        FIRST DEVELOPER BURN GENESIS TOKEN                        |--
    -----------------------------------------------------------------------------------------

    |   (BurningGenesisToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   FirstDeveloperAction firstDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   assetClassValueOf txInfoMint happyToken == -1
    ,   length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,       firstDevDidPhaseOne
        &&  secondDevDidPhaseOne
        &&  firstDevDidPhaseTwo
        &&  secondDevDidPhaseTwo

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,       isNumOfInputsHasDatum txInfoInputs 1
        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 0
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                        SECOND DEVELOPER BURN GENESIS TOKEN                       |--
    -----------------------------------------------------------------------------------------
    |   (BurningGenesisToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   SecondDeveloperAction secondDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   assetClassValueOf txInfoMint happyToken == -1
    ,   length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,       firstDevDidPhaseOne
        &&  secondDevDidPhaseOne
        &&  firstDevDidPhaseTwo
        &&  secondDevDidPhaseTwo

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,       isNumOfInputsHasDatum txInfoInputs 1
        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 0
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                             DISTRO DEBUGGER ACTION                               |--
    -----------------------------------------------------------------------------------------

    |   ( DistroDebuggerAction
            , DistroDatum {} ) <- (redeemer , datum)

    ,   txSignedBy ctxTxInfo distroDebuggerPKH
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                                 ANY OTHER CASE                                   |--
    -----------------------------------------------------------------------------------------

    |   otherwise   =   error()

            where

                datum :: DistroDatum
                datum = unsafeFromBuiltinData @DistroDatum rawDatum

                redeemer :: DistroAction
                redeemer = unsafeFromBuiltinData @DistroAction rawRedeemer

                ctx :: ScriptContext
                ctx@ScriptContext{ scriptContextTxInfo = TxInfo{..} }  = unsafeFromBuiltinData @ScriptContext rawCTX

                ctxTxInfo :: TxInfo
                ctxTxInfo = scriptContextTxInfo ctx

                getMintingRedeemer :: CurrencySymbol -> RegulatorAction
                getMintingRedeemer cs
                    |   Just rawMintingRedeemer <- Map.lookup (Minting cs) txInfoRedeemers
                    =   unsafeFromBuiltinData @RegulatorAction $ getRedeemer rawMintingRedeemer
                    |   otherwise   = error()


                currencySymbolOf :: AssetClass -> CurrencySymbol
                currencySymbolOf (AssetClass (cs, _)) = cs

                singleScriptOutputUTxO :: TxOut
                singleScriptOutputUTxO
                    |   [o]         <-  getContinuingOutputs ctx = o
                    |   otherwise   =   error()

                singleScriptInputUTxO :: TxOut
                singleScriptInputUTxO
                    |   Just i      <-  findOwnInput ctx = txInInfoResolved i
                    |   otherwise   =   error()

                getPaymentPKH :: Address -> PubKeyHash
                getPaymentPKH address
                    |   Just pkh    <-  toPubKeyHash address = pkh
                    |   otherwise   =   error()

                isNumOfInputsHasDatum :: [TxInInfo] -> Integer -> Bool
                isNumOfInputsHasDatum utxos number = loopInputs utxos 0
                    where
                        loopInputs :: [TxInInfo] -> Integer -> Bool
                        loopInputs []     counter = counter == number
                        loopInputs (x:xs) counter
                            |   NoOutputDatum <- txOutDatum $ txInInfoResolved x = loopInputs xs counter
                            |   otherwise = loopInputs xs (counter + 1)

                isNumOutputsHasDatum :: [TxOut] -> Integer -> Bool
                isNumOutputsHasDatum utxos number = loopInputs utxos 0
                    where
                        loopInputs :: [TxOut] -> Integer  -> Bool
                        loopInputs []     counter = counter == number
                        loopInputs (x:xs) counter
                            |   NoOutputDatum <- txOutDatum x = loopInputs xs counter
                            |   otherwise = loopInputs xs (counter + 1)

                getTxOutInlineDatum :: TxOut -> DistroDatum
                getTxOutInlineDatum tx
                    | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                    = unsafeFromBuiltinData @DistroDatum inline
                    | otherwise = error()


{-================================================== END OF VALIDATOR SECTION ====================================================-}

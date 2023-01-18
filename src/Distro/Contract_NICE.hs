
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
    -- |                     FIRST DEVELOPER CLAIMS AT PHASE ONE                          |--
    -----------------------------------------------------------------------------------------
    |   (PhaseOneClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} phaseTwoInfo ) <- (redeemer , datum)

    ,    traceIfFalse "Wrong Minting Contract Currency Symbol/Policy ID" $ currencySymbolOf happyToken == mintingContractCS

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
    ,   traceIfFalse "Wrong Amount Minted By First DeveloperKKKKKKKKKKK"     $ assetClassValueOf txInfoMint happyToken == firstDevPhaseOneClaimAmount
    ,   traceIfFalse "Only 1 Token Burn/Mint Action Is Allowed"   $ length (flattenValue txInfoMint) == 1
                    {-|---------------------------------Signatories--------------------------------------|-}
    ,   traceIfFalse "First Developer Must Sign Tx"               $ txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)
                    {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   traceIfFalse "Phase One Date Has Not Been Reached"        $ from dateOfPhaseOne `contains` txInfoValidRange
                    {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   traceIfFalse "Phase One Tokens Were Minted By First Dev"  $ not firstDevDidPhaseOne

    ,   traceIfFalse "happyToken Mismatch"                        $ happyToken == happyToken'

    ,   traceIfFalse "firstDevPhaseOneClaimAmount Mismatched"     $ firstDevPhaseOneClaimAmount  ==  firstDevPhaseOneClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseOne Mismatched"             $ not firstDevDidPhaseOne      ==  firstDevDidPhaseOne'
    ,   traceIfFalse "secondDevPhaseOneClaimAmount Mismatched"    $ secondDevPhaseOneClaimAmount ==  secondDevPhaseOneClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseOne Mismatched"            $ secondDevDidPhaseOne         ==  secondDevDidPhaseOne'
    ,   traceIfFalse "dateOfPhaseOne Mismatched"                  $ dateOfPhaseOne               ==  dateOfPhaseOne'

    ,   traceIfFalse "phaseTwoInfo Mismatched"                    $ phaseTwoInfo == phaseTwoInfo'
                    {-|----------------------------------Input/Output------------------------------------|-}
    ,   traceIfFalse "SC Output UTxO Has No Happy State Token"    $ assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   traceIfFalse "SC Input UTxO Has No Happy State Token"     $ assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   traceIfFalse "Script Input & Output Must Be Equal"        $ txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,   traceIfFalse "Incorrect Tx In/Out"                        $     isNumOfInputsHasDatum txInfoInputs 1
                                                                    &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                       SECOND DEVELOPER CLAIMS AT PHASE ONE                       |--
    -----------------------------------------------------------------------------------------
    |   (PhaseOneClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} phaseTwoInfo ) <- (redeemer , datum)

    ,    traceIfFalse "Wrong Minting Contract Currency Symbol/Policy ID" $ currencySymbolOf happyToken == mintingContractCS

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
    ,   traceIfFalse "Wrong Amount Minted By Second Developer"      $ assetClassValueOf txInfoMint happyToken == secondDevPhaseOneClaimAmount
    ,   traceIfFalse "Only 1 Token Burn/Mint Action Is Allowed"     $ length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   traceIfFalse "Second Developer Must Sign Tx"                $ txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)

                {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   traceIfFalse "Phase One Date Has Not Been Reached"          $ from dateOfPhaseOne `contains` txInfoValidRange

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   traceIfFalse "Phase One Tokens Were Minted By Second Dev"   $ not secondDevDidPhaseOne

    ,   traceIfFalse "happyToken Mismatch"                          $ happyToken == happyToken'

    ,   traceIfFalse "firstDevPhaseOneClaimAmount Mismatched"       $ firstDevPhaseOneClaimAmount  ==  firstDevPhaseOneClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseOne Mismatched"               $ firstDevDidPhaseOne          ==  firstDevDidPhaseOne'
    ,   traceIfFalse "secondDevPhaseOneClaimAmount Mismatched"      $ secondDevPhaseOneClaimAmount ==  secondDevPhaseOneClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseOne Mismatched"              $ not secondDevDidPhaseOne     ==  secondDevDidPhaseOne'
    ,   traceIfFalse "dateOfPhaseOne Mismatched"                    $ dateOfPhaseOne               ==  dateOfPhaseOne'

    ,   traceIfFalse "phaseTwoInfo Mismatched"                      $ phaseTwoInfo == phaseTwoInfo'

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   traceIfFalse "SC Output UTxO Has No Happy State Token"      $ assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   traceIfFalse "SC Input UTxO Has No Happy State Token"       $ assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   traceIfFalse "Script Input & Output Must Be Equal"          $ txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,   traceIfFalse "Incorrect Tx In/Out"                          $       isNumOfInputsHasDatum txInfoInputs 1
                                                                        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                       FIRST DEVELOPER CLAIMS AT PHASE TWO                        |--
    -----------------------------------------------------------------------------------------
    |   (PhaseTwoClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   traceIfFalse "Wrong Minting Contract Currency Symbol/Policy ID" $ currencySymbolOf happyToken == mintingContractCS

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
    ,   traceIfFalse "Wrong Amount Minted By First Developer"       $ if firstDevDidPhaseOne
                                                                            then assetClassValueOf txInfoMint happyToken == firstDevPhaseTwoClaimAmount
                                                                            else assetClassValueOf txInfoMint happyToken == firstDevPhaseTwoClaimAmount + firstDevPhaseOneClaimAmount
    ,   traceIfFalse "Only 1 Token Burn/Mint Action Is Allowed"     $ length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   traceIfFalse "First Developer Must Sign Tx"                 $ txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)

                {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   traceIfFalse "Phase Two Date Has Not Been Reached"          $ from dateOfPhaseTwo `contains` txInfoValidRange

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   traceIfFalse "Phase Two Tokens Were Minted By First Dev"    $ not firstDevDidPhaseTwo

    ,   traceIfFalse "happyToken Mismatch"                          $ happyToken == happyToken'

    ,   traceIfFalse "firstDevPhaseOneClaimAmount Mismatched"       $ firstDevPhaseOneClaimAmount  ==  firstDevPhaseOneClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseOne Mismatched"               $ if firstDevDidPhaseOne
                                                                            then firstDevDidPhaseOne        ==  firstDevDidPhaseOne'
                                                                            else not firstDevDidPhaseOne    ==  firstDevDidPhaseOne'
    ,   traceIfFalse "secondDevPhaseOneClaimAmount Mismatched"      $ secondDevPhaseOneClaimAmount ==  secondDevPhaseOneClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseOne Mismatched"              $ secondDevDidPhaseOne         ==  secondDevDidPhaseOne'
    ,   traceIfFalse "dateOfPhaseOne Mismatched"                    $ dateOfPhaseOne               ==  dateOfPhaseOne'

    ,   traceIfFalse "firstDevPhaseTwoClaimAmount Mismatched"       $ firstDevPhaseTwoClaimAmount  ==  firstDevPhaseTwoClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseTwo Mismatched"               $ not firstDevDidPhaseTwo      ==  firstDevDidPhaseTwo'
    ,   traceIfFalse "secondDevPhaseTwoClaimAmount Mismatched"      $ secondDevPhaseTwoClaimAmount ==  secondDevPhaseTwoClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseTwo Mismatched"              $ secondDevDidPhaseTwo         ==  secondDevDidPhaseTwo'
    ,   traceIfFalse "dateOfPhaseTwo Mismatched"                    $ dateOfPhaseTwo               ==  dateOfPhaseTwo'

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   traceIfFalse "SC Output UTxO Has No Happy State Token"      $ assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   traceIfFalse "SC Input UTxO Has No Happy State Token"       $ assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   traceIfFalse "Script Input & Output Must Be Equal"          $ txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,   traceIfFalse "Incorrect Tx In/Out"                          $       isNumOfInputsHasDatum txInfoInputs 1
                                                                        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                       SECOND DEVELOPER CLAIMS AT PHASE TWO                       |--
    -----------------------------------------------------------------------------------------
    |   (PhaseTwoClaimingToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   traceIfFalse "Wrong Minting Contract Currency Symbol/Policy ID" $ currencySymbolOf happyToken == mintingContractCS

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
    ,   traceIfFalse "Wrong Amount Minted By Second Developer"      $ if secondDevDidPhaseOne
                                                                        then assetClassValueOf txInfoMint happyToken == secondDevPhaseTwoClaimAmount
                                                                        else assetClassValueOf txInfoMint happyToken == secondDevPhaseTwoClaimAmount + secondDevPhaseOneClaimAmount
    ,   traceIfFalse "Only 1 Token Burn/Mint Action Is Allowed"     $ length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   traceIfFalse "Second Developer Must Sign Tx"                $ txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)

                {-|---------------------------------Time Handling------------------------------------|-}
    -- ,   traceIfFalse "Phase Two Date Has Not Been Reached"          $ from dateOfPhaseTwo `contains` txInfoValidRange

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   traceIfFalse "Phase Two Tokens Were Minted By Second Dev"   $ not secondDevDidPhaseTwo

    ,   traceIfFalse "happyToken Mismatch"                          $ happyToken == happyToken'

    ,   traceIfFalse "firstDevPhaseOneClaimAmount Mismatched"       $ firstDevPhaseOneClaimAmount   ==  firstDevPhaseOneClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseOne Mismatched"               $ firstDevDidPhaseOne           ==  firstDevDidPhaseOne'
    ,   traceIfFalse "secondDevPhaseOneClaimAmount Mismatched"      $ secondDevPhaseOneClaimAmount  ==  secondDevPhaseOneClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseOne Mismatched"              $ if secondDevDidPhaseOne
                                                                        then secondDevDidPhaseOne       ==  secondDevDidPhaseOne'
                                                                        else not secondDevDidPhaseOne   ==  secondDevDidPhaseOne'
    ,   traceIfFalse "dateOfPhaseOne Mismatched"                    $ dateOfPhaseOne                ==  dateOfPhaseOne'

    ,   traceIfFalse "firstDevPhaseTwoClaimAmount Mismatched"       $ firstDevPhaseTwoClaimAmount   ==  firstDevPhaseTwoClaimAmount'
    ,   traceIfFalse "firstDevDidPhaseTwo Mismatched"               $ firstDevDidPhaseTwo           ==  firstDevDidPhaseTwo'
    ,   traceIfFalse "secondDevPhaseTwoClaimAmount Mismatched"      $ secondDevPhaseTwoClaimAmount  ==  secondDevPhaseTwoClaimAmount'
    ,   traceIfFalse "secondDevDidPhaseTwo Mismatched"              $ not secondDevDidPhaseTwo      ==  secondDevDidPhaseTwo'
    ,   traceIfFalse "dateOfPhaseTwo Mismatched"                    $ dateOfPhaseTwo                ==  dateOfPhaseTwo'

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   traceIfFalse "SC Output UTxO Has No Happy State Token"      $ assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   traceIfFalse "SC Input UTxO Has No Happy State Token"       $ assetClassValueOf (txOutValue singleScriptInputUTxO) happyToken == 1
    ,   traceIfFalse "Script Input & Output Must Be Equal"          $ txOutValue singleScriptInputUTxO == txOutValue singleScriptOutputUTxO
    ,   traceIfFalse "Incorrect Tx In/Out"                          $       isNumOfInputsHasDatum txInfoInputs 1
                                                                        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 1
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                        FIRST DEVELOPER BURN GENESIS TOKEN                        |--
    -----------------------------------------------------------------------------------------

    |   (BurningGenesisToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   FirstDeveloperAction firstDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   traceIfFalse "Wrong Amount Minted By First Developer"       $ assetClassValueOf txInfoMint happyToken == -1
    ,   traceIfFalse "Only 1 Token Burn/Mint Action Is Allowed"     $ length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   traceIfFalse "First Developer Must Sign Tx"                 $ txSignedBy ctxTxInfo (getPaymentPKH firstDevAddress)

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   traceIfFalse "One Or More Dev Has Not Claim Their Token"    $       firstDevDidPhaseOne
                                                                        &&  secondDevDidPhaseOne
                                                                        &&  firstDevDidPhaseTwo
                                                                        &&  secondDevDidPhaseTwo

                {-|----------------------------------Input/Output------------------------------------|-}
    ,   traceIfFalse "SC Output UTxO Has No Happy State Token"      $ assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   traceIfFalse "Incorrect Tx In/Out"                          $       isNumOfInputsHasDatum txInfoInputs 1
                                                                        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 0
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                        SECOND DEVELOPER BURN GENESIS TOKEN                       |--
    -----------------------------------------------------------------------------------------
    |   (BurningGenesisToken mintingContractCS
            , DistroDatum happyToken PhaseOneInfo{..} PhaseTwoInfo{..} ) <- (redeemer , datum)

    ,   SecondDeveloperAction secondDevAddress <- getMintingRedeemer mintingContractCS

                {-|--------------------------------Minting Handling----------------------------------|-}
    ,   traceIfFalse "Wrong Amount Minted By Second Developer"      $ assetClassValueOf txInfoMint happyToken == -1
    ,   traceIfFalse "Only 1 Token Burn/Mint Action Is Allowed"     $ length (flattenValue txInfoMint) == 1

                {-|---------------------------------Signatories--------------------------------------|-}
    ,   traceIfFalse "Second Developer Must Sign Tx"                $ txSignedBy ctxTxInfo (getPaymentPKH secondDevAddress)

                {-|---------------------------------Datum Handling-----------------------------------|-}
    ,   traceIfFalse "One Or More Dev Has Not Claim Their Token"    $       firstDevDidPhaseOne
                                                                        &&  secondDevDidPhaseOne
                                                                        &&  firstDevDidPhaseTwo
                                                                        &&  secondDevDidPhaseTwo
                {-|----------------------------------Input/Output------------------------------------|-}
    ,   traceIfFalse "SC Output UTxO Has No Happy State Token"      $ assetClassValueOf (txOutValue singleScriptOutputUTxO) happyToken == 1
    ,   traceIfFalse "Incorrect Tx In/Out"                          $       isNumOfInputsHasDatum txInfoInputs 1
                                                                        &&  isNumOutputsHasDatum (getContinuingOutputs ctx) 0
    =   ()

    -----------------------------------------------------------------------------------------
    -- |                             DISTRO DEBUGGER ACTION                               |--
    -----------------------------------------------------------------------------------------

    |   ( DistroDebuggerAction
            , DistroDatum {}
            ) <- (redeemer , datum)

    ,   traceIfFalse "Distro Debugger Must Sign"  $ txSignedBy ctxTxInfo distroDebuggerPKH

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
                    |   otherwise   = traceError "No Redeemer Found"

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

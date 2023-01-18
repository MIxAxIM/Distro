{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module  Regulator.ToJSON
                (       main
                ,       genesisMintActionRedeemer
                ,       firstDeveloperActionRedeemer
                ,       secondDeveloperActionRedeemer
                ,       regulatorDebuggerActionRedeemer
                ,       happyTokenName'
                ,       firstDevPhaseOneClaimAmount'
                ,       firstDevDidPhaseOne'
                ,       secondDevPhaseOneClaimAmount'
                ,       secondDevDidPhaseOne'
                ,       dateOfPhaseOne'
                ,       firstDevPhaseTwoClaimAmount''
                ,       firstDevDidPhaseTwo''
                ,       secondDevPhaseTwoClaimAmount''
                ,       secondDevDidPhaseTwo''
                ,       dateOfPhaseTwo''
                ,       firstDevAddress'
                ,       secondDevAddress'
                ,       happyToken'
                ,       happyTokenCurrencySymbol'
                ) where

import           Cardano.Api                 (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                              scriptDataToJson)
import           Cardano.Api.Shelley         (fromPlutusData)
import           Data.Aeson                  (encode)
import           Data.ByteString.Lazy        (writeFile)
import           Plutus.V1.Ledger.Address    (Address (..))
import           Plutus.V1.Ledger.Credential (Credential (PubKeyCredential),
                                              StakingCredential (StakingHash))
import           Plutus.V1.Ledger.Time       (POSIXTime (..))
import           Plutus.V1.Ledger.Value      (AssetClass, CurrencySymbol,
                                              TokenName, assetClass)
import           PlutusTx                    (ToData, toData)
import           PlutusTx.Prelude            (Bool (False, True), Integer,
                                              Maybe (Just), ($), (*), (+), (.),
                                              (<>))
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath.Posix       ((<.>), (</>))

import qualified Prelude                     as Haskell

import           Distro.DataTypes            (DistroDatum (..),
                                              PhaseOneInfo (..),
                                              PhaseTwoInfo (..))
import           Regulator.DataTypes         (RegulatorAction (..))


{-----------------------HAPPY TOKEN INFORMATION---------------------}

happyTokenName' :: TokenName
happyTokenName' = "HAPPY_TOKEN"

happyTokenCurrencySymbol' :: CurrencySymbol
happyTokenCurrencySymbol' = "520ade25e2dc0e3334a99821c5740de45dd8de4810c0c6d2d322430f"

happyToken' :: AssetClass
happyToken' = assetClass happyTokenCurrencySymbol' happyTokenName'

{-------------------------------------------------------------------}

{----------------------FIRST DEVELOPER INFORMATION------------------}

firstDevPKH :: Credential
firstDevPKH = PubKeyCredential "cbbfc3e193813ee338d40eb0f4f178d46245e0d87adc4e9e8e0be489"

firstDevStakePKH :: Maybe StakingCredential
firstDevStakePKH = Just $ StakingHash $ PubKeyCredential "6683a4a58711b3c82a4daea1b607b3642f00164f3f28d14edc0f5031"

firstDevAddress' :: Address
firstDevAddress' = Address firstDevPKH firstDevStakePKH

{-------------------------------------------------------------------}

{-------------------SECOND DEVELOPER INFORMATION--------------------}

secondDevPKH :: Credential
secondDevPKH = PubKeyCredential "5e822f882ae15a6598f04fdeac0e85423793c7d65164e36f3edbc48e"

secondDevStakePKH :: Maybe StakingCredential
secondDevStakePKH = Just $ StakingHash $ PubKeyCredential "4eed7b2060846bbccb19b08caa58ced3ad4b005daca450b5c147f97d"

secondDevAddress' :: Address
secondDevAddress' = Address secondDevPKH secondDevStakePKH

{-------------------------------------------------------------------}

{------------------------PHASE ONE INFORMATION----------------------}

firstDevPhaseOneClaimAmount' :: Integer
firstDevPhaseOneClaimAmount' =  10000

firstDevDidPhaseOne' :: Bool
firstDevDidPhaseOne' =  False

secondDevPhaseOneClaimAmount' :: Integer
secondDevPhaseOneClaimAmount' =  20000

secondDevDidPhaseOne' :: Bool
secondDevDidPhaseOne' =  False

dateOfPhaseOne' :: POSIXTime
dateOfPhaseOne' =  1673444643000

{-------------------------------------------------------------------}

{------------------------PHASE TWO INFORMATION----------------------}

firstDevPhaseTwoClaimAmount'' :: Integer
firstDevPhaseTwoClaimAmount'' =  90000

firstDevDidPhaseTwo'' :: Bool
firstDevDidPhaseTwo'' =  False

secondDevPhaseTwoClaimAmount'' :: Integer
secondDevPhaseTwoClaimAmount'' =  180000

secondDevDidPhaseTwo'' :: Bool
secondDevDidPhaseTwo'' =  False

dateOfPhaseTwo'' :: POSIXTime
dateOfPhaseTwo'' = 1673445243000

{-------------------------------------------------------------------}

{------------------------------DATUMS-------------------------------}

genesisMintInlineDatum :: DistroDatum
genesisMintInlineDatum = DistroDatum
                                happyToken'
                                (PhaseOneInfo
                                        firstDevPhaseOneClaimAmount'
                                        firstDevDidPhaseOne'
                                        secondDevPhaseOneClaimAmount'
                                        secondDevDidPhaseOne'
                                        dateOfPhaseOne')
                                (PhaseTwoInfo
                                        firstDevPhaseTwoClaimAmount''
                                        firstDevDidPhaseTwo''
                                        secondDevPhaseTwoClaimAmount''
                                        secondDevDidPhaseTwo''
                                        dateOfPhaseTwo'')

{-------------------------------------------------------------------}

{-----------------------------REDEEMERS-----------------------------}

genesisMintActionRedeemer :: RegulatorAction
genesisMintActionRedeemer = GenesisMint

firstDeveloperActionRedeemer :: RegulatorAction
firstDeveloperActionRedeemer = FirstDeveloperAction firstDevAddress'

secondDeveloperActionRedeemer :: RegulatorAction
secondDeveloperActionRedeemer = SecondDeveloperAction secondDevAddress'

regulatorDebuggerActionRedeemer :: RegulatorAction
regulatorDebuggerActionRedeemer = RegulatorDebuggerAction

{-------------------------------------------------------------------}

writeJSON :: ToData a => Haskell.FilePath -> a -> Haskell.IO ()
writeJSON file = writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . toData

main :: Haskell.IO ()
main = do
        let     datDir          = "dist/data/datums"
                redDir          = "dist/data/redeemers"
                contractName    = "Regulator"
        createDirectoryIfMissing True $ datDir </> contractName
        createDirectoryIfMissing True $ redDir </> contractName

        writeJSON (datDir </> contractName </> "genesis_mint_inline_datum"      <.> "json") genesisMintInlineDatum

        writeJSON (redDir </> contractName </> "genesis_mint_action_redeemer"           <.> "json") genesisMintActionRedeemer
        writeJSON (redDir </> contractName </> "first_developer_action_redeemer"        <.> "json") firstDeveloperActionRedeemer
        writeJSON (redDir </> contractName </> "second_developer_action_redeemer"       <.> "json") secondDeveloperActionRedeemer
        writeJSON (redDir </> contractName </> "regulator_debugger_action_redeemer"     <.> "json") regulatorDebuggerActionRedeemer

        Haskell.putStrLn        "\n<--------------------------------------DONE------------------------------------------>"
        Haskell.putStrLn        "   ATTENTION:"
        Haskell.putStrLn    $   "       Datum location:     " <> Haskell.show (datDir </> contractName)
        Haskell.putStrLn    $   "       Redeemers location: " <> Haskell.show (redDir </> contractName)
        Haskell.putStrLn        "<------------------------------------------------------------------------------------>\n"

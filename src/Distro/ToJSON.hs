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

module  Distro.ToJSON
                (       main
                ,       firstDevClaimAtPhaseOneDatum
                ,       firstDevClaimAtPhaseTwoDatum
                ,       phaseOneClaimingTokenRedeemer
                ,       phaseTwoClaimingTokenRedeemer
                ,       burningGenesisTokenRedeemer
                ,       distroDebuggerActionRedeemer
                )
                        where

import           Cardano.Api           (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                        scriptDataToJson)
import           Cardano.Api.Shelley   (fromPlutusData)
import           Data.Aeson            (encode)
import           Data.ByteString.Lazy  (writeFile)
import           PlutusTx              (ToData, toData)
import           PlutusTx.Prelude      (Bool (False, True), ($), (.), (<>))
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath.Posix ((<.>), (</>))

import qualified Prelude               as Haskell

import           Distro.DataTypes      (DistroAction (..), DistroDatum (..),
                                        PhaseOneInfo (..), PhaseTwoInfo (..))
import           Regulator.ToJSON      (dateOfPhaseOne', dateOfPhaseTwo'',
                                        firstDevPhaseOneClaimAmount',
                                        firstDevPhaseTwoClaimAmount'',
                                        happyToken', happyTokenCurrencySymbol',
                                        secondDevDidPhaseOne',
                                        secondDevDidPhaseTwo'',
                                        secondDevPhaseOneClaimAmount',
                                        secondDevPhaseTwoClaimAmount'')


{---------------FIRST DEVELOPER CLAIM AT PHASE ONE ------------------}

firstDevDidPhaseOne' :: Bool
firstDevDidPhaseOne' =  True

firstDevDidPhaseTwo' :: Bool
firstDevDidPhaseTwo' =  False

{-------------------------------------------------------------------}

{---------------FIRST DEVELOPER CLAIM AT PHASE TWO -----------------}

firstDevDidPhaseOne'' :: Bool
firstDevDidPhaseOne'' =  True

firstDevDidPhaseTwo'' :: Bool
firstDevDidPhaseTwo'' =  True

{-------------------------------------------------------------------}

{------------------------------DATUMS-------------------------------}

firstDevClaimAtPhaseOneDatum :: DistroDatum
firstDevClaimAtPhaseOneDatum = DistroDatum
                                happyToken'
                                (PhaseOneInfo
                                        firstDevPhaseOneClaimAmount'

                                        firstDevDidPhaseOne'

                                        secondDevPhaseOneClaimAmount'
                                        secondDevDidPhaseOne'
                                        dateOfPhaseOne')
                                (PhaseTwoInfo
                                        firstDevPhaseTwoClaimAmount''

                                        firstDevDidPhaseTwo'

                                        secondDevPhaseTwoClaimAmount''
                                        secondDevDidPhaseTwo''
                                        dateOfPhaseTwo'')

firstDevClaimAtPhaseTwoDatum :: DistroDatum
firstDevClaimAtPhaseTwoDatum = DistroDatum
                                happyToken'
                                (PhaseOneInfo
                                        firstDevPhaseOneClaimAmount'

                                        firstDevDidPhaseOne''

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

phaseOneClaimingTokenRedeemer :: DistroAction
phaseOneClaimingTokenRedeemer = PhaseOneClaimingToken happyTokenCurrencySymbol'

phaseTwoClaimingTokenRedeemer :: DistroAction
phaseTwoClaimingTokenRedeemer = PhaseTwoClaimingToken happyTokenCurrencySymbol'

burningGenesisTokenRedeemer :: DistroAction
burningGenesisTokenRedeemer = BurningGenesisToken happyTokenCurrencySymbol'

distroDebuggerActionRedeemer :: DistroAction
distroDebuggerActionRedeemer = DistroDebuggerAction

{-------------------------------------------------------------------}


writeJSON :: ToData a => Haskell.FilePath -> a -> Haskell.IO ()
writeJSON file = writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . toData

main :: Haskell.IO ()
main = do
        let datDir          = "dist/data/datums"
            redDir          = "dist/data/redeemers"
            contractName    = "Distro"
        createDirectoryIfMissing True $ datDir </> contractName
        createDirectoryIfMissing True $ redDir </> contractName

        writeJSON (datDir </> contractName </> "first_dev_claim_at_phase_one_datum"     <.> "json") firstDevClaimAtPhaseOneDatum
        writeJSON (datDir </> contractName </> "first_dev_claim_at_phase_two_datum"     <.> "json") firstDevClaimAtPhaseTwoDatum

        writeJSON (redDir </> contractName </> "phase_one_claiming_token_redeemer"      <.> "json") phaseOneClaimingTokenRedeemer
        writeJSON (redDir </> contractName </> "phase_two_claiming_token_redeemer"      <.> "json") phaseTwoClaimingTokenRedeemer
        writeJSON (redDir </> contractName </> "burning_genesis_token_redeemer"         <.> "json") burningGenesisTokenRedeemer
        writeJSON (redDir </> contractName </> "distro_debugger_action_redeemer"        <.> "json") distroDebuggerActionRedeemer

        Haskell.putStrLn        "\n<--------------------------------------DONE------------------------------------------>"
        Haskell.putStrLn        "   ATTENTION:"
        Haskell.putStrLn    $   "       Datums location:    " <> Haskell.show ( datDir </> contractName)
        Haskell.putStrLn    $   "       Redeemers location: " <> Haskell.show (redDir </> contractName)
        Haskell.putStrLn        "<------------------------------------------------------------------------------------>\n"

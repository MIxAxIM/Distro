{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Regulator.Compiler
    (   regulatorMintingPolicy
    ,   contractParams
    ,   writeScript
    )
        where

{-------------------------DUMP PLUTUS CORE------------------------}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-plc #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-pir #-}

{--------------------REMOVE ALL ERROR MESSAGES--------------------}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-context #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

{------------------------------DEBUG-------------------------------}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:coverage-all=True #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:coverage-boolean=True #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:coverage-location=True #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug=True #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context=3 #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:verbose=True #-}

import           Cardano.Api.SerialiseTextEnvelope      (TextEnvelopeDescr,
                                                         textEnvelopeToJSON,
                                                         writeFileTextEnvelope)
import           Cardano.Api.Shelley                    (FileError,
                                                         PlutusScript (PlutusScriptSerialised),
                                                         PlutusScriptV2,
                                                         displayError)
import           Codec.Serialise                        (serialise)
import           Data.ByteString.Lazy                   (ByteString, toStrict)
import qualified Data.ByteString.Lazy                   as LBS
import           Data.ByteString.Short                  (ShortByteString,
                                                         length, toShort)
import           Data.List                              (break, intercalate,
                                                         reverse)
import           Data.List.Split                        (chunksOf)
import           Plutus.ApiCommon                       (ProtocolVersion (..))
import           Plutus.Script.Utils.V2.Scripts         (scriptHash,
                                                         validatorHash)
import           Plutus.Script.Utils.V2.Typed.Scripts   (mkUntypedMintingPolicy,
                                                         validatorScript)
import           Plutus.V1.Ledger.Scripts               (MintingPolicy, Script,
                                                         mkMintingPolicyScript,
                                                         scriptSize,
                                                         unMintingPolicyScript,
                                                         unScript)
import           Plutus.V2.Ledger.Api                   (ExBudget (..),
                                                         ExCPU (..),
                                                         ExMemory (..),
                                                         VerboseMode (Verbose),
                                                         evaluateScriptCounting,
                                                         exBudgetCPU,
                                                         exBudgetMemory,
                                                         mkEvaluationContext,
                                                         toData)
import           Plutus.V2.Ledger.Contexts              (ScriptContext)
import           PlutusCore                             (DefaultFun, DefaultUni,
                                                         defaultCostModelParams)
import           PlutusCore.Evaluation.Machine.ExMemory (CostingInteger)
import           PlutusCore.Pretty                      (Doc,
                                                         prettyPlcReadableDef)
import           PlutusTx                               (BuiltinData, Data,
                                                         applyCode, compile,
                                                         liftCode)
import           PlutusTx.Code                          (getPlc, sizePlc)
import           Prelude                                (Bool (True), Char,
                                                         Either (Left, Right),
                                                         FilePath, IO, Integer,
                                                         Maybe (Just), Show,
                                                         error, fst, otherwise,
                                                         print, putStrLn, show,
                                                         snd, writeFile, ($),
                                                         (++), (.), (<>), (==))
import           System.Directory                       (createDirectoryIfMissing)
import           System.FilePath.Posix                  ((<.>), (</>))
import           UntypedPlutusCore                      (DeBruijn,
                                                         NamedDeBruijn, Program)

import qualified Plutonomy

{-================================================================================================================================-}
{-==========                                             CONTRACT SECTION                                               ==========-}
{-================================================================================================================================-}

------------------------------------------------------------------------------------------
-- |                               CONTRACT IMPORTS                                   | --
------------------------------------------------------------------------------------------

import qualified Distro.Compiler                        as Distro
import           Distro.DataTypes                       (PhaseOneInfo (..),
                                                         PhaseTwoInfo (..))
import           Regulator.Contract                     (regulatorMintingPolicy)
import           Regulator.DataTypes                    (DevTeamAddresses (..),
                                                         RegulatorAction,
                                                         RegulatorParams (..))
import           Regulator.ToJSON                       (dateOfPhaseOne',
                                                         dateOfPhaseTwo'',
                                                         firstDevAddress',
                                                         firstDevDidPhaseOne',
                                                         firstDevDidPhaseTwo'',
                                                         firstDevPhaseOneClaimAmount',
                                                         firstDevPhaseTwoClaimAmount'',
                                                         firstDeveloperActionRedeemer,
                                                         happyTokenName',
                                                         secondDevAddress',
                                                         secondDevDidPhaseOne',
                                                         secondDevDidPhaseTwo'',
                                                         secondDevPhaseOneClaimAmount',
                                                         secondDevPhaseTwoClaimAmount'')

------------------------------------------------------------------------------------------
-- |                    TYPED COMPILATION & NO OPTIMIZATION                           | --
------------------------------------------------------------------------------------------

mkRegulatorMintingPolicy :: RegulatorParams -> MintingPolicy
mkRegulatorMintingPolicy regulatorParams = Plutonomy.optimizeUPLC
                                                $   mkMintingPolicyScript
                                                    $   $$(compile [|| mkUntypedMintingPolicy . regulatorMintingPolicy ||])
                                                        `applyCode` liftCode regulatorParams

-- mkRegulatorMintingPolicy :: RegulatorParams -> MintingPolicy
-- mkRegulatorMintingPolicy regulatorParams = Plutonomy.optimizeUPLCWith
--                                                 Plutonomy.aggressiveOptimizerOptions
--                                                 $   mkMintingPolicyScript
--                                                     $   $$(compile [|| mkUntypedMintingPolicy . regulatorMintingPolicy ||])
--                                                         `applyCode` liftCode regulatorParams

------------------------------------------------------------------------------------------
-- |                     UNTYPED COMPILATION & OPTIMIZATION                           | --
------------------------------------------------------------------------------------------

-- mkRegulatorMintingPolicy :: RegulatorParams -> MintingPolicy
-- mkRegulatorMintingPolicy regulatorParams = Plutonomy.optimizeUPLC
--                                             $   mkMintingPolicyScript
--                                                 $   $$(compile [|| regulatorMintingPolicy ||])
--                                                     `applyCode` liftCode regulatorParams

-- mkRegulatorMintingPolicy :: RegulatorParams -> MintingPolicy
-- mkRegulatorMintingPolicy regulatorParams = Plutonomy.optimizeUPLCWith
--                                                 Plutonomy.aggressiveOptimizerOptions
--                                                 $   mkMintingPolicyScript
--                                                     $   $$(compile [|| regulatorMintingPolicy ||])
--                                                         `applyCode` liftCode regulatorParams

------------------------------------------------------------------------------------------
-- |                               CONTRACT DETAILS                                   | --
------------------------------------------------------------------------------------------

type ContractParams = RegulatorParams

contractParams :: ContractParams
contractParams = RegulatorParams

{--------------COMMENT IT OUT FOR DISTRO TYPED VALIDATION-----------------}
    {   distroContractVH        =   validatorHash $ validatorScript $ Distro.mkDistroValidator Distro.contractParams

{-------------COMMENT IT OUT FOR DISTRO UNTYPED VALIDATION----------------}
    -- {   distroContractVH        =   validatorHash $ Distro.mkDistroValidator Distro.contractParams

    ,   happyTokenName          =   happyTokenName'
    ,   phaseOneInfo            =   PhaseOneInfo
                                        {   firstDevPhaseOneClaimAmount     =  firstDevPhaseOneClaimAmount'
                                        ,   secondDevPhaseOneClaimAmount    =  secondDevPhaseOneClaimAmount'
                                        ,   firstDevDidPhaseOne             =  firstDevDidPhaseOne'
                                        ,   secondDevDidPhaseOne            =  secondDevDidPhaseOne'
                                        ,   dateOfPhaseOne                  =  dateOfPhaseOne'
                                        }
    ,   phaseTwoInfo            =   PhaseTwoInfo
                                        {   firstDevPhaseTwoClaimAmount     =  firstDevPhaseTwoClaimAmount''
                                        ,   firstDevDidPhaseTwo             =  firstDevDidPhaseTwo''
                                        ,   secondDevPhaseTwoClaimAmount    =  secondDevPhaseTwoClaimAmount''
                                        ,   secondDevDidPhaseTwo            =  secondDevDidPhaseTwo''
                                        ,   dateOfPhaseTwo                  =  dateOfPhaseTwo''
                                        }
    ,   devTeamAddresses        =   DevTeamAddresses
                                        {   firstDevAddress                 =   firstDevAddress'
                                        ,   secondDevAddress                =   secondDevAddress'
                                        }
    ,   regulatorDebuggerPKH    =   "bdc24850053ddf1e0c53bf20a518dcc4dcc5200ac4adb0056a1ded5c"
    }

contractName :: [Char]
contractName = "Regulator"

-- {----------------COMMENT IT OUT FOR REGULATOR UNTYPED VALIDATION--------------}

-- contractValidator
--     :: RegulatorParams
--     -> BuiltinData
--     -> BuiltinData
--     -> ()
-- contractValidator = regulatorMintingPolicy

-- contractMKScript :: Script
-- contractMKScript = unMintingPolicyScript $ mkRegulatorMintingPolicy contractParams

-- {-----------------------------------------------------------------------------}

-- {-----------------COMMENT IT OUT FOR REGULATOR TYPED VALIDATION---------------}

contractValidator
    :: RegulatorParams
    -> RegulatorAction
    -> ScriptContext
    -> Bool
contractValidator = regulatorMintingPolicy

contractMKScript :: Script
contractMKScript = unMintingPolicyScript $ mkRegulatorMintingPolicy contractParams

{--------------------------------------------------------------------------------}

contractDescription :: TextEnvelopeDescr
contractDescription  = "This is minter contract which regulate minting and burning Happy Token for dev team"

------------------------------------------------------------------------------------------
-- |                               CONTRACT ExBUDGET                                  | --
------------------------------------------------------------------------------------------

red :: RegulatorAction
red = firstDeveloperActionRedeemer

dataToCalculate :: [Data]
dataToCalculate = [toData red]

{-================================================== END OF CONTRACT SECTION =====================================================-}


{-================================================================================================================================-}
{-==========                                            GENERATOR SECTION                                               ==========-}
{-================================================================================================================================-}

writeCBORValidator :: FilePath -> Script -> IO (Either (FileError ()) ())
writeCBORValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file (Just contractDescription) . PlutusScriptSerialised . toShort . toStrict . serialise

writeOnlyJSON ::  Script -> ByteString
writeOnlyJSON = textEnvelopeToJSON @(PlutusScript PlutusScriptV2) (Just contractDescription) . PlutusScriptSerialised . toShort . toStrict . serialise

writeScript  :: IO ()
writeScript  = do
    let conDir          = "dist/build/contracts"
        coresDir        = "dist/data/core"
    createDirectoryIfMissing True conDir
    createDirectoryIfMissing True $ coresDir </> contractName
    result <- writeCBORValidator (conDir </> contractName <.> ".plutus") contractMKScript
    case result of
        Left err -> print $ displayError err
        Right () -> do
                        writeFile  (coresDir </> contractName </> "prettifiedTypedPlutusCore" <.> "txt") (show $ prettyPLC contractParams)
                        writeFile  (coresDir </> contractName </> "rawTypePlutusCore" <.> "txt") (show $ plc contractParams)
                        writeFile  (coresDir </> contractName </> "prettifiedUntypedPlutusCore" <.> "txt") (show prettyUPLC)
                        writeFile  (coresDir </> contractName </> "rawUntypedPlutusCore" <.> "txt") (show uplc)

                        putStrLn        "\n<----------------------------------------CONTRACT INFO----------------------------------------->\n"
                        putStrLn    $   "       Name:                       "   ++  contractName
                        putStrLn    $   "       Location:                   "   ++  conDir      </> contractName
                        putStrLn    $   "       Cores Location:             "   ++  coresDir    </> contractName
                        putStrLn    $   "       PLC Size (Bytes):           "   <>  show (format $  plcSize contractParams)
                        putStrLn    $   "       CBOR Size (Bytes):          "   <>  show (format $  LBS.length $ writeOnlyJSON contractMKScript)
                        putStrLn    $   "       CBOR Binary Size (Bytes):   "   <>  show (format $  length plutusCore)
                        putStrLn    $   "       Plutus Binary Size (Bytes): "   <>  show (format $  scriptSize contractMKScript)
                        putStrLn    $   "       CPU Usage (Step/PSec):      "   <>  show (format    getCPUExBudget)
                        putStrLn    $   "       Memory Usage (Bytes):       "   <>  show (format    getMemoryExBudget)
                        putStrLn    $   "       Script Hash:                "   <>  show (scriptHash contractMKScript)
                        putStrLn        "\n<----------------------------------------------------------------------------------------------->\n"

    where

        format :: Show a => a -> [Char]
        format x = h++t
            where
                sp = break (== '.') $ show x
                h = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
                t = snd sp

        plutusCore :: ShortByteString
        plutusCore = toShort $ toStrict $ serialise contractMKScript

        uplc :: Program DeBruijn DefaultUni DefaultFun ()
        uplc = unScript contractMKScript

        plc :: ContractParams -> Program NamedDeBruijn DefaultUni DefaultFun ()
        plc cp = getPlc
            ($$(compile [|| contractValidator ||])
            `applyCode` liftCode cp)

        plcSize :: ContractParams -> Integer
        plcSize cp = sizePlc
                ($$(compile [|| contractValidator ||])
                `applyCode` liftCode cp)

        prettyUPLC :: Doc ann0
        prettyUPLC = prettyPlcReadableDef uplc

        prettyPLC :: ContractParams -> Doc ann0
        prettyPLC cp = prettyPlcReadableDef $ plc cp

        vasilPV :: ProtocolVersion
        vasilPV =  ProtocolVersion 7 0

        scriptExBudget :: ExBudget
        scriptExBudget
            |   Just costModel  <-  defaultCostModelParams
            ,   Right model     <-  mkEvaluationContext costModel
            ,   ([], Right exB) <-  evaluateScriptCounting vasilPV Verbose model plutusCore dataToCalculate = exB
            |   otherwise = error "scriptExBudget Failed"

        getCPUExBudget :: CostingInteger
        getCPUExBudget
            |   (ExCPU unit) <- exBudgetCPU scriptExBudget = unit
            |   otherwise = 0

        getMemoryExBudget :: CostingInteger
        getMemoryExBudget
            |   (ExMemory unit) <- exBudgetMemory scriptExBudget = unit
            |   otherwise = 0

{-================================================= END OF GENERATOR SECTION =====================================================-}

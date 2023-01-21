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

module Distro.Compiler
    (   mkDistroValidator
    ,   contractParams
    ,   writeScript
    )
        where

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
import           Plutus.Script.Utils.V2.Scripts         (scriptHash)
import           Plutus.Script.Utils.V2.Typed.Scripts   (DatumType,
                                                         RedeemerType,
                                                         TypedValidator,
                                                         ValidatorTypes,
                                                         mkTypedValidatorParam,
                                                         mkUntypedValidator,
                                                         validatorScript)
import           Plutus.V1.Ledger.Scripts               (Script, Validator,
                                                         mkValidatorScript,
                                                         scriptSize, unScript,
                                                         unValidatorScript)
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
-- import qualified Plutonomy.Optimize
-- import qualified Plutonomy.Raw.Transform
-- import qualified Plutonomy.Scripts
-- import qualified Plutonomy.UPLC

{-================================================================================================================================-}
{-==========                                             CONTRACT SECTION                                               ==========-}
{-================================================================================================================================-}

------------------------------------------------------------------------------------------
-- |                               CONTRACT IMPORTS                                   | --
------------------------------------------------------------------------------------------

import           Distro.Contract                        (distroValidator)
import           Distro.DataTypes                       (DistroAction,
                                                         DistroDatum,
                                                         DistroParams (..))
import           Distro.ToJSON                          (firstDevClaimAtPhaseOneDatum,
                                                         phaseOneClaimingTokenRedeemer)

------------------------------------------------------------------------------------------
-- |                    TYPED COMPILATION & NO OPTIMIZATION                           | --
------------------------------------------------------------------------------------------

data DistroDataTypes
instance ValidatorTypes DistroDataTypes where
    type instance DatumType DistroDataTypes = DistroDatum
    type instance RedeemerType DistroDataTypes = DistroAction

mkDistroValidator :: DistroParams -> TypedValidator DistroDataTypes
mkDistroValidator = mkTypedValidatorParam @DistroDataTypes
                        $$(compile [|| distroValidator ||])
                        $$(compile [|| mkUntypedValidator ||])

------------------------------------------------------------------------------------------
-- |                           COMPILATION & OPTIMIZATION                             | --
------------------------------------------------------------------------------------------

-- mkDistroValidator :: DistroParams -> Validator
-- mkDistroValidator distroParams = Plutonomy.optimizeUPLC
--                                     $   mkValidatorScript
--                                         $   $$(compile [|| distroValidator ||])
--                                             `applyCode` liftCode distroParams

-- mkDistroValidator :: DistroParams -> Validator
-- mkDistroValidator distroParams = Plutonomy.optimizeUPLCWith
--                                     Plutonomy.aggressiveOptimizerOptions
--                                     $   mkValidatorScript
--                                         $   $$(compile [|| distroValidator ||])
--                                             `applyCode` liftCode distroParams

------------------------------------------------------------------------------------------
-- |                      COMPILATION & OPTIMIZATION WITH OPTIONS                     | --
------------------------------------------------------------------------------------------

-- defaultOptimizerOptions :: Plutonomy.Optimize.OptimizerOptions
-- defaultOptimizerOptions = Plutonomy.Optimize.OptimizerOptions
--     { Plutonomy.Optimize.ooOptimizerRounds = 2
--     , Plutonomy.Optimize.ooPreInlineConsts = True
--     , Plutonomy.Optimize.ooInlineUsedOnce  = True
--     , Plutonomy.Optimize.ooInlineSaturated = True
--     , Plutonomy.Optimize.ooSplitDelay      = True
--     , Plutonomy.Optimize.ooEtaForce        = True
--     , Plutonomy.Optimize.ooEtaFun          = True
--     , Plutonomy.Optimize.ooFloatOutLambda  = True
--     , Plutonomy.Optimize.ooFloatOutDelay   = True
--     , Plutonomy.Optimize.ooFloatOutAppArg  = Just Plutonomy.Raw.Transform.FloatOutAppArgValue
--     , Plutonomy.Optimize.ooIfLambda        = True
--     , Plutonomy.Optimize.ooCombineBindings = True
--     , Plutonomy.Optimize.ooKnownRewrites   = True
--     , Plutonomy.Optimize.ooTraceRewrite    = Just Plutonomy.Raw.Transform.TraceRewrite
--     , Plutonomy.Optimize.ooIfeRewrite      = Just Plutonomy.Raw.Transform.IfeRewrite
--     , Plutonomy.Optimize.ooAppError        = Just Plutonomy.Raw.Transform.AppErrorValue
--     , Plutonomy.Optimize.ooCommuteEquals   = True
--     , Plutonomy.Optimize.ooLetZero         = True
--     , Plutonomy.Optimize.ooCSE             = True
--     , Plutonomy.Optimize.ooFloatIn         = True
--     }

-- aggressiveOptimizerOptions :: Plutonomy.Optimize.OptimizerOptions
-- aggressiveOptimizerOptions = defaultOptimizerOptions
--     { Plutonomy.Optimize.ooOptimizerRounds = 10
--     , Plutonomy.Optimize.ooTraceRewrite    = Just Plutonomy.Raw.Transform.TraceRemove
--     , Plutonomy.Optimize.ooIfeRewrite      = Just Plutonomy.Raw.Transform.IfeRewriteMore
--     , Plutonomy.Optimize.ooAppError        = Just Plutonomy.Raw.Transform.AppErrorAll
--     , Plutonomy.Optimize.ooFloatOutAppArg  = Just Plutonomy.Raw.Transform.FloatOutAppArgAll
--     }

-- mkDistroValidator :: DistroParams -> Validator
-- mkDistroValidator distroParams = Plutonomy.optimizeUPLCWith
--                                     Plutonomy.aggressiveOptimizerOptions
--                                     $   mkValidatorScript
--                                         $   $$(compile [|| distroValidator ||])
--                                             `applyCode` liftCode distroParams

------------------------------------------------------------------------------------------
-- |                               CONTRACT DETAILS                                   | --
------------------------------------------------------------------------------------------

type ContractParams = DistroParams

contractParams :: ContractParams
contractParams = DistroParams
    {   distroDebuggerPKH   =   "bdc24850053ddf1e0c53bf20a518dcc4dcc5200ac4adb0056a1ded5c"
    }

contractName :: [Char]
contractName = "Distro"

{--------------COMMENT IT OUT FOR DISTRO TYPED VALIDATION-----------------}
contractValidator
    :: DistroParams
    -> DistroDatum
    -> DistroAction
    -> ScriptContext
    -> Bool
contractValidator = distroValidator

contractMKScript :: Script
contractMKScript = unValidatorScript $ validatorScript $ mkDistroValidator contractParams

{-------------COMMENT IT OUT FOR DISTRO UNTYPED VALIDATION----------------}
-- contractValidator
--     :: DistroParams
--     -> BuiltinData
--     -> BuiltinData
--     -> BuiltinData
--     -> ()
-- contractValidator = distroValidator

-- contractMKScript :: Script
-- contractMKScript = unValidatorScript $ mkDistroValidator contractParams

contractDescription :: TextEnvelopeDescr
contractDescription  = "This is distro contract which govern distribution of Happy token"

------------------------------------------------------------------------------------------
-- |                               CONTRACT ExBUDGET                                  | --
------------------------------------------------------------------------------------------

red :: DistroAction
red = phaseOneClaimingTokenRedeemer

dat :: DistroDatum
dat = firstDevClaimAtPhaseOneDatum

dataToCalculate :: [Data]
dataToCalculate = [toData red, toData dat]

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


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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module  Distro.DataTypes
            (   DistroParams    (..)
            ,   DistroDatum     (..)
            ,   DistroAction    (..)
            ,   PhaseOneInfo    (..)
            ,   PhaseTwoInfo    (..)
            )
                where


import           GHC.Generics            (Generic)
import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Plutus.V1.Ledger.Time   (POSIXTime)
import           Plutus.V1.Ledger.Value  (AssetClass, CurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Prelude        (Bool, Eq, Integer, (&&), (==))
import qualified Prelude                 as Haskell


newtype DistroParams = DistroParams
    {   distroDebuggerPKH   ::  PubKeyHash
    }
    deriving
        (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)

PlutusTx.makeLift ''DistroParams


data PhaseOneInfo = PhaseOneInfo
    {   firstDevPhaseOneClaimAmount  :: Integer
    ,   firstDevDidPhaseOne          :: Bool
    ,   secondDevPhaseOneClaimAmount :: Integer
    ,   secondDevDidPhaseOne         :: Bool
    ,   dateOfPhaseOne               :: POSIXTime
    }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)

instance Eq PhaseOneInfo where
    {-# INLINABLE (==) #-}
    a == b  =   ( firstDevPhaseOneClaimAmount   a == firstDevPhaseOneClaimAmount    b ) &&
                ( firstDevDidPhaseOne           a == firstDevDidPhaseOne            b ) &&
                ( secondDevPhaseOneClaimAmount  a == secondDevPhaseOneClaimAmount   b ) &&
                ( secondDevDidPhaseOne          a == secondDevDidPhaseOne           b ) &&
                ( dateOfPhaseOne                a == dateOfPhaseOne                 b )

PlutusTx.makeLift ''PhaseOneInfo
PlutusTx.makeIsDataIndexed ''PhaseOneInfo [ ( 'PhaseOneInfo, 0 ) ]


data PhaseTwoInfo = PhaseTwoInfo
    {   firstDevPhaseTwoClaimAmount  :: Integer
    ,   firstDevDidPhaseTwo          :: Bool
    ,   secondDevPhaseTwoClaimAmount :: Integer
    ,   secondDevDidPhaseTwo         :: Bool
    ,   dateOfPhaseTwo               :: POSIXTime
    }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)

instance Eq PhaseTwoInfo where
    {-# INLINABLE (==) #-}
    a == b  =   ( firstDevPhaseTwoClaimAmount   a == firstDevPhaseTwoClaimAmount    b ) &&
                ( firstDevDidPhaseTwo           a == firstDevDidPhaseTwo            b ) &&
                ( secondDevPhaseTwoClaimAmount  a == secondDevPhaseTwoClaimAmount   b ) &&
                ( secondDevDidPhaseTwo          a == secondDevDidPhaseTwo           b ) &&
                ( dateOfPhaseTwo                a == dateOfPhaseTwo                 b )

PlutusTx.makeLift ''PhaseTwoInfo
PlutusTx.makeIsDataIndexed ''PhaseTwoInfo [ ( 'PhaseTwoInfo, 0 ) ]


data DistroDatum = DistroDatum
    {   happyToken   :: AssetClass
    ,   phaseOneInfo :: PhaseOneInfo
    ,   phaseTwoInfo :: PhaseTwoInfo
    }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)

PlutusTx.makeIsDataIndexed ''DistroDatum [ ( 'DistroDatum, 0 ) ]


data DistroAction
    =   PhaseOneClaimingToken CurrencySymbol
    |   PhaseTwoClaimingToken CurrencySymbol
    |   BurningGenesisToken CurrencySymbol
    |   DistroDebuggerAction
    deriving (Haskell.Show)

PlutusTx.makeIsDataIndexed ''DistroAction   [ ( 'PhaseOneClaimingToken, 1 )
                                            , ( 'PhaseTwoClaimingToken, 2 )
                                            , ( 'BurningGenesisToken,   3 )
                                            , ( 'DistroDebuggerAction,  4 )
                                            ]

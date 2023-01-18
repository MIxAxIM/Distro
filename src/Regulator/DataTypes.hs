
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

module  Regulator.DataTypes
            (   RegulatorParams     (..)
            ,   RegulatorAction     (..)
            ,   PhaseOneInfo        (..)
            ,   PhaseTwoInfo        (..)
            ,   DevTeamAddresses    (..)
            )
                where

import           GHC.Generics             (Generic)
import           Plutus.V1.Ledger.Address (Address)
import           Plutus.V1.Ledger.Crypto  (PubKeyHash)
import           Plutus.V1.Ledger.Scripts (ValidatorHash)
import           Plutus.V1.Ledger.Value   (TokenName)
import qualified PlutusTx
import           PlutusTx.Prelude         (Eq, (&&), (==))
import qualified Prelude                  as Haskell

import           Distro.DataTypes         (PhaseOneInfo (..), PhaseTwoInfo (..))


data DevTeamAddresses = DevTeamAddresses
    {   firstDevAddress  :: !Address
    ,   secondDevAddress :: !Address
    }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)

instance Eq DevTeamAddresses where
    {-# INLINABLE (==) #-}
    a == b  =   ( firstDevAddress   a == firstDevAddress    b ) &&
                ( secondDevAddress  a == secondDevAddress   b )

PlutusTx.makeLift ''DevTeamAddresses


data RegulatorParams = RegulatorParams
    {   distroContractVH     :: !ValidatorHash
    ,   happyTokenName       :: !TokenName
    ,   phaseOneInfo         :: !PhaseOneInfo
    ,   phaseTwoInfo         :: !PhaseTwoInfo
    ,   devTeamAddresses     :: !DevTeamAddresses
    ,   regulatorDebuggerPKH :: !PubKeyHash
    }
        deriving
            (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)


PlutusTx.makeLift ''RegulatorParams


data RegulatorAction
    = GenesisMint
    | FirstDeveloperAction Address
    | SecondDeveloperAction Address
    | RegulatorDebuggerAction
        deriving
            (Haskell.Show)

PlutusTx.makeIsDataIndexed ''RegulatorAction    [ ( 'GenesisMint,               1 )
                                                , ( 'FirstDeveloperAction,      2 )
                                                , ( 'SecondDeveloperAction,     3 )
                                                , ( 'RegulatorDebuggerAction,   4 )
                                                ]

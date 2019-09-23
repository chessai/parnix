{-# language
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , RoleAnnotations
  , StandaloneDeriving
  , ViewPatterns
  #-}

module Parnix.Types
  ( -- * monad
    Nix(..)
  , runNix

    -- * expr type
  , NixExpr(..)

    -- * error types
  , NixError(..)
  , ParseError(..)
  , InstantiateError(..)
  , RealizeError(..)
  , ProcError(..)

    -- * store paths
  , StorePath(..)
  , Stage(..)

    -- * host
  , Host(..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Except (ExceptT,runExceptT)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Net.Types (IPv4)
import Data.Aeson (FromJSON)

data NixError
  = NixParseError ParseError
  | NixInstantiateError InstantiateError
  | NixRealizeError RealizeError
  | NixProcError ProcError
  | NixUnknownError String
    -- ^ catch-all error
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ParseError
  = SyntaxError String
    -- ^ the input string was not a syntactically
    --   valid nix expression
  | UnknownParseError
    -- ^ catch-all error
  deriving stock (Eq, Show)

data InstantiateError
  = NotADerivation
    -- ^ the given expression does not evaluate to a derivation
  | UnknownInstantiateError
    -- ^ catch-all error
  deriving stock (Eq, Show)

data RealizeError
  = UnknownRealizeError
    -- ^ catch-all error
  deriving stock (Eq, Show)

data ProcError
  = UnknownProcError String
    -- ^ catch-all process error
  deriving stock (Eq, Show)

newtype Nix a = Nix { getNix :: ExceptT NixError IO a }
  deriving newtype (Functor,Applicative,Monad)

runNix :: Nix a -> IO (Either NixError a)
runNix = runExceptT . getNix

data Stage
  = Derivation
    -- ^ A nix derivation is a complete build instruction
    --   that can be realized.
  | Realized
    -- ^ Once a derivation is realized, the finished output
    --   can be used.

newtype StorePath :: Stage -> Type where
  StorePath :: { fromStorePath :: FilePath } -> StorePath s
type role StorePath nominal

deriving newtype instance Show (StorePath d)

newtype NixExpr = NixExpr String
  deriving newtype (Eq, Show)

data Host = Host
  { user :: String
  , ip :: IPv4
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)


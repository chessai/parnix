{-# language
    DataKinds
  , DerivingStrategies
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , ScopedTypeVariables
  , ViewPatterns
  #-}

module Parnix.Realize
  ( parseInstRealize
  ) where

import qualified Data.List as L
import Control.Monad.Trans.Except (withExceptT)
import Control.Monad ((>=>))
import Parnix.Types
import Parnix.Util

parse :: ()
  => FilePath
     -- ^ path to nix expression
  -> Nix NixExpr
parse path = do
  let parseError :: String -> ParseError
      parseError (L.stripPrefix "error: syntax error, " -> Just mes)
        = SyntaxError mes
      parseError _
        = UnknownParseError
  expr <- Nix $ withExceptT
    (\case
      NixProcError (UnknownProcError err) -> NixParseError (parseError err)
      e -> e
    )
    (getNix (evalNixOutput "nix-instantiate" [ "--parse", path ]))
  pure (NixExpr expr)

-- todo: validate filepath
instantiate :: ()
  => NixExpr
  -> Nix (StorePath 'Derivation)
instantiate (NixExpr expr) = do
  let parseInstantiateError :: String -> InstantiateError
      parseInstantiateError (L.stripPrefix "error: expression does not evaluate to a derivation" -> Just _) = NotADerivation
      parseInstantiateError _ = UnknownInstantiateError
  path <- Nix $ withExceptT
    (\case
      NixProcError (UnknownProcError s) -> NixInstantiateError (parseInstantiateError s)
      e -> e
    )
    (getNix (evalNixOutput "nix-instantiate" [ "-E", expr ]))
  pure (StorePath path)

realize :: StorePath 'Derivation -> Nix (StorePath 'Realized)
realize (StorePath d) = storeOp [ "-r", d ]

parseInstRealize :: FilePath -> Nix (StorePath 'Realized)
parseInstRealize = parse >=> instantiate >=> realize

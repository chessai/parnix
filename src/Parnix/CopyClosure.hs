{-# language
    DataKinds
  #-}

module Parnix.CopyClosure
  ( copyClosure
  ) where

import Control.Monad (void)
import Parnix.Types
import Parnix.Util
import qualified System.Process as P

copyClosure :: StorePath 'Realized -> Host -> IO ()
copyClosure (StorePath path) h = void $ do
  P.readProcess "nix-copy-closure" [ "--to", encodeHost h, path ] ""

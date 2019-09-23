{-# language
    DataKinds
  , DerivingStrategies
  , DuplicateRecordFields
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , LambdaCase
  , NamedFieldPuns
  , RecordWildCards
  , ScopedTypeVariables
  , ViewPatterns
  #-}

module Parnix
  ( main
  ) where

import Parnix.Types
import Parnix.Util (foldCommuteIO)
import Parnix.Realize (parseInstRealize)
import Parnix.CopyClosure (copyClosure)
import qualified System.FilePath as F
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified Options.Applicative as O
import qualified Data.Yaml as Yaml

main :: IO ()
main = do
  Config{storePath,hosts,chunks} <- loadConfig
  -- we split the list up into parts of 5, to avoid overloading
  -- the link
  foldMap (foldCommuteIO (copyClosure storePath)) (splitUp chunks hosts)

splitUp :: Int -> [a] -> [[a]]
splitUp _ [] = []
splitUp chunkSize ls =
  let (first,rest) = splitAt chunkSize ls
  in first : splitUp chunkSize rest

data Args = Args
  { exprPath :: FilePath
    -- ^ path to nix expression
  , hostsPath :: FilePath
    -- ^ path to hosts file (yaml/json)
  , chunks :: Int
    -- ^ number of hosts to push out at once
  }

data Config = Config
  { storePath :: StorePath 'Realized
  , hosts :: [Host]
  , chunks :: Int
  }

loadConfig :: IO Config
loadConfig = do
  Args{exprPath,hostsPath,chunks} <- O.execParser $ O.info
    (argsParser O.<**> O.helper)
    O.fullDesc
  Yaml.decodeFileEither hostsPath >>= \case
    Left err -> do
      print err
      Exit.exitFailure
    Right hosts -> do
      let (dir,_) = F.splitFileName exprPath
      e <- Dir.withCurrentDirectory dir (runNix (parseInstRealize exprPath))
      case e of
        Left err -> do
          print err
          Exit.exitFailure
        Right storePath -> do
          pure Config{..}

argsParser :: O.Parser Args
argsParser = Args
  <$> O.strOption
    ( O.long "expr"
    <> O.help "Path to nix expression"
    <> O.metavar "FILEPATH"
    )
  <*> O.strOption
    ( O.long "hosts"
    <> O.help "Path to hosts file"
    <> O.metavar "[(USER,IPv4)]"
    )
  <*> O.option O.auto
    ( O.long "chunks"
    <> O.help "Number of hosts to push out closure to at once"
    <> O.metavar "INT"
    <> O.value 5
    <> O.showDefault
    )


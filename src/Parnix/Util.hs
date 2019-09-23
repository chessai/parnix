{-# language
    BangPatterns
  , DataKinds
  , LambdaCase
  , NamedFieldPuns
  , ScopedTypeVariables
  #-}

module Parnix.Util
  ( evalNixOutput
  , storeOp
  , encodeHost
  , foldCommuteIO
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Error.Safe (tryLast)
import Control.Exception (SomeException,try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT,withExceptT,throwE)
import Data.Foldable (foldlM)
import Parnix.Types
import System.Exit (ExitCode(..))
import qualified Data.List as L
import qualified Net.IPv4 as IPv4
import qualified System.Process as P

readProcess :: ()
  => ((String,String) -> ExitCode -> ExceptT String IO a)
     -- ^ handler for (stdout,stderr)
  -> String
     -- ^ name of the executable
  -> [String]
     -- ^ arguments
  -> Nix a
     -- ^ error: (stderr,errormsg), success: path
readProcess with exec args = Nix $ do
  (exc, out, err) <- liftIO $
    P.readCreateProcessWithExitCode
      (P.proc exec args) ""
  withExceptT (NixProcError . UnknownProcError) $ with (out,err) exc

evalNixOutput :: ()
  => String
     -- ^ name of executable
  -> [String]
     -- ^ arguments
  -> Nix String
evalNixOutput = readProcess $ \(out,err) -> \case
  ExitFailure _ -> throwE
    $ case mconcat
       . L.intersperse "\n"
       . L.dropWhile (not . L.isPrefixOf "error: ")
       . L.dropWhile (not . L.isPrefixOf "warning: ")
       . lines $ err of
       "" -> "nix didn't output any error message"
       s -> s
  ExitSuccess -> tryLast
    ("nix didn't output a store path")
    (lines out)

storeOp :: [String] -> Nix (StorePath 'Realized)
storeOp op = do
  fromStore <- Nix $ withExceptT
    (const (NixRealizeError (UnknownRealizeError)))
    (getNix (evalNixOutput "nix-store" op))
  pure (StorePath fromStore)

encodeHost :: Host -> String
encodeHost Host{user,ip} = user ++ "@" ++ IPv4.encodeString ip

forkIO_ :: IO () -> IO ()
forkIO_ = void . forkIO

foldCommuteIO :: forall t m a. (Foldable t, Monoid m)
  => (a -> IO m)
  -> t a
  -> IO m
foldCommuteIO f xs = do
  var <- newEmptyMVar
  total <- foldlM
    (\ !n a -> do
      forkIO_ (try (f a) >>= putMVar var)
      pure (n + 1)
    ) 0 xs
  let go2 :: Int -> SomeException -> IO (Either SomeException m)
      go2 !n e = if n < total
        then takeMVar var *> go2 (n + 1) e
        else pure (Left e)
  let go :: Int -> m -> IO (Either SomeException m)
      go !n !m = if n < total
        then takeMVar var >>= \case
          Left r -> go2 (n + 1) r
          Right m' -> go (n + 1) (m <> m')
        else pure (Right m)
  x <- go 0 mempty
  case x of
    Left e -> error (show e)
    Right m -> pure m


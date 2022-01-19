{-# LANGUAGE PartialTypeSignatures #-}

module Terminal
  ( Point,
    runTerminal,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (guard)
import Control.Monad.Loops (whileM)
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import Data.Word (Word64)
import FRP.Yampa (Arrow (..), DTime, Event (..), SF, maybeToEvent, reactimate)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Natural (Natural)
import System.Console.ANSI (clearScreen, hideCursor, showCursor)
import System.Console.Terminal.Size (Window (Window), size)
import System.IO (BufferMode (..), hReady, hSetBuffering, hSetEcho, stdin)
import System.Process (system)
import Terminal.Input (Key, getKey)

type Point = (Int, Int)

runTerminal :: SF (Event (NonEmpty Key), Event (Natural, Natural)) (Maybe String) -> IO ()
runTerminal sf = do
  sizeRef <- newIORef undefined
  -- _ <- system "tput smcup"
  hideCursor
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  clock <- getMonotonicTimeNSec >>= newIORef
  reactimate ((NoEvent,) <$> startup sizeRef) (getInput clock sizeRef) actuate sf
  -- _ <- system "tput rmcup"
  showCursor
  where
    startup sizeRef =
      size >>= maybe (throwIO $ userError "Not attached to terminal") \(Window x y) ->
        writeIORef sizeRef (x, y) $> Event (x, y)

terminalSize :: IORef (Natural, Natural) -> IO (Event (Natural, Natural))
terminalSize sizeRef =
  size >>= maybe (throwIO $ userError "Not attached to terminal") \(Window x y) -> do
    let s' = (x, y)
    s <- atomicModifyIORef sizeRef (const s' &&& id)
    pure $ guard (s /= s') $> s'

actuate :: Bool -> Maybe String -> IO Bool
actuate _ = \case
  Nothing -> pure True
  Just picture -> clearScreen *> putStr picture $> False

getInput ::
  IORef Word64 ->
  IORef (Natural, Natural) ->
  Bool ->
  IO (DTime, Maybe (Event (NonEmpty Key), Event (Natural, Natural)))
getInput clock sizeRef _ = do
  termSize <- terminalSize sizeRef
  keys <- whileM (hReady stdin) getKey
  t <- getMonotonicTimeNSec
  delta <- (t -) <$> readIORef clock
  let w = 5 * 10 ^ (8 :: Int) - delta
  threadDelay $ fromIntegral (w `div` 1000)
  getMonotonicTimeNSec >>= writeIORef clock
  pure (fromIntegral delta, Just ((maybeToEvent . nonEmpty) keys, termSize))

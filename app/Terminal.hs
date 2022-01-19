{-# LANGUAGE PartialTypeSignatures #-}

module Terminal
  ( Point,
    runTerminal,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (guard, void)
import Control.Monad.Loops (whileM)
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word64)
import FRP.Yampa (Arrow (..), DTime, Event (..), SF, maybeToEvent, reactimate)
import GHC.Clock (getMonotonicTimeNSec)
import System.Console.ANSI (clearScreen, hideCursor, setCursorPosition, showCursor)
import System.Console.Terminal.Size (Window (Window), size)
import System.IO (BufferMode (..), hReady, hSetBuffering, hSetEcho, stdin)
import Terminal.Input (Key, getKey)

type Point = (Int, Int)

runTerminal :: SF (Event (NonEmpty Key), Event Point) (Map Point Char, Event ()) -> IO ()
runTerminal sf = do
  sizeRef <- newIORef undefined
  prevRef <- newIORef mempty
  clearScreen
  hideCursor
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  clock <- getMonotonicTimeNSec >>= newIORef
  reactimate ((NoEvent,) <$> startup sizeRef) (getInput clock sizeRef) (actuate prevRef) sf
  showCursor
  where
    startup sizeRef =
      size >>= maybe (throwIO $ userError "Not attached to terminal") \(Window x y) ->
        writeIORef sizeRef (x, y) $> Event (x, y)

terminalSize :: IORef Point -> IO (Event Point)
terminalSize sizeRef =
  size >>= maybe (throwIO $ userError "Not attached to terminal") \(Window x y) -> do
    let s' = (x, y)
    s <- atomicModifyIORef sizeRef (const s' &&& id)
    pure $ guard (s /= s') $> s'

actuate :: IORef (Map Point Char) -> Bool -> (Map Point Char, Event ()) -> IO Bool
actuate prevRef _ = \case
  (_, Event ()) -> pure True
  (charMap, NoEvent) -> do
    prev <- atomicModifyIORef prevRef (const charMap &&& id)
    let new = Map.differenceWith (const . Just) charMap prev
        gone = Map.difference prev charMap $> ' '
    render $ new <> gone
    pure False

render :: Map Point Char -> IO ()
render = void . Map.traverseWithKey \x c -> uncurry setCursorPosition x *> putChar c

getInput ::
  IORef Word64 ->
  IORef Point ->
  Bool ->
  IO (DTime, Maybe (Event (NonEmpty Key), Event Point))
getInput clock sizeRef _ = do
  termSize <- terminalSize sizeRef
  keys <- whileM (hReady stdin) getKey
  t <- getMonotonicTimeNSec
  delta <- (t -) <$> readIORef clock
  let w = 5 * 10 ^ (8 :: Int) - delta
  threadDelay $ fromIntegral (w `div` 1000)
  getMonotonicTimeNSec >>= writeIORef clock
  pure (fromIntegral delta, Just ((maybeToEvent . nonEmpty) keys, termSize))

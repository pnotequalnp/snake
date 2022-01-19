module Terminal.Input
  ( Key (..),
    getKey,
  )
where

import Control.Exception (throwIO)
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Ix (inRange)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

data Key
  = Unicode Char
  | CursorUp Natural
  | CursorDown Natural
  | CursorForward Natural
  | CursorBack Natural
  | CSIEscapeUnknown [Char] [Char] Char
  deriving stock (Show)

getKey :: IO Key
getKey =
  getChar >>= \case
    '\ESC' -> feEscape
    c -> pure (Unicode c)

feEscape :: IO Key
feEscape =
  getChar >>= \case
    '[' ->
      csiEscape <&> \case
        ('A', _, c) | Just n <- readMaybe c -> CursorUp n
        ('A', _, []) -> CursorUp 1
        ('B', _, c) | Just n <- readMaybe c -> CursorDown n
        ('B', _, []) -> CursorDown 1
        ('C', _, c) | Just n <- readMaybe c -> CursorForward n
        ('C', _, []) -> CursorForward 1
        ('D', _, c) | Just n <- readMaybe c -> CursorBack n
        ('D', _, []) -> CursorBack 1
        (final, intermediate, parameter) -> CSIEscapeUnknown parameter intermediate final
    c -> throwIO . userError $ "Invalid Fe escape sequence character: " <> [c]

csiEscape :: IO (Char, [Char], [Char])
csiEscape = parameter
  where
    parameterChar = inRange (0x30, 0x3F)
    intermediateChar = inRange (0x20, 0x2F)
    finalChar = inRange (0x40, 0x7E)
    parameter =
      getChar >>= \case
        c@(ord -> x)
          | parameterChar x -> fmap (c :) <$> parameter
          | intermediateChar x -> (\(z, xs) -> (z, c : xs, [])) <$> intermediate
          | finalChar x -> pure (c, [], [])
        c -> throwIO . userError $ "Invalid CSI escape sequence parameter character: " <> [c]
    intermediate =
      getChar >>= \case
        c@(ord -> x)
          | intermediateChar x -> fmap (c :) <$> intermediate
          | finalChar x -> pure (c, [])
        c -> throwIO . userError $ "Invalid CSI escape sequence intermediate character: " <> [c]

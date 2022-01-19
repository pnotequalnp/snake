module Main (main) where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, pattern Empty, pattern (:<|), pattern (:|>))
import FRP.Yampa
import Numeric.Natural (Natural)
import Terminal (Point, runTerminal)
import Terminal.Input (Key (..))

main :: IO ()
main = runTerminal proc (inp, dim) -> do
  (charMap, score) <- snakeF -< (inp, dim)
  dim' <- hold (0, 0) -< dim
  returnA -< guard (isNoEvent score) $> render dim' charMap

snakeF :: SF (Event (NonEmpty Key), Event (Natural, Natural)) (Map Point Char, Event Natural)
snakeF = proc (inp, dimE) -> do
  bounds <- (\(x, y) -> (fromIntegral x - 2, fromIntegral y - 1)) ^<< hold (1, 1) -< dimE
  direction <- hold (0, 1) -< mapFilterE (directionKey . NE.head) inp
  snekHead <- accumHold (3, 3) -< Event (add2 direction)
  let score = 0
      grow = NoEvent
  snekBody <- bodyF -< (snekHead, grow)
  let walls = boundary bounds
      snek = Map.singleton snekHead 'H' <> foldMap (`Map.singleton` 'T') snekBody
      gameOver = not (inBounds bounds snekHead) || snekHead `elem` snekBody
  returnA -< (snek <> walls, guard gameOver $> score)

bodyF :: SF ((Int, Int), Event Int) (Seq (Int, Int))
bodyF = proc (snekHead, grow) -> do
  snd ^<< accumHold ((3, 3), (3, 2) :<| (3, 1) :<| Empty) -< case grow of
    NoEvent -> Event (shiftHead snekHead)
    Event _ -> Event (growHead snekHead)
  where
    shiftHead h' = \case
      (_, Empty) -> (h', Empty)
      (h, t :|> _) -> (h', h :<| t)
    growHead h' (h, t) = (h', h :<| t)

boundary :: Point -> Map Point Char
boundary (h, w) = l <> r <> t <> b <> cs
  where
    l = Map.fromList $ (,'┃') . (,0) <$> [1 .. h - 1]
    r = Map.fromList $ (,'┃') . (,w) <$> [1 .. h - 1]
    t = Map.fromList $ (,'━') . (0,) <$> [1 .. w - 1]
    b = Map.fromList $ (,'━') . (h,) <$> [1 .. w - 1]
    cs = Map.fromList [((0, 0), '┏'), ((0, w), '┓'), ((h, 0), '┗'), ((h, w), '┛')]

render :: (Natural, Natural) -> Map Point Char -> String
render (fromIntegral -> h, fromIntegral -> w) charMap = intercalate "\n" $ renderLine <$> [0 .. h - 1 :: Int]
  where
    renderLine n = fromMaybe ' ' . (`Map.lookup` charMap) . (n,) <$> [0 .. w - 1]

directionKey :: Key -> Maybe Point
directionKey = \case
  CursorUp _ -> Just (-1, 0)
  CursorDown _ -> Just (1, 0)
  CursorForward _ -> Just (0, 1)
  CursorBack _ -> Just (0, -1)
  _ -> Nothing

add2 :: Num a => (a, a) -> (a, a) -> (a, a)
add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

inBounds :: Point -> Point -> Bool
inBounds (h, w) (x, y) = 0 < x && x < h && 0 < y && y <= w

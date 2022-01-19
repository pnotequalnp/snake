{-# LANGUAGE PartialTypeSignatures #-}

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
import System.Random (StdGen, mkStdGen)
import System.Random qualified as Rand
import Terminal (Point, runTerminal)
import Terminal.Input (Key (..))

main :: IO ()
main = runTerminal proc evs -> do
  (picture, score) <- snakeF (mkStdGen 0) -< evs
  returnA -< (picture, score $> ())

snakeF ::
  StdGen ->
  SF (Event (NonEmpty Key), Event Point) (Map Point Char, Event (Maybe Natural))
snakeF g = proc (inp, dimE) -> do
  rec bounds <- hold (1, 1) -< dimE
      direction <- hold (0, 1) -< mapFilterE (directionKey . NE.head) inp
      food' <- foodF g -< bounds
      food <- dHold (10, 10) -< grow $> food'
      snekHead <- accumHold (3, 3) -< Event (add2 direction)
      score <- accumHold 0 -< grow $> succ
      snekBody <- bodyF -< (snekHead, grow)
      let grow = guard (snekHead == food)
          walls = boundary bounds
          snek = Map.singleton snekHead 'H' <> foldMap (`Map.singleton` 'T') snekBody
          scene = Map.singleton food 'F' <> snek <> walls
          gameOver = not (inBounds bounds snekHead) || snekHead `elem` snekBody
  returnA -< (scene, guard gameOver $> Just score)

foodF :: StdGen -> SF Point Point
foodF g = loopPre (Rand.split g) proc ((h, w), (g1, g2)) -> do
  let (y, g1') = Rand.randomR (1, h - 1) g1
      (x, g2') = Rand.randomR (1, w - 1) g2
  returnA -< ((y, x), (g1', g2'))

bodyF :: SF (Point, Event ()) (Seq Point)
bodyF = proc (snekHead, grow) -> do
  snd ^<< accumHold ((3, 3), (3, 2) :<| (3, 1) :<| Empty)
    -< case grow of
      NoEvent -> Event (shiftHead snekHead)
      Event _ -> Event (growHead snekHead)
  where
    shiftHead h' = \case
      (_, Empty) -> (h', Empty)
      (h, t :|> _) -> (h', h :<| t)
    growHead h' (h, t) = (h', h :<| t)

gameOverF ::
  Natural ->
  SF (Event (NonEmpty Key), Event Point) (Maybe String, Event (Maybe Natural))
gameOverF score = proc (inp, _dim) -> do
  let msg = "Game Over! You scored " <> show score <> "!"
  returnA
    -< case inp of
      NoEvent -> (Just msg, NoEvent)
      Event k -> case NE.head k of
        Unicode '\n' -> (Just msg, Event Nothing)
        Unicode ' ' -> (Just msg, Event Nothing)
        Unicode 'q' -> (Nothing, NoEvent)
        _ -> (Just msg, NoEvent)

boundary :: Point -> Map Point Char
boundary (h, w) = l <> r <> t <> b <> cs
  where
    l = Map.fromList $ (,'┃') . (,0) <$> [1 .. h - 2]
    r = Map.fromList $ (,'┃') . (,w - 1) <$> [1 .. h - 2]
    t = Map.fromList $ (,'━') . (0,) <$> [1 .. w - 2]
    b = Map.fromList $ (,'━') . (h - 1,) <$> [1 .. w - 2]
    cs = Map.fromList [((0, 0), '┏'), ((0, w - 1), '┓'), ((h - 1, 0), '┗'), ((h - 1, w - 1), '┛')]

text :: String -> Point -> Map Point Char
text msg (y, x) = Map.fromList $ zipWith ((,) . (y,)) [x ..] msg

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

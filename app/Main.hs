{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans.Has.Reader
import LiveCoding
import LiveCoding.HandlingState
import LiveCoding.PortMidi
import LiveCoding.Vivid
import Vivid hiding (sync)

main :: IO ()
main = putStrLn "hello"

sineDef :: SDBody' '["freq", "gate", "fadeSecs"] [Signal]
sineDef = do
  s <- sinOsc (freq_ (V :: V "freq"))
  s' <- envGate ~* s ~* 0.1
  out 0 [s', s']

sineCell :: (VividAction m, HasHandlingState m t) => Cell t Float ()
sineCell = proc frequency -> do
  liveSynth
    -<
      ( (realToFrac frequency :: I "freq", 1 :: I "gate", 2 :: I "fadeSecs"),
        sineDef,
        Started
      )
  returnA -< ()

portMidiCell1 :: Cell (HandlingStateT IO) () ()
portMidiCell1 = loopPortMidiC $ proc () -> do
  readEventsC "name" -< ()
  sineCell -< 440 -- this would require liftHandlingState

portMidiCell2 :: Cell (HandlingStateT IO) () ()
portMidiCell2 = loopPortMidiC $
  foreverE True $ proc () -> do
    b <- arrM (const ask) -< ()
    if b
      then readEventsC "name" -< () -- this would require liftCell $ liftCell
      else returnA -< []
    sineCell -< 440 -- this would require liftCell $ liftCell $  liftHandlingState
    throwC -< False -- this would require liftCell
    returnA -< ()

module Utils where

import Graphics.Gloss.Interface.Pure.Game

import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)

import World

asState :: Reader s a -> State s a
asState rdr = runReader rdr <$> get

roundToIntPoint :: Point -> IntPt
roundToIntPoint (x,y) = (round (x/scaleFactor), round (y/scaleFactor))

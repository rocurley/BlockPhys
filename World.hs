{-# LANGUAGE TemplateHaskell #-}

module World
( Velocity
, Velocity(..)
, IntPt
, BlockType(..)
, BlockKey(..)
, blockLoc
, BlockVal(..)
, blockType
, cci
, Block(..)
, BlockMap(..)
, LinkKey(..)
, LinkDir(..)
, LinkVal(..)
, Link(..)
, LinkMap(..)
, Force(..)
, CConKey(..)
, CConVal(..)
, CCon(..)
, CConMap(..)
, Player(..)
, JumpStatus(..)
, World(..)
, playerWidth
, playerHeight
, blocks
, links
, cCons
, cCis
, Direction(..)
, force0
, popCci
, pushCci
, linkedBlocks
, atMulti
) where

import qualified Data.Map as H hiding (Map)
import Data.Map (Map)
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict hiding (mapM,mapM_)
import Data.Monoid
import Data.Foldable

type IntPt = (Int,Int)

type Velocity = (Float,Float)
data Trajectory = Parabola Point Velocity Float deriving (Show,Eq,Ord)

data BlockType = Normal | Bedrock deriving (Eq,Ord,Show)
newtype BlockKey = BlockKey {_blockLoc :: IntPt} deriving (Eq,Ord,Show)
data BlockVal = BlockVal {_blockType :: BlockType, _cci :: Int} deriving (Eq,Ord,Show)
type Block = (BlockKey,BlockVal)
type BlockMap = Map BlockKey BlockVal

--I don't like that this can represent invalid states, but the validitiy of a link is
--pretty much tied to the global state, so I don't think it can be cromulently enforced
--by the type system.

data LinkKey = Link LinkDir IntPt deriving (Eq,Ord,Show)
data LinkDir = L2R | D2U deriving (Eq,Ord,Show)
data LinkVal = OffLink | OnLink Force deriving (Eq,Ord,Show)
type Link = (LinkKey,LinkVal)
type LinkMap  = Map LinkKey LinkVal


data Force = Force {_up :: Double, _right :: Double, _rotCCW :: Double} deriving (Eq,Ord,Show)
force0 = Force 0 0 0

data Direction = UpDir| DnDir | LfDir | RtDir deriving (Eq,Ord,Show)

type CConKey = Int
type CConVal = Int
type CCon = (CConKey,CConVal)
type CConMap = Map CConKey CConVal

data Player = Player {_playerLoc :: Point,_playerVel :: Velocity, _jumpStatus :: JumpStatus} 

data JumpStatus :: Standing BlockKey | Jumping Float | Falling | NewlyFalling Float

data World = World {_blocks :: BlockMap,
                    _links :: LinkMap,
                    _cCons :: CConMap,
                    _cCis :: [Int]
                    _player :: Player}

makeLenses ''World
makeLenses ''BlockVal
makeLenses ''BlockKey
makeLenses ''Player

playerWidth = 0.4
playerHeight = 0.8

getBlocks :: State World BlockMap
getBlocks = do
    World blocks _ _ _ <- get
    return blocks

lookupBlock :: BlockKey -> State World (Maybe BlockVal)
lookupBlock blockKey = H.lookup blockKey <$> getBlocks

setBlock :: BlockKey -> Maybe BlockVal -> State World ()
setBlock blockKey blockVal = do
    World blocks links cCons cci <- get
    put $ World (H.alter (const blockVal) blockKey blocks) links cCons cci 

alterBlock :: BlockKey -> (Maybe BlockVal -> Maybe BlockVal) -> State World ()
alterBlock blockKey f = do
    World blocks links cCons cci <- get
    put $ World (H.alter f blockKey blocks) links cCons cci 

getLinks :: State World LinkMap
getLinks = do
    World _ links _ _ <- get
    return links

lookupLink :: LinkKey -> State World (Maybe LinkVal)
lookupLink linkKey = H.lookup linkKey <$> getLinks

setLink :: LinkKey -> Maybe LinkVal -> State World ()
setLink linkKey linkVal = do
    World blocks links cCons cci <- get
    put $ World blocks(H.alter (const linkVal) linkKey links) cCons cci 

alterLink :: LinkKey -> (Maybe LinkVal -> Maybe LinkVal) -> State World ()
alterLink linkKey f = do
    World blocks links cCons cci <- get
    put $ World blocks (H.alter f linkKey links) cCons cci 

adjustLink :: LinkKey -> (LinkVal -> LinkVal) -> State World ()
adjustLink linkKey f = do
    World blocks links cCons cci <- get
    put $ World blocks (H.adjust f linkKey links) cCons cci 

popCci :: CConVal -> State World Int
popCci grounded = do
    World blocks links cCons (i:is) <- get
    put $ World blocks links (H.insert i grounded cCons) is
    return i

pushCci :: Int -> State World ()
pushCci i = do
    World blocks links cCons is <- get
    put $ World blocks links (H.delete i cCons) $ i:is

setCc :: Int -> CConVal -> State World ()
setCc i grounded = do
    World blocks links cCons is <- get
    put $ World blocks links (H.insert i grounded cCons) is

getCc :: Int -> State World (Maybe CConVal)
getCc i = do
    World _ _ cCons _ <- get
    return $ H.lookup i cCons

adjustCc :: (CConVal -> CConVal) -> CConKey -> State World ()
adjustCc f key = do
    World blocks links cCons is <- get
    put $ World blocks links (H.adjust f key cCons) is

linkedBlocks :: LinkKey -> (BlockKey,BlockKey)
linkedBlocks (Link L2R (x,y)) = (BlockKey (x,y), BlockKey (x+1,y))
linkedBlocks (Link D2U (x,y)) = (BlockKey (x,y), BlockKey (x,y+1))

newtype AEndo a m = AEndo {appAEndo :: m (a -> a)}
instance Applicative m => Monoid (AEndo a m) where
        mempty = AEndo $ pure id
        AEndo f `mappend` AEndo g = AEndo $ (.) <$> f <*> g

atMulti :: (Applicative m,Foldable f,Ord k) => f k -> (Maybe v -> m (Maybe v)) -> Map k v -> m (Map k v)
atMulti keys f mp = ($ mp) <$> appAEndo (foldMap (AEndo . alter) keys) where
    alter k = mapSet k <$> f (H.lookup k mp)

mapSet :: Ord k => k -> Maybe v -> Map k v -> Map k v
mapSet k (Just v) = H.insert k v
mapSet k Nothing = H.delete k
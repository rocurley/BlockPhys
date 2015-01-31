
module World
( IntPt
, BlockType(..)
, BlockKey(..)
, BlockVal(..)
, Block(..)
, BlockMap(..)
, LinkKey(..)
, LinkVal(..)
, Link(..)
, LinkMap(..)
, Force(..)
, CConKey(..)
, CConVal(..)
, CCon(..)
, CConMap(..)
, World(..)
, Direction(..)
, force0
, getBlocks
, lookupBlock
, setBlock
, alterBlock
, getLinks
, lookupLink
, setLink
, alterLink
, adjustLink
, popCci
, pushCci
, setCc
, getCc
, adjustCc
, linkedBlocks
) where

import qualified Data.Map as H
import Control.Applicative
-- import Control.Lens
import Control.Monad.State.Strict hiding (mapM,mapM_)

type IntPt = (Int,Int)

data BlockType = Normal | Bedrock deriving (Eq,Ord,Show)
newtype BlockKey = BlockKey IntPt deriving (Eq,Ord,Show)
data BlockVal = BlockVal BlockType Int deriving (Eq,Ord,Show)
type Block = (BlockKey,BlockVal)
type BlockMap = H.Map BlockKey BlockVal

--I don't like that this can represent invalid states, but the validitiy of a link is
--pretty much tied to the global state, so I don't think it can be cromulently enforced
--by the type system. (Why haven't you learned Agda again?)

data LinkKey = L2R IntPt | D2U IntPt deriving (Eq,Ord,Show)
data LinkVal = OffLink | OnLink Force deriving (Eq,Ord,Show)
type Link = (LinkKey,LinkVal)
type LinkMap  = H.Map LinkKey LinkVal


data Force = Force {up :: Double, right :: Double, rotCCW :: Double} deriving (Eq,Ord,Show)
force0 = Force 0 0 0

data Direction = UpDir| DnDir | LfDir | RtDir deriving (Eq,Ord,Show)

type CConKey = Int
type CConVal = Int
type CCon = (CConKey,CConVal)
type CConMap = H.Map CConKey CConVal

data World = World {_blocks :: BlockMap, _links :: LinkMap, _cCons :: CConMap, _cCis :: [Int]}

-- makeLenses ''World

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
    put $ World blocks links (H.insert i grounded cCons) $ is

getCc :: Int -> State World (Maybe CConVal)
getCc i = do
    World _ _ cCons _ <- get
    return $ H.lookup i cCons

adjustCc :: (CConVal -> CConVal) -> CConKey -> State World ()
adjustCc f key = do
    World blocks links cCons is <- get
    put $ World blocks links (H.adjust f key cCons) is

linkedBlocks :: LinkKey -> (BlockKey,BlockKey)
linkedBlocks (L2R (x,y)) = (BlockKey (x,y), BlockKey (x+1,y))
linkedBlocks (D2U (x,y)) = (BlockKey (x,y), BlockKey (x,y+1))
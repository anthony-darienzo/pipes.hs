module Main where 

import Graphics.Vty

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.ST.Strict (ST, stToIO)
import Data.STRef
    ( STRef
    , newSTRef
    , writeSTRef
    , readSTRef
    , modifySTRef )

import Data.List.Split (chunksOf)

import Data.Bifunctor (bimap)
import Data.Tuple (swap)

import qualified System.Random as R
import qualified System.Random.Stateful as RS

import qualified Data.Array.ST.Safe as AS

tickrate = 20000

pipeStyles = 
    [ "|+ ++-+  +|++ +-"
    , "│╭ ╮╯─╮  ╰│╯╰ ╭─"
    , "┃┏ ┓┛━┓  ┗┃┛┗ ┏━" ]

-- ab -> a*4 + b
-- 0 : up, 1: right, 2: down, 3: left
-- 00 means up, then up: -> ┃
-- 12 means right, then down: -> ┓
getPipeChar' :: Int -> Int -> Char
getPipeChar' a b = 
    let pipes = pipeStyles !! 0
    in seq pipes $ pipes !! (a*4 + b)

data Dir = U | R | D | L deriving (Show, Eq, Ord)
dirToInt x = case x of
    U -> 0; R -> 1; D -> 2; L -> 3

dirToDiff x = case x of
    U -> (0,-1); R -> (1,0); D -> (0,1); L -> (-1,0)

intToDir :: Int -> Dir
intToDir x = case x of
    0 -> U; 1 -> R; 2 -> D; 3 -> L;
    _ -> U

type PipeTile = (Dir,Dir)
type Pipe = [ Dir ]

type Position = (Int,Int)
type PositionTable s = AS.STArray s Position (Maybe PipeTile) 

getPipeChar :: PipeTile -> Char
getPipeChar (a,b) =
    let a' = dirToInt a
        b' = dirToInt b
    in getPipeChar' a' b'

imageForPipeTile :: PipeTile -> Image
imageForPipeTile = char defAttr . getPipeChar

checkBounds :: DisplayRegion -> (Int,Int) -> Bool
checkBounds (w,h) (x,y)  =
    0 <= x && x < w 
    && 0 <= y && y < h

writePipeTileInBound :: DisplayRegion -> PositionTable s -> Position -> PipeTile -> ST s Position
writePipeTileInBound dbounds@(w,h) ptable pos@(x,y) tile@(t1,t2) = do
    when ( checkBounds dbounds pos ) $
        AS.writeArray ptable (y,x) (Just tile)
    let (dx,dy) = dirToDiff t2
    if checkBounds dbounds (x+dx,y+dy)
        then return (x+dx,y+dy)
        else return pos

imageForSTPositionTable :: DisplayRegion -> PositionTable s -> ST s Image
imageForSTPositionTable (w,h) ptable = do
    entries <- AS.getElems ptable
    let rows    = chunksOf w entries
        imgRows = map (foldl f emptyImage) rows
    return $ vertCat imgRows
    where f prevImg e = case e of
            Nothing -> prevImg <|> backgroundFill 1 1
            Just x  -> prevImg <|> imageForPipeTile x

randomPipe :: R.RandomGen g => g -> Pipe
randomPipe g =
    let is      = R.randomRs (0, 2) g -- range of new dirs is num of all dirs - 1
        dirs    = scanl (\ prev i -> choose i prev) U is
    in concatMap (replicate 5) dirs -- smooth out pipes
    where 
        choose :: Int -> Dir -> Dir
        choose i' prev' -- Prevent undoing a pipe by removing the reverse direction from list of options.
            | i' <= ( (dirToInt prev' + 2) `mod` 4) - 1 = intToDir i'
            | otherwise = intToDir (i' + 1)

drawPipeSTSequence :: Int -> DisplayRegion -> Pipe -> ST s (ST s Image)
drawPipeSTSequence seed dbounds pipe = do
    let shiftedBounds = bimap (subtract 1) (subtract 1) dbounds
        arrBounds = swap shiftedBounds
        rgenSeed = RS.mkStdGen seed
    startpos <- 
        (RS.newSTGenM rgenSeed) >>= (\ g -> do
            x <- RS.uniformEnumRM (0, fst shiftedBounds) g
            y <- RS.uniformEnumRM (0, snd shiftedBounds) g
            return (x,y) )
    headRef  <- newSTRef startpos
    tailRef  <- newSTRef (pairs pipe)
    ptable   <- AS.newArray ( ((0,0), arrBounds) ) Nothing
    return $ f ptable headRef tailRef
    where
        f :: PositionTable s -> STRef s Position -> STRef s [ PipeTile ] -> ST s Image
        f ptable headRef pipeRef = do
            pipeHead <- readSTRef headRef
            pipeTiles <- readSTRef pipeRef
            case pipeTiles of
                []      -> return emptyImage
                p:_   -> do
                    newPos <- writePipeTileInBound dbounds ptable pipeHead p
                    writeSTRef headRef newPos
                    modifySTRef pipeRef tail
                    img <- imageForSTPositionTable dbounds ptable
                    return img
        pairs :: [a] -> [(a,a)]
        -- [1,2,3] |-> [(1,2),(2,3)]
        pairs [] = []
        pairs xs = zip xs (tail xs) -- from topograph package

drawPipeIOSequence :: Int -> DisplayRegion -> Pipe -> IO (IO Image)
drawPipeIOSequence seed dbounds pipe = do
    imgStream <- stToIO (drawPipeSTSequence seed dbounds pipe)
    return $ stToIO imgStream

drawPipeSequentially :: DisplayRegion -> Pipe -> Vty -> IO ()
drawPipeSequentially dbounds pipe vty = do
    seed <- RS.randomIO
    imgStream <- drawPipeIOSequence seed dbounds pipe
    imgStreamToDraw imgStream
    where 
        imgStreamToDraw :: IO Image -> IO ()
        imgStreamToDraw is = do
            e <- nextEventNonblocking vty 
            case e of
                Just ( EvKey kEnter [] ) -> return ()
                _ -> do
                    threadDelay tickrate
                    img <- is
                    update vty ( picForImage img )
                    imgStreamToDraw is

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    let iface = outputIface vty
    dbounds <- displayBounds iface -- (w,h)

    rgen <- R.getStdGen
    let rpipe = randomPipe rgen

    drawPipeSequentially dbounds rpipe vty
    shutdown vty
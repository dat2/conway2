{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- external
import           Control.Monad                (forM)
import           Data.List                    (group, maximumBy, partition)
import           Data.Ord                     (comparing)
import           Graphics.Gloss               (Display (..), Picture (..),
                                               black, blue, rectangleSolid, red,
                                               simulate)
import           Graphics.Gloss.Data.ViewPort (ViewPort (..))
import           Options.Generic              (Generic, ParseRecord (..),
                                               getRecord)
import           System.Random                (Random (..), getStdRandom)

-- | grid cells, this can be easily extended to support any number of colours
data Cell = Off | Red | Blue deriving (Show, Eq, Bounded, Enum)

-- | allow us to generate a random cell
instance Random Cell where
    random g = case randomR (fromEnum (minBound :: Cell), fromEnum (maxBound :: Cell)) g of
        (r, g') -> (toEnum r, g')
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

-- | A grid is the conway grid.
data Grid a = Grid { grid :: [[(Int, Int, a)]], width :: Int, height :: Int }
        deriving (Show)

-- | create a new conway grid
-- TODO 45% chance of being alive
new :: Int -> Int -> IO (Grid Cell)
new width height = do
    grid <- forM [0..(height - 1)] (forM [0..(width - 1)] . randomCell)
    return Grid { width = width, height = height, grid = grid }

-- | get an (row, column) out of the grid. this does no bounds checking.
get :: Grid a -> (Int, Int) -> a
get Grid { grid } (row, col) = thd $ grid !! row !! col
  where thd (_,_,t) = t

-- | generate a random cell
randomCell :: Int -> Int -> IO (Int, Int, Cell)
randomCell row col = (,,) row col <$> getStdRandom random

-- | a variable for the length of each cell
squareLength :: Num a => a
squareLength = 10

-- | take a grid, and return a picture
draw :: Grid Cell -> Picture
draw Grid { grid, width, height } = Pictures $ map (Pictures . map cell) grid
  where cell (row,col,Red)  = Color red (square col row)
        cell (row,col,Blue) = Color blue (square col row)
        cell (row,col,Off)  = Color black (square col row)
        offset v = fromIntegral (v * squareLength) / 2 - squareLength / 2
        square x y = Translate (fromIntegral (x * squareLength) - offset width) (fromIntegral (y * squareLength) - offset height) $ rectangleSolid squareLength squareLength

-- | step will update the grid a single simulation step
step :: ViewPort -> Float -> Grid Cell -> Grid Cell
step _ _ g@Grid { width, height, grid } = Grid { width = width, height = height, grid = map (map (nextCell g)) grid }

-- | this will calculate the next value of a given cell
nextCell :: Grid Cell -> (Int, Int, Cell) -> (Int, Int, Cell)
nextCell g@Grid { width, height, grid } (row, col, cell)
    -- If a cell is 'off' but exactly 3 of its neighbours are on, that cell will also turn on - like reproduction.
    | cell == Off && alive == 3 = (row, col, majority on)
    -- If a cell is 'on' but less than two of its neighbours are on, it will die out - like underpopulation
    -- If a cell is 'on' but more than three of its neighbours are on, it will die out - like overcrowding
    | cell /= Off && (alive < 2 || alive > 3) = (row, col, Off)
    -- If the amount of the other colors is greater then amount of that cell's own color then it just changes color.
    | cell /= Off && 1 + length same <= length diff = (row, col, majority on)
    | otherwise = (row, col, cell)
    where neighbours = map (get g) $ neighbourIndexes (width, height) (row, col)
          on = filter (/= Off) neighbours
          alive = length on
          (same, diff) = partition (== cell) $ filter (/= Off) neighbours

-- | return the element that occurs the most in the list. the list must be non empty
majority :: Eq a => [a] -> a
majority = head . snd . maximumBy (comparing fst) . map (\xs -> (length xs, xs)) . group

-- | given a (width, height) and a (row, column), return a list of [(row, columns)] to access
neighbourIndexes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbourIndexes (width,height) (r, c) = [ (row, col) | row <- map clampRow [r - 1, r, r + 1], col <- map clampCol [c - 1, c, c + 1], row /= r || col /= c ]
  where clampRow r = r `mod` height
        clampCol c = c `mod` width

-- | given width, height, fps, play the game :)
run :: Int -> Int -> Int -> IO ()
run w h fps = do
    initial <- new w h
    let win = InWindow "Conway's Extended Game of Life" (w * squareLength, h * squareLength) (0, 0)
    simulate win black fps initial draw step

-- | config (width, height, fps)
data ConwayArgs = ConwayArgs Int Int Int
    deriving (Generic, Show)

instance ParseRecord ConwayArgs

-- | run the program
main :: IO ()
main = do
    (ConwayArgs w h fps) :: ConwayArgs <- getRecord "conway2"
    run w h fps

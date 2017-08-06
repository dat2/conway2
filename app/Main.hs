{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- external
import           Control.Monad
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Options.Generic
import           System.Random

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

-- | generate a random cell
randomCell :: Int -> Int -> IO (Int, Int, Cell)
randomCell row col = (,,) row col <$> getStdRandom random

-- | given the dimensions, and the coordinates (row, col), determine a new cell
mapGrid :: ((Int,Int) -> (Int,Int,a) -> a) -> Grid a -> Grid a
mapGrid f Grid { grid, width, height } = Grid { grid = each (f (width, height)) grid, width = width, height = height }
    where each f = map (map (\(row,col,a) -> (row,col,f(row,col,a))))

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

-- | "step" takes a
step :: ViewPort -> Float -> Grid Cell -> Grid Cell
step _ _ g@Grid { width, height, grid } = Grid { width = width, height = height, grid = map (map (nextCell g)) grid }


nextCell :: Grid Cell -> (Int, Int, Cell) -> (Int, Int, Cell)
nextCell Grid { width, height, grid } (row, col, cell)
    -- If a cell is 'on' but less than two of its neighbours are on, it will die out - like underpopulation
    -- If a cell is 'on' but more than three of its neighbours are on, it will die out - like overcrowding
    | cell /= Off && same > diff && (alive < 2 || alive > 3) = (row, col, Off)
    | cell /= Off && same <= diff = (row, col, flipCell cell)
    | otherwise = (row, col, cell)
    where ns = map get $ neighbours (width, height) (row, col)
          get (row, col) = thd (grid !! row !! col)
          on = filter (/= Off) ns
          alive = length on
          same = 1 + length (filter (== cell) on)
          diff = length $ filter (/= cell) on

thd :: (a,b,c) -> c
thd (_,_,t) = t

flipCell :: Cell -> Cell
flipCell Red  = Blue
flipCell Blue = Red
flipCell x    = x

{-
- The total amount of cells in his neighbourhood of his color (including himself) is greater then the amount of cells not in his color in his neighbourhood
    -> apply normal rules, meaning that you have to count in the cells of other colors as alive cells
- If the amout of the other colors is greater then amount of that cell's own color then it just changes color.
-}

neighbours :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours (width,height) (r, c) = [ (row, col) | row <- map clampRow [r - 1, r, r + 1], col <- map clampCol [c - 1, c, c + 1], row /= r || col /= c ]
  where clampRow = wrap 0 (height - 1)
        clampCol = wrap 0 (width - 1)

-- | wrap a number to the other side
wrap :: Int -> Int -> Int -> Int
wrap min max n
    | n < min = max
    | n > max = min
    | otherwise = n

-- | given width, height, fps, play the game :)
run :: Int -> Int -> Int -> IO ()
run w h fps = do
    initial <- new w h
    let win = InWindow "Conway's Extended Game of Life" (w * squareLength, h * squareLength) (0, 0)
    simulate win black fps initial draw step

-- | config (width, height)
data ConwayArgs = ConwayArgs Int Int Int
    deriving (Generic, Show)

instance ParseRecord ConwayArgs

main :: IO ()
main = do
    (ConwayArgs w h fps) :: ConwayArgs <- getRecord "conway2"
    run w h fps

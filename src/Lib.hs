module Lib
    ( run
    ) where

import qualified Data.Vector    as V
import           Graphics.Gloss
import           System.Random

data Cell = Off | Red | Blue deriving (Show, Eq, Bounded, Enum)

instance Random Cell where
    random g = case randomR (fromEnum (minBound :: Cell), fromEnum (maxBound :: Cell)) g of
        (r, g') -> (toEnum r, g')
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

data Grid a = Grid { grid :: V.Vector (V.Vector (Int, Int, Cell)), width :: Int, height :: Int }
        deriving (Show)

new :: Int -> Int -> IO (Grid Cell)
new width height = do
    grid <- V.generateM height (V.generateM width . mkCell)
    return Grid { width = width, height = height, grid = grid }

mkCell :: Int -> Int -> IO (Int, Int, Cell)
mkCell row col = do
    cell <- getStdRandom random
    return (row, col, cell)

run :: Int -> Int -> IO ()
run w h = do
    initial <- new w h
    print initial
    display (InWindow "Nice Window" (200, 200) (10, 10)) black (Circle 80)

{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( run
    ) where

import           Control.Monad
import           Graphics.Gloss
import           System.Random

data Cell = Off | Red | Blue deriving (Show, Eq, Bounded, Enum)

instance Random Cell where
    random g = case randomR (fromEnum (minBound :: Cell), fromEnum (maxBound :: Cell)) g of
        (r, g') -> (toEnum r, g')
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

data Grid a = Grid { grid :: [[(Int, Int, Cell)]], width :: Int, height :: Int }
        deriving (Show)

new :: Int -> Int -> IO (Grid Cell)
new width height = do
    grid <- forM [0..(height - 1)] (forM [0..(width - 1)] . mkCell)
    return Grid { width = width, height = height, grid = grid }

mkCell :: Int -> Int -> IO (Int, Int, Cell)
mkCell row col = (,,) row col <$> getStdRandom random

squareLength :: Num a => a
squareLength = 50

draw :: Grid Cell -> Picture
draw Grid { grid, width, height } = Pictures $ map (Pictures . map cell) grid
  where cell (row,col,Red)  = Color red (square col row)
        cell (row,col,Blue) = Color blue (square col row)
        cell (row,col,Off)  = Color black (square col row)
        offset v = fromIntegral (v * squareLength) / 2 - squareLength / 2
        square x y = Translate (fromIntegral (x * squareLength) - offset width) (fromIntegral (y * squareLength) - offset height) $ rectangleSolid squareLength squareLength

run :: Int -> Int -> IO ()
run w h = do
    initial <- new w h
    display (InWindow "Nice Window" (w * squareLength, h * squareLength) (0, 0)) black (draw initial)

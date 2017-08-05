{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- external
import           Options.Generic

-- internal
import           Lib

data ConwayArgs = ConwayArgs Int Int
    deriving (Generic, Show)

instance ParseRecord ConwayArgs

main :: IO ()
main = do
    (ConwayArgs w h) :: ConwayArgs <- getRecord "conway2"
    run w h

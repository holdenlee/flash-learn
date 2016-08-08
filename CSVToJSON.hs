{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
--import Data.Tree.NTree.TypeDefs
import Data.Maybe
--import Text.XML.HXT.Core
--import Control.Monad
--import Control.Monad.Trans
--import Control.Monad.Trans.Maybe
--import Network.HTTP
--import Network.URI
import System.Environment
--import Control.Concurrent.ParallelIO
--import Text.Parsec
import Text.Parsec.String
import Data.Aeson

import Text.ParserCombinators.Parsec
import Data.CSV
import Data.String.Utils
--import Text.Printf
import Data.Char
--import System.Directory
import System.IO

data IndexCard = IndexCard {
      front :: String
    , back  :: String
    , avg :: Float
    } deriving (Generic, Show)

instance ToJSON IndexCard

fromRight :: Either a b -> b
fromRight (Right x) = x

mlookup :: Int -> [a] -> Maybe a
mlookup n li = if 0<=n && n<(length li) then Just (li!!n) else Nothing

mindex :: [a] -> Int -> Maybe a
mindex = flip mlookup
--or use indexed lens 

main = do
  args <- getArgs
  let file = fromMaybe "input.csv" $ args `mindex` 0
  let outputFile = fromMaybe ((takeWhile (/='.') file)++".json") $ args `mindex` 1
  putStrLn =<< (fmap show $ parseFromFile csvFile file)
  csv <- fmap fromRight $ parseFromFile csvFile file
  let ans = csv >>= (\li -> case li of
                              [] -> []
                              [name] -> [IndexCard name name 10]
                              [name, name2] -> [IndexCard name name2 10]
                              name:name2:avg:_ -> [IndexCard name name2 (read avg)])
  B.writeFile outputFile $ encode ans


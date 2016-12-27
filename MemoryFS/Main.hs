module Main where

import MemoryFS
import System.Fuse.Box
import Control.Concurrent.STM.TVar
import qualified Data.HashMap.Lazy as Map
import Data.HashMap.Lazy ( HashMap )

type MemoryMapTVar = TVar (HashMap Node ByteString)
newtype MemoryFS = MemoryFS MemoryMapTVar

main :: IO ()
main = do
  putStrLn "hello world"

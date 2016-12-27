module MemoryFS (
    MemoryFS,
    runMemoryFS,
    mainMemoryFS
  ) where

import System.Fuse.Box
import Control.Concurrent.STM.TVar
import qualified Data.HashMap.Lazy ( HashMap ) as Map
import qualified Data.ByteString.Lazy ( ByteString ) as B
import Control.Monad.Reader

-- |Type that the 'MemoryFS' is holding onto.
type MemoryMapTVar = TVar (HashMap Node ByteString)

-- |Type of the 'MemoryFS' monad stack itself.
type MemoryFS a = ReaderT MemoryMapTVar IO a

runMemoryFS :: MemoryFS a -> IO a
-- ^Constructs and executes the 'MemoryFS' monad in the 'IO' monad.
runMemoryFS act = do
    tvar <- newTVarIO $ Map.empty
    runReaderT act tvar

instance CommonFS MemoryFS () where
  fsOpen :: Node -> OpenMode -> OpenFileFlags -> MemoryFS (FSResult ())
  fsOpen = undefined

  fsClose :: Node -> () -> MemoryFS (FSResult ())
  fsClose = undefined

  fsSyncFile :: Node -> SyncType -> MemoryFS (FSResult ())
  fsSyncFile = undefined

  fsOpenDir :: Node -> MemoryFS (FSResult ())
  fsOpenDir = undefined

  fsSyncDir :: Node -> SyncType -> MemoryFS (FSResult ())
  fsSyncDir = undefined

  fsCloseDir :: Node -> MemoryFS (FSResult ())
  fsCloseDir = undefined

  fsInit :: MemoryFS ()
  fsInit = undefined

  fsDestroy :: MemoryFS ()
  fsDestory = undefined

  fsFlush :: Node -> () -> MemoryFS (FSResult ())
  fsFlush = undefined



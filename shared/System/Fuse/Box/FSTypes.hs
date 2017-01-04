{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module System.Fuse.Box.FSTypes
  (
    FuseBox(..),
    MonadIO,
    MonadError,
    module System.IO,
    module System.Posix.Types,
    module Foreign.C.Error,
    ByteString,
    Exception,
    fsError,
    fsOkay,
    fsErrorOrOkay,
    fuseBoxMain,
    readFuseBox,
    writeFuseBox,
    readWriteFuseBox
  ) where

import Data.ByteString (ByteString)
import System.Posix.Types
import Foreign.C.Error
import System.IO
import System.Fuse
import Data.Either
import Control.Monad.IO.Class
import Control.Monad.Except
import System.Fuse.Box.Node
import Control.Exception.Base (Exception)

type FuseResult a = IO (Either Errno a)
type FuseCall = IO Errno

data FuseBox m fh = FuseBox
  {
    fsbxGetFileStat :: Node -> m FileStat,
    fsbxReadSymbolicLink :: Node -> m FilePath,
    fsbxCreateDevice :: Node -> EntryType -> FileMode -> DeviceID -> m (),
    fsbxCreateDirectory :: Node -> FileMode -> m (),
    fsbxRemoveLink :: Node -> m (),
    fsbxRemoveDirectory :: Node -> m (),
    fsbxCreateSymbolicLink :: Node -> Node -> m (),
    fsbxRename :: Node -> Node -> m (),
    fsbxCreateLink :: Node -> Node -> m (),
    fsbxSetFileMode :: Node -> Node -> m (),
    fsbxSetOwnerAndGroup :: Node -> UserID -> GroupID -> m (),
    fsbxSetFileSize :: Node -> FileOffset -> m (),
    fsbxOpen :: Node -> OpenMode -> OpenFileFlags -> m fh,
    fsbxRead :: Node -> fh -> ByteCount -> FileOffset -> m ByteString,
    fsbxWrite :: Node -> fh -> ByteString -> FileOffset -> m ByteCount,
    fsbxGetFileSystemStats :: Node -> m FileSystemStats,
    fsbxFlush :: Node -> fh -> m (),
    fsbxRelease :: Node -> fh -> m (),
    fsbxSynchronizeFile :: Node -> SyncType -> m (),
    fsbxOpenDirectory :: Node -> m (),
    fsbxReadDirectory :: Node -> m [(Node, FileStat)],
    fsbxReleaseDirectory :: Node -> m (),
    fsbxSynchronizeDirectory :: Node -> SyncType -> m (),
    fsbxAccess :: Node -> Int -> m (),
    fsbxInit :: m (),
    fsbxDestroy :: m ()
  }

class (MonadIO m, MonadError Errno m) => MonadFuse m fh | m -> fh where
  runFuse :: m a -> FuseResult a
  -- ^Runs fuse and unpacks it into a 'FuseResult'

class (MonadFuse m fh) => MonadFSRead m fh | m -> fh where

class (MonadFuse m fh) => MonadFSWrite m fh | m -> fh where

fsError :: Errno -> FuseResult a
-- ^Convenience method for throwing an 'Errno' as a result.
fsError a = return (Left a)

fsOkay :: FuseResult a
-- ^Convenience method for generating an 'eOK' file system result.
fsOkay = fsError eOK

fsErrorOrOkay :: FuseResult a -> FuseCall
-- ^Returns the 'Errno' value, which may be 'eOK'. If the 'FuseCall'
-- successfully returns a value (including `()`), then this function
-- returns 'eOK'. If the call raises an error, returns the error value.
fsErrorOrOkay action = do
  result <- action
  case result of
    (Left e) -> return e
    (Right _) -> return eOK

denodeify :: (Node -> a) -> (FilePath -> a)
-- ^Converts functions that start with a 'Node' to one that
-- starts with a 'FilePath'.
denodeify f = \fp -> f (nodeFromFilePath fp)

denodeify2 :: (Node -> Node -> a) -> (FilePath -> FilePath -> a)
-- ^Converts a function that starts with two 'Node' arguments to
-- a function that starts with two 'FilePath' arguments.
denodeify2 f = \fp1 fp2 -> f (nodeFromFilePath fp1) (nodeFromFilePath fp2)

def :: FuseOperations
-- ^Shorthand for the default fuse operations.
def = defaultFuseOperations

readFuseBox :: (MonadFSRead m fh) => FuseBox m fh
-- ^Creates a 'FuseBox' which is read-only and executes in the given monad.
readFuseBox = FuseBox
  {

  }

writeFuseBox :: (MonadFSWrite m fh) => FuseBox m fh
-- ^Creates a 'FuseBox' which is write-only and executes in the given monad.
writeFuseBox = FuseBox
  {

  }

readWriteFuseBox :: (MonadFSRead m fh, MonadFSWrite m fh) => FuseBox m fh
-- ^Creates a 'FuseBox' which is read-write and executes in the given monad.
readWriteFuseBox = readBox
    {

    }
  where
    readBox = readFuseBox
    writeBox = writeFuseBux

fuseBoxMain :: (Exception e) => FuseBox m fh -> (e -> IO Errno) -> IO ()
-- ^Converts the 'FuseBox' into 'FuseOperations' and then calls 'fuseMain'.
fuseBoxMain box handler = fuseMain ops handler
  where
    ops = undefined

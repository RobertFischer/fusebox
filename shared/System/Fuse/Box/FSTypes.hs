{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module System.Fuse.Box.FSTypes
  (
    CommonFS,
    ReadFS,
    WriteFS,
    FuseBox,
    MonadIO,
    MonadError,
    FuseCall,
    FuseResult,
    module System.IO,
    module System.Posix.Types,
    module Foreign.C.Error,
    ByteString,
    fsError,
    fsOkay,
    fsErrorOrOkay
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

type FuseResult a = IO (Either Errno a)
type FuseCall = IO Errno

class (MonadIO m, MonadError Errno m) => MonadFuse m where
  runFuse :: m a -> FuseResult a
  -- ^Runs fuse and unpacks it into a 'FuseResult'

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
    (Right _) -> fsOkay

nodeify :: (FilePath -> a) -> (Node -> a)
-- ^Converts functions that start with a 'FilePath' to one that
-- starts with a 'Node'.
nodeify f = \node -> f (nodeFilePath node)

denodeify :: (Node -> a) -> (FilePath -> a)
-- ^Converts functions that start with a 'Node' to one that
-- starts with a 'FilePath'.
denodeify f = \fp -> f (nodeFromFilePath fp)

denodeify2 :: (Node -> Node -> a) -> (FilePath -> FilePath -> a)
-- ^Converts a function that starts with two 'Node' arguments to
-- a function that starts with two 'FilePath' arguments.
denodeify2 f = \fp1 fp2 -> f (nodeFromFilePath fp1) (nodeFromFilePath fp2)

-- |Shorthand for when we need to unpack to an error number exclusively.
denodeToErrno f = fsErrOrOkay <$> denodeify f

-- |Shorthand for when we need to unpack a bi-'Node' function to an error number exclusively.
denode2ToErrno f = fsErrOrOkay <$> denodeify2 f

-- |Shorthand for when we need to unpack the file system monad into a full result.
denode f = runFuseCall $ denodeify f

-- |Class for common filesystem operations.
class (MonadFuse m) => CommonFS m fh where
  fsOpen :: Node -> OpenMode -> OpenFileFlags -> m fh
  fsClose :: Node -> fh -> m ()
  fsSyncFile :: Node -> SyncType -> m ()
  fsOpenDir :: Node -> m ()
  fsSyncDir :: Node -> SyncType -> m ()
  fsCloseDir :: Node -> m ()
  fsInit :: m ()
  fsDestroy :: m ()
  fsFlush :: Node -> fh -> m ()

-- |Class for filesystem operations done by readable filesystems.
class (CommonFS m fh) => ReadFS m fh where
  fsGetFileStat :: Node -> m FileStat
  fsReadSymlink :: Node -> m Node
  fsRead :: Node -> fh -> ByteCount -> FileOffset -> m ByteString
  fsGetStats :: Node -> m FileSystemStats
  fsReadDir :: Node -> m [(Node, FileStat)]
  fsAccess :: Node -> Int -> m ()

-- |Class for filesystem operations done by writable filesystems.
class (ReadFS m fh) => WriteFS m fh where
  fsCreateLink :: Node -> Node -> m ()
  fsSetFileMode :: Node -> FileMode -> m ()
  fsSetOwnerGroup :: Node -> UserID -> GroupID -> m ()
  fsSetFileSize :: Node -> FileOffset -> m ()
  fsSetFileTimes :: Node -> EpochTime -> EpochTime -> m ()
  fsCreateDevice :: Node -> EntryType -> FileMode -> DeviceID -> m ()
  fsCreateDirectory :: Node -> FileMode -> m ()
  fsRemoveLink :: Node -> m ()
  fsRemoveDirectory :: Node -> m ()
  fsCreateSymlink :: Node -> Node -> m ()
  fsWrite :: Node -> fh -> ByteString -> FileOffset -> m ByteCount

def :: FuseOperations fh
-- ^Shorthand for the default FUSE operations.
def = defaultFuseOps

readFuseOps :: (ReadFS m fh) => m (FuseOperations fh)
readFuseOps = FuseOperations
    {
        fuseGetFileStat = denode fsGetFileStat,
        fuseReadSymbolicLink = denode fsReadSymlink,
        fuseCreateDevice = fuseCreateDevice def,
        fuseCreateDirectory = fuseCreateDirectory def,
        fuseRemoveLink = fuseRemoveLink def,
        fuseRemoveDirectory = fuseRemoveDirectory def,
        fuseCreateSymbolicLink = fuseCreateSymbolicLink def,
        fuseRename = fuseRename def,
        fuseCreateLink = fuseCreateLink def,
        fuseSetFileMode = fuseSetFileMode def,
        fuseSetFileTimes = fuseSetFileTimes def,
        fuseOpen = denode fsOpen,
        fuseRead = denode fsRead,
        fuseWrite = fuseWrite def,
        fuseGetFileSystemStats = denode fsGetStats,
        fuseFlush = denodeToErrno fsFlush,
        fuseRelease = \fp fh -> do
          _ <- denode fsClose $ fp fh
          return (),
        fuseSynchronizeFile = denodeToErrno fsSyncFile,
        fuseOpenDirectory = denodeToErrno fsOpenDir,
        fuseReadDirectory = denode fsReadDir,
        fuseReleaseDirectory = denodeToErrno fsCloseDir,
        fuseSynchronizeDirectory = denodeToErrno fsSyncDir,
        fuseAccess = denodeToErrno fsAccess,
        fuseInit = liftIO fsInit,
        fuseDestroy = liftIO fsDestroy
      }

readFuseOps :: (WriteFS m fh) => m (FuseOperations fh)
writeFuseOps = FuseOperations
      {
        fuseGetFileStat = denode fsGetFileStat,
        fuseReadSymbolicLink = denode fsReadSymlink,
        fuseCreateDevice = denodeToErrno fsCreateDevice,
        fuseCreateDirectory = denodeToErrno fsCreateDirectory,
        fuseRemoveLink = denodeToErrno fsRemoveLink,
        fuseRemoveDirectory = denodeToErrno fsRemoveDirectory,
        fuseCreateSymbolicLink = denode2ToErrno fsCreateSymlink,
        fuseRename = denode2ToErrno fsRename,
        fuseCreateLink = denode2ToErrno fsCreateLink,
        fuseSetFileMode = denode2ToErrno fsSetFileMode,
        fuseSetOwnerAndGroup = denodeToErrno fsSetOwnerGroup,
        fuseSetFileSize = denodeToErrno fsSetFileSize,
        fuseOpen = denode fsOpen,
        fuseRead = denode fsRead,
        fuseWrite = denode fsWrite,
        fuseGetFileSystemStats = denode fsGetStats,
        fuseFlush = denodeToErrno fsFlush,
        fuseRelease = \fp fh -> do
          _ <- denode fsClose $ fp fh
          return (),
        fuseSynchronizeFile = denodeToErrno fsSyncFile,
        fuseOpenDirectory = denodeToErrno fsOpenDir,
        fuseReadDirectory = denode fsReadDir,
        fuseReleaseDirectory = denodeToErrno fsCloseDir,
        fuseSynchronizeDirectory = denodeToErrno fsSyncDir,
        fuseAccess = denodeToErrno fsAccess,
        fuseInit = liftIO fsInit,
        fuseDestroy = liftIO fsDestroy
      }

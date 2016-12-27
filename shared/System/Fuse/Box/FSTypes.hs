module System.Fuse.Box.FSTypes
  (
    CommonFS,
    ReadFS,
    WriteFS,
    FuseBox
  ) where

import System.Fuse
import Data.Either
import Control.Monad.IO.Class

-- |Captures the result of a file system call.
newtype FSResult a = FSResult (Either Errno a)

iofsResultUnpack :: (MonadIO m) => m a -> IO (Either Errno a)
iofsResultUnpack act = do
  (FSResult a) <- liftIO act
  return a

fsResultToEither :: FSResult a -> Either Errno a
-- ^Translates a file system result into an 'Either'.
fsResultToEither (FSResult a) = a

fsError :: Errno -> FSResult a
-- ^Convenience method for generating a file system error result.
fsError err = FSResult $ Left err

fsOkay :: FSResult ()
-- ^Convenience method for generating an 'eOK' file system result.
fsOkay = FSResult $ Left eOK

fsResult :: a -> FSResult a
-- ^Convenience method for returning a 'FSResult' that returns a value.
--  If you pass '()' as an argument to this function, it is the same as
--  calling 'fsOkay'.
fsResult () = fsOkay
fsResult a = FSResult $ Right a

fsErrOrOkay :: FSResult a -> Errno
-- ^Returns the 'Errno' value, which may be 'eOK'. If the value of
-- the 'FSResult' is a 'Right', then it returns 'eOK'. If the value
-- is a 'Left', returns the wrapped value.
fsErrOrOkay (FSResult (Left err)) = err
fsErrOrOkay (FSResult (Right _))  = eOK

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

-- |Shorthand for the default FUSE operations.
def = defaultFuseOps

-- |Shorthand for when we need to unpack to an error number exclusively.
denodeToErrno f = fsErrorOrOkay <$> denodeify f

-- |Shorthand for when we need to unpack a bi-'Node' function to an error number exclusively.
denode2ToErrno f = fsErrorOrOkay <$> denodeify2 f

-- |Shorthand for when we need to unpack the file system monad into a full result.
denode f = iofsResultUnpack $ denodeify f

-- |Class for common filesystem operations.
class (MonadIO m) => CommonFS m fh where
  fsOpen :: Node -> OpenMode -> OpenFileFlags -> m (FSResult fh)
  fsClose :: Node -> fh -> m (FSResult ())
  fsSyncFile :: Node -> SyncType -> m (FSResult ())
  fsOpenDir :: Node -> m (FSResult ())
  fsSyncDir :: Node -> SyncType -> m (FSResult ())
  fsCloseDir :: Node -> m (FSResult ())
  fsInit :: m ()
  fsDestroy :: m ()
  fsFlush :: Node -> fh -> m (FSResult ())

-- |Class for filesystem operations done by readable filesystems.
class (CommonFS m fh) => ReadFS m fh where
  fsGetFileStat :: Node -> m (FSResult FileStat)
  fsReadSymlink :: Node -> m (FSResult Node)
  fsRead :: Node -> fh -> ByteCount -> FileOffset -> m (FSResult ByteString)
  fsGetStats :: Node -> m (FSResult FileSystemStatus)
  fsReadDir :: Node -> m (FSResult [(Node, FileStat)])
  fsAccess :: Node -> Int -> m (FSResult ()),

-- |Class for filesystem operations done by writable filesystems.
class (CommonFS m fh) => WriteFS m fh where
  fsCreateLink :: Node -> Node -> m (FSResult ())
  fsSetFileMode :: Node -> FileMode -> m (FSResult ())
  fsSetOwnerGroup :: Node -> UserID -> GroupID -> m (FSResult ())
  fsSetFileSize :: Node -> FileOffset -> m (FSResult ())
  fsSetFileTimes :: Node -> EpochTime -> EpochTime -> m (FSResult ())
  fsCreateDevice :: Node -> EntryType -> FileMode -> DeviceID -> m (FSResult ())
  fsCreateDirectory :: Node -> FileMode -> m (FSResult ())
  fsRemoveLink :: Node -> m (FSResult ())
  fsRemoveDirectory :: Node -> m (FSResult ())
  fsCreateSymlink :: Node -> Node -> m (FSResult ())
  fsWrite :: Node -> fh -> ByteString -> FileOffset -> m (FSResult ByteCount)

-- |Class for things that can provide filesystem operations
class FuseBox m fh where
  fuseOperations :: m (FuseOperations fh)

instance {-# OVERLAPPABLE #-} (ReadFS m fh) => FuseBox m fh where
  fuseOperations = FuseOperations
    {
        fuseGetFileStat = denode fsGetFileStat
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

instance {-# OVERLAPPABLE #-} (WriteFS m fh) => FuseBox m fh where
    fuseOperations = FuseOperations
      {
        fuseGetFileStat = fuseGetFileStat def,
        fuseReadSymbolicLink = fuseReadSymbolicLink def,
        fuseCreateDevice = denodeToErrno fsCreateDevice,
        fuseCreateDirectory = denodeToErrno fsCreateDirectory,
        fuseRemoveLink = denodeToErrno fsRemoveLink,
        fuseRemoveDirectory = denodeToErrno fsRemoveDirectory,
        fuseCreateSymbolicLink = denode2ToErrno fsCreateSymlink,
        fuseRename = denode2ToErrno fsRename,
        fuseCreateLink = denode2ToErrno fsCreateLink,
        fuseSetFileMode = denodeToErrno fsSetFileMode,
        fuseSetOwnerAndGroup = denodeToErrno fsSetOwnerGroup,
        fuseSetFileSize = denodeToErrno fsSetFileSize,
        fuseSetFileTimes = denodeToErrno fsSetFileTimes,
        fuseOpen = denode fsOpen,
        fuseRead = fuseRead def,
        fuseWrite = denode fsWrite,
        fuseGetFileSystemStats = fuseGetFileSystemStats def,
        fuseFlush = denodeToErrno fsFlush,
        fuseRelease = \fp fh -> do
          _ <- denode fsClose $ fp fh
          return (),
        fuseSynchronizeFile = denodeToErrno fsSyncFile,
        fuseOpenDirectory = denodeToErrno fsOpenDir,
        fuseReadDirectory = fuseReleaseDirectory def,
        fuseReleaseDirectory = denodeToErrno fsCloseDir,
        fuseSynchronizeDirectory = denodeToErrno fsSyncDir,
        fuseAccess = fuseAccess def,
        fuseInit = liftIO fsInit,
        fuseDestroy = liftIO fsDestroy
      }

instance {-# OVERLAPPING #-} (ReadFS m fh, WriteFS m fh) => FuseBox m fh where
    fuseOperations = FuseOperations
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

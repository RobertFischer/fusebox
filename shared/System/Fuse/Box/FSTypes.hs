module System.Fuse.Box.FSTypes
  (
    module System.Fuse.Box.FSTypes
  ) where

import System.Fuse
import Data.Either
import Control.Monad.Trans.Either

-- |Captures the result of a file system call.
newtype FSResult a = FSResult (Either Errno a)
type IOFSResult a = IO (FSResult a)
type FileIOResult a = Node -> IOFSResult a

iofsResultUnpack :: IOFSResult a -> IO (Either Errno a)
iofsResultUnpack act = do
  (FSResult a) <- act
  return a

fsResultToEither :: FSResult a -> Either Errno a
-- ^Translates a file system result into an 'Either'.
fsResultToEither (FSResult a) = a

fsError :: Errno -> FSResult a
-- ^Convenience method for generating a file system error result.
fsError err = FSResult $ Left err

fsOkay :: FSResult ()
-- ^Convenience method for generating an 'eOK' file system result.
--  (This is technically an error, but using 'fsError' to mean
--  'eOK' causes cognitive dissonance.)
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

-- |Class for common filesystem operations.
class CommonFS fh where
  fsOpen :: OpenMode -> OpenFileFlags -> FileIOResult fh
  fsClose :: fh -> FileIOResult ()
  fsSyncFile :: SyncType -> FileIOResult ()
  fsOpenDir :: FileIOResult ()
  fsSnycDir :: SyncType -> FileIOResult ()
  fsCloseDir :: FileIOResult ()
  fsInit :: IO ()
  fsDestroy :: IO ()
  fsFlush :: fh -> FileIOResult ()

-- |Class for filesystem operations done by readable filesystems.
class CommonFS fh => ReadFS fh where
  fsGetFileStat :: FileIOResult FileStat
  fsReadSymlink :: FileIOResult Node
  fsRead :: fh -> ByteCount -> FileOffset -> FileIOResult Byte
  fsGetStats :: FileIOResult FileSystemStatus
  fsReadDir :: FileIOResult [(Node, FileStat)]
  fsAccess :: Int -> FileIOResult (),

-- |Class for filesystem operations done by writable filesystems.
class CommonFS fh => WriteFS fh where
  fsCreateLink :: Node -> FileIOResult ()
  fsSetFileMode :: FileMode FileIOResult ()
  fsSetOwnerGroup :: UserID -> GroupID -> FileIOResult ()
  fsSetFileSize :: FileOffset -> FileIOResult ()
  fsSetFileTimes :: EpochTime -> EpochTime -> FileIOResult ()
  fsCreateDevice :: EntryType -> FileMode -> DeviceID -> FileIOResult ()
  fsCreateDirectory :: FileMode -> FileIOResult ()
  fsRemoveLink :: FileIOResult ()
  fsRemoveDirectory :: FileIOResult ()
  fsCreateSymlink :: Node -> FileIOResult ()
  fsWrite :: fh -> ByteString -> FileOffset -> FileIOResult ByteCount

-- |Class for things that can provide filesystem operations
class FuseBox fh where
  fuseOperations :: FuseOperations fh

instance {-# OVERLAPPABLE #-} ReadFS fh => FuseBox fh where
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
        fuseOpen = \fp mode flags -> denode (fsOpen mode flags) fp,
        fuseRead = \fp fh byteCnt offset -> denode (fsRead fh byteCnt offset) fp,
        fuseWrite = fuseWrite def,
        fuseGetFileSystemStats = denode fsGetStats,
        fuseFlush = \fp fh -> fsErrorOrOkay <$> denode (fsFlush fh) fp,
        fuseRelease = \fp fh -> do
          _ <- denode (fsClose fh) fp
          return (),
        fuseSynchronizeFile = \fp syncType -> fsErrOrOkay <$> denode (f syncType) fp,
        fuseOpenDirectory = denodeToErrno fsOpenDir,
        fuseReadDirectory = denode fsReadDir,
        fuseReleaseDirectory = denodeToErrno fsCloseDir,
        fuseSynchronizeDirectory = denodeToErrno fsSyncDir,
        fuseAccess = \fp perms -> denodeToErrno (fsAccess perms) fp,
        fuseInit = fsInit,
        fuseDestroy = fsDestroy
      }
    where
      def = defaultFuseOps
      denodeToErrno f fp = fsErrorOrOkay <$> f (nodeFromFilePath fp)
      denode f fp = iofsResultUnpack $ f (nodeFromFilePath fp)

instance {-# OVERLAPPABLE #-} WriteFS fh => FuseBox fh where
    fuseOperations = FuseOperations
      {
        fuseGetFileStat = fuseGetFileStat def,
        fuseReadSymbolicLink = fuseReadSymbolicLink def,
        fuseCreateDevice = \fp entry mode devId -> denodeToErrno (fsCreateDevice entry mode devId) fp,
        fuseCreateDirectory = \fp mode -> denodeToErrno (fsCreateDirectory mode) fp,
        fuseRemoveLink = denodeToErrno fsRemoveLink,
        fuseRemoveDirectory = denodeToErrno fsRemoveDirectory,
        fuseCreateSymbolicLink = \source target -> denodeToErrno (fsCreateSymlink $ nodeFromFilePath target) source,
        fuseRename = \source target -> denodeToErrno (fsRename $ nodeFromFilePath target) source,
        fuseCreateLink = \source target -> denodeToErrno (fsCreateLink $ nodeFromFilePath target) source,
        fuseSetFileMode = \fp mode -> denodeToErrno (fsSetFileMode mode) fp,
        fuseSetOwnerAndGroup = \fp usrId grpId -> denodeToErrno (fsSetOwnerGroup usrId grpId) fp,
        fuseSetFileSize = \fp offset -> denodeToErrno (fsSetFileSize offset) fp,
        fuseSetFileTimes = \fp time1 time2 -> denodeToErrno (fsSetFileTimes time1 time2) fp,
        fuseOpen = \fp mode flags = denode (fsOpen mode flags) fp,
        fuseRead = fuseRead def,
        fuseWrite = \fp fh bytes offset = denode (fsWrite fh bytes offset) fp,
        fuseGetFileSystemStats = fuseGetFileSystemStats def,
        fuseFlush = \fp fh -> denodeToErrno (fsFlush fh) fp,
        fuseRelease = \fp fh -> do
          _ <- denode (fsClose fh) fp
          return (),
        fuseSynchronizeFile = \fp syncType -> denodeToErrno (fsSyncFile syncType) fp,
        fuseOpenDirectory = denodeToErrno fsOpenDir,
        fuseReadDirectory = fuseReleaseDirectory def,
        fuseReleaseDirectory = denodeToErrno fsCloseDir,
        fuseSynchronizeDirectory = \fp syncType -> denodeToErrno (fsSnycDir syncType) fp,
        fuseAccess = fuseAccess def,
        fuseInit = fsInit,
        fuseDestroy = fsDestroy
      }
  where
    def = defaultFuseOps
    denoded f fp = f $ nodeFromFilePath fp
    denodeToErrno f fp = fsErrorOrOkay <$> denoded f fp
    denode f fp = iofsResultUnpack $ denoded f fp

instance {-# OVERLAPPING #-} (ReadFS fh, WriteFS fh) => FuseBox fh where
    fuseOperations = FuseOperations
      {
        fuseGetFileStat = denode fsGetFileStat,
        fuseReadSymbolicLink = denode fsReadSymlink,
        fuseCreateDevice = \fp entryType mode deviceId ->
          denodeToErrno (fsCreateDevice entryType mode deviceId) fp,
        fuseCreateDirectory = \fp mode -> denodeToErrno (fsCreateDirectory mode) fp,
        fuseRemoveLink = denodeToErrno fsRemoveLink,
        fuseRemoveDirectory = denodeToErrno fsRemoveDirectory,
        fuseCreateSymbolicLink = \source target ->
          denodeToErrno (fsCreateSymlink $ nodeFromFilePath target) source,
        fuseRename = \source target ->
          denodeToErrno (fsRename $ nodeFromFilePath target) source,
        fuseCreateLink = \source target ->
          denodeToErrno (fsCreateLink $ nodeFromFilePath target) source,
        fuseSetFileMode = \fp mode -> denodeToErrno (fsSetFileMode mode) fp,
        fuseSetOwnerAndGroup = \fp usrId grpId ->
          denodeToErrno (fsSetOwnerGroup usrId grpId) fp,
        fuseSetFileSize = \fp offset -> denodeToErrno (fsSetFileSize offset) fp,
        fuseOpen = \fp mode flags = denode (fsOpen mode flags) fp,
        fuseRead = \fp fh byteCnt offset -> denode (fsRead fh byteCnt offset) fp,
        fuseWrite = \fp fh bytes offset = denode (fsWrite fh bytes offset) fp,
        fuseGetFileSystemStats = denode fsGetStats,
        fuseFlush = \fp fh -> denodeToErrno (fsFlush fh) fp,
        fuseRelease = \fp fh -> do
          _ <- denode (fsClose fh) fp
          return (),
        fuseSynchronizeFile = \fp syncType -> denodeToErrno (f syncType) fp,
        fuseOpenDirectory = denodeToErrno fsOpenDir,
        fuseReadDirectory = denode fsReadDir,
        fuseReleaseDirectory = denodeToErrno fsCloseDir,
        fuseSynchronizeDirectory = denodeToErrno fsSyncDir,
        fuseAccess = \fp perms -> denodeToErrno (fsAccess perms) fp,
        fuseInit = fsInit,
        fuseDestroy = fsDestroy
      }
  where
    def = defaultFuseOps
    denoded f fp = f $ nodeFromFilePath fp
    denodeToErrno f fp = fsErrorOrOkay <$> denoded f fp
    denode f fp = iofsResultUnpack $ denoded f fp


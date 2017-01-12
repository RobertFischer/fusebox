{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module System.Fuse.Box.FSTypes
  (
    FuseBox(..),
    MonadIO,
    MonadError,
    MonadFuse,
    MonadFSRead,
    MonadFSWrite,
    module System.IO,
    module System.Posix.Types,
    module Foreign.C.Error,
    ByteString,
    Exception,
    fsError,
    fsOkay,
    fsErrorOrOkay,
    fuseBoxMain,
    roFuseBox,
    rwFuseBox
  ) where

import Control.Exception.Base (Exception)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Either
import Foreign.C.Error
import System.Fuse
import System.Fuse.Box.Node
import System.IO
import System.Posix.Types

type FuseResult a = IO (Either Errno a)
type FuseCall = IO Errno

data FuseBox m fh = FuseBox
  {
    fsbxFhToNode :: fh -> m Node,
    fsbxGetFileStat :: Node -> m FileStat,
    fsbxReadSymbolicLink :: Node -> m Node,
    fsbxCreateDevice :: Node -> EntryType -> FileMode -> DeviceID -> m (),
    fsbxCreateDirectory :: Node -> FileMode -> m (),
    fsbxRemoveLink :: Node -> m (),
    fsbxRemoveDirectory :: Node -> m (),
    fsbxCreateSymbolicLink :: Node -> Node -> m (),
    fsbxRename :: Node -> Node -> m (),
    fsbxCreateLink :: Node -> Node -> m (),
    fsbxSetFileMode :: Node -> FileMode -> m (),
    fsbxSetOwnerAndGroup :: Node -> UserID -> GroupID -> m (),
    fsbxSetFileSize :: Node -> FileOffset -> m (),
    fsbxOpen :: Node -> OpenMode -> OpenFileFlags -> m fh,
    fsbxRead :: fh -> ByteCount -> FileOffset -> m ByteString,
    fsbxWrite :: fh -> ByteString -> FileOffset -> m ByteCount,
    fsbxGetFileSystemStats :: Node -> m FileSystemStats,
    fsbxFlush :: fh -> m (),
    fsbxRelease :: fh -> m (),
    fsbxSynchronizeFile :: Node -> SyncType -> m (),
    fsbxOpenDirectory :: Node -> m (),
    fsbxReadDirectory :: Node -> m [(Node, FileStat)],
    fsbxReleaseDirectory :: Node -> m (),
    fsbxSynchronizeDirectory :: Node -> SyncType -> m (),
    fsbxAccess :: Node -> Int -> m (),
    fsbxInit :: m (),
    fsbxDestroy :: m (),
    fsbxSetFileTimes :: Node -> EpochTime -> EpochTime -> m ()
  }

-- |Typeclass defining basic Fuse operations.
class (MonadIO m, MonadError Errno m) => MonadFuse m fh | m -> fh where
  -- |Runs fuse and unpacks it into a 'FuseResult'.
  runFuse :: m a -> FuseResult a

  -- |Convert a file handle into a 'Node'.
  mfFhToNode :: fh -> m Node

  -- |Open the file at the given path and return the file handle to it.
  mfOpen :: Node -> OpenMode -> OpenFileFlags -> m fh

  -- |Flush the file, clearing any buffers.
  mfFlush :: fh -> m ()

  -- |Close the file handle.
  mfRelease :: fh -> m ()

  -- |Check file access permissions
  mfAccess :: Node -> Int -> m ()

  -- |Return the file stats for the file at the given node.
  mfGetFileStat :: Node -> m FileStat

  -- |Given a node anywhere on the file system, return details about the file system
  mfGetFileSystemStats :: Node -> m FileSystemStats

  -- |Initialize the file system
  mfInit :: m ()

  -- |Destroy the file system
  mfDestroy :: m ()

-- |Typeclass defining read operations (regardless of ability to do write operations).
class (MonadFuse m fh) => MonadFSRead m fh | m -> fh where

  -- |Perform a read from the file handle. It should read up to the given number of bytes
  -- starting at the given offset.
  mfRead :: fh -> ByteCount -> FileOffset -> m ByteString

  -- |Return the target of the symbolic link at the given location.
  mfReadSymbolicLink :: Node -> m Node

  -- |Opens a directory, most specifically checking to see that the open operation is permited.
  mfOpenDirectory :: Node -> m ()

  -- |Implements 'readdir(3)', returning the entire contents of the directory as a list of tuples.
  mfReadDirectory :: Node -> m [(Node, FileStat)]

  -- |Implements 'closedir(3)'.
  mfReleaseDirectory :: Node -> m ()

-- |Typeclass defining write operations (regardless of ability to do read operations).
class (MonadFuse m fh, MonadFSRead m fh) => MonadFSWrite m fh | m -> fh where

  -- |Implements 'createDevice' ('mknod(2)'), and also called for regular file creation.
  mfCreateDevice :: Node -> EntryType -> FileMode -> DeviceID -> m ()

  -- |Implements 'createDirectory' ('mkdir(2)')
  mfCreateDirectory :: Node -> FileMode -> m ()

  -- |Implements 'removeDirectory' ('rmdir(2)')
  mfRemoveDirectory :: Node -> m ()

  -- |Implements 'removeLink' ('unlink(2)')
  mfRemoveLink ::  Node -> m ()

  -- |Implements 'createSymbolicLink' ('symlink(2)')
  mfCreateSymbolicLink :: Node -> Node -> m ()

  -- |Implements 'rename' ('rename(2)')
  mfRename :: Node -> Node -> m ()

  -- |Implements 'setFileMode' ('chmod(2)')
  mfSetFileMode :: Node -> FileMode -> m ()

  -- |Implements 'setOwnerAndGroup' ('chown(2)')
  mfSetOwnerAndGroup :: Node -> UserID -> GroupID -> m ()

  -- |Implements 'setFileSize' ('truncate(2)')
  mfSetFileSize :: Node -> FileOffset -> m ()

  -- |Implements 'setFileTimes' ('utime(2)')
  mfSetFileTimes :: Node -> EpochTime -> EpochTime -> m ()

  -- |Implements 'pwrite(2)'
  mfWrite :: fh -> ByteString -> FileOffset -> m ByteCount

  -- |Implements 'fsync(2)'
  mfSynchronizeFile :: Node -> SyncType -> m ()

  -- |Synchronize all the contents of the directory
  mfSynchronizeDirectory :: Node -> SyncType -> m ()


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

notSup :: (MonadFuse m fh) => m a
-- ^Utility function for when a 'FuseBox' does not support an operation.
notSup = throwError eNOTSUP

roFuseBox :: (MonadFSRead m fh) => FuseBox m fh
-- ^Creates a 'FuseBox' which is read-only and executes in the given monad.
roFuseBox = FuseBox
  {
    fsbxCreateLink = \_ _ -> notSup,
    fsbxFhToNode = mfFhToNode,
    fsbxGetFileStat = mfGetFileStat,
    fsbxReadSymbolicLink = mfReadSymbolicLink,
    fsbxCreateDevice = \_ _ _ _ -> notSup,
    fsbxCreateDirectory = \_ _ -> notSup,
    fsbxRemoveLink = \_ -> notSup,
    fsbxRemoveDirectory = \_ -> notSup,
    fsbxCreateSymbolicLink = \_ _ -> notSup,
    fsbxRename = \_ _ -> notSup,
    fsbxSetFileMode = \_ _ -> notSup,
    fsbxSetOwnerAndGroup = \_ _ _ -> notSup,
    fsbxSetFileSize = \_ _ -> notSup,
    fsbxSetFileTimes = \_ _ _ -> notSup,
    fsbxOpen = mfOpen,
    fsbxRead = mfRead,
    fsbxWrite = \_ _ _ -> notSup,
    fsbxGetFileSystemStats = mfGetFileSystemStats,
    fsbxFlush = mfFlush,
    fsbxRelease = mfRelease,
    fsbxSynchronizeFile = \_ _ -> notSup,
    fsbxOpenDirectory = mfOpenDirectory,
    fsbxReadDirectory = mfReadDirectory,
    fsbxReleaseDirectory = mfReleaseDirectory,
    fsbxSynchronizeDirectory = \_ _ -> notSup,
    fsbxAccess = mfAccess,
    fsbxInit = mfInit,
    fsbxDestroy = mfDestroy
  }

rwFuseBox :: (MonadFSWrite m fh) => FuseBox m fh
-- ^Creates a 'FuseBox' which is read-write and executes in the given monad.
rwFuseBox = roFuseBox
  {
    fsbxCreateDevice = mfCreateDevice,
    fsbxCreateDirectory = mfCreateDirectory,
    fsbxRemoveLink = mfRemoveLink,
    fsbxRemoveDirectory = mfRemoveDirectory,
    fsbxCreateSymbolicLink = mfCreateSymbolicLink,
    fsbxRename = mfRename,
    fsbxSetFileMode = mfSetFileMode,
    fsbxSetOwnerAndGroup = mfSetOwnerAndGroup,
    fsbxSetFileSize = mfSetFileSize,
    fsbxSetFileTimes = mfSetFileTimes,
    fsbxWrite = mfWrite,
    fsbxSynchronizeFile = mfSynchronizeFile,
    fsbxSynchronizeDirectory = mfSynchronizeDirectory
  }

fuseBoxMain :: (Exception e) => FuseBox m fh -> (e -> IO Errno) -> IO ()
-- ^Converts the 'FuseBox' into 'FuseOperations' and then calls 'fuseMain'.
fuseBoxMain box handler = fuseMain ops handler
  where
    ops = undefined

-- TODO Implement the conversion from a FuseBox to a FuseOperations

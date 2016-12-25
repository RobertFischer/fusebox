module System.Fuse.Box.Node
  (
    FilePath,
    Node(..),
    Text,
    nodeName,
    nodeString,
    simplifyNode,
    nodeBacktracks,
    nodesBacktrack,
    concatNodes,
    nodeFromFilePath
  ) where

import System.Fuse.Box

import qualified Data.Text as T
import Data.Text ( Text )
import Data.List
import qualified System.FilePath.Posix as POSIX

-- |Represents a node, which is a parsed filename.
--  A 'NodeSegment' is a single segment (such as "bar" in "/foo/bar/baz").
--  A 'NodePath' is a collection of segments, without any "." or ".." metacharacters.
data Node = NodeSegment Text | NodePath [Node]

nodesBacktrack :: [Node] -> Bool
-- ^Convenience version of 'nodeBacktracks' that wraps the list of nodes in a 'NodePath'
--  automatically. Makes for much more readable code.
nodesBacktrack = any nodeBacktracks

nodeBacktracks :: Node -> Bool
-- ^Returns 'True' if the node is a single segment of "..", or the path contains
--  a segment of "..".
nodeBacktracks (NodeSegment "..") = True
nodeBacktracks (NodePath []) = False
nodeBacktracks (NodePath ps) = nodesBacktrack ps
nodeBacktracks _ = False

nodeName :: Node -> Text
-- ^Returns a 'Text' representation of the node name.
--  A 'NodeSegment' is unpacked to 'Text'.
--  A 'NodePath' is the name of the member nodes joined by "/" characters.
nodeName (NodeSegment t) = t
nodeName (NodePath nodes) = T.intercalate POSIX.pathSeparator $ map nodeName nodes

nodeString :: Node -> String
-- ^Convenience function that provides the return value of 'nodeName' as a 'String'.
nodeString = T.unpack . nodeName

simplifyNode :: Node -> Node
-- ^Simplifies the node. A segment of ".", "/", or ".." is returned as the empty path;
--  otherwise, the segment is returned directly. A node path has all "." and
--  "/" segments removed, and each ".." is removed along with the previous segment
--  (if any); other segments are returned unadulterated. A node path is also reduced
--  to being solely consisting of segments, and no sub-paths.
simplifyNode s@(NodeSegment t)
  | t == "" = NodePath []
  | t == "." = NodePath []
  | t == ".." = NodePath []
  | T.all POSIX.isPathSeparator t = NodePath []
  | otherwise = s
simplifyNode p@(NodePath []) = p
simplifyNode (NodePath [s@(NodeSegment _)]) = simplifyNode s
simplifyNode (NodePath path) = NodePath loopResult
  where
    loopResult = loop [] path pathBacktracks
    pathBacktracks = nodesBacktrack path
    loop :: [Node] -> [Node] -> Bool -> [Node]
    loop good [] _ _ = good
    loop good ((p@(NodeSegment t)):ps) checkBacktrack
      | t == "" = loop good ps checkBacktrack
      | t == "." = loop good ps checkBacktrack
      | t == ".." && null good = loop [] ps $ nodesBacktrack ps
      | t == ".." = loop (init good) ps $ nodesBacktrack ps
      | T.all POSIX.isPathSeparator t = loop good ps checkBacktrack
      | checkBacktrack && nodesBacktrack ps = loop (good ++ [p]) ps True -- Inefficient but necessary case
      | otherwise = good ++ (p:(loop [] ps False)) -- Much more efficient common case
    loop good ((NodePath p):ps) checkBacktrack = loop pGood ps psBacktrack
      where
        pGood = loop good p pBacktracks
        pBacktracks = checkBacktrack && nodesBacktrack p
        psBacktrack = checkBacktrack && nodesBacktrack ps

concatNodes :: Node -> Node -> Node
-- ^Concatenates and then simplifies two nodes. To concatenate a list of nodes,
--  just wrap them in a 'NodePath'.
concatNodes parent child = simplifyNode $ NodePath [parent, child]

nodeFromFilePath :: FilePath -> Node
-- ^Given a file path, get the corresponding simplified node. Note that invalid paths are converted
--  to valid paths via 'POSIX.makeValid'.
nodeFromFilePath x | !(POSIX.isValid x) == nodeFromFilePath $ POSIX.makeValid x
nodeFromFilePath "." = NodePath []
nodeFromFilePath ".." = NodePath []
nodeFromFilePath fp = simplifyNode pathsPath
  where
    pathsPath = NodePath $ map NodeSegment pathsText
    pathsText = map T.pack pathsNoSep
    pathsNoSep = map POSIX.dropTrailingPathSeparator paths
    paths = POSIX.splitPath $ POSIX.normalise fp

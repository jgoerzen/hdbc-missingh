{- arch-tag: HVFS instance helpers
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : MissingH.IO.HVFS.InstanceHelpers
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Utilities for creating instances of the items defined in
"MissingH.IO.HVFS".

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}

module MissingH.IO.HVFS.InstanceHelpers(-- * HVFSStat objects
                                        SimpleStat,
                                        -- * HVFS objects & types
                                        
                                       )
where
import MissingH.IO.HVFS
import Data.IORef

{- | A simple class that assumes that everything is either a file
or a directory. -}
data SimpleStat = SimpleStat {
                              isFile :: Bool -- ^ True if file, False if directory
                             } deriving (Show, Eq)
instance HVFSStat SimpleStat where
    vIsRegularFile x = isFile x
    vIsDirectory x = not (isFile x)

----------------------------------------------------------------------
-- In-Memory Tree Types
----------------------------------------------------------------------

type MemoryNode = (String, MemoryEntry)
data MemoryEntry = MemoryDirectory [MemoryNode]
                 | MemoryFile String
data MemoryVFS = MemoryVFS 
               { content :: IORef [MemoryNode],
                 cwd :: IORef String
               }

-- | Create a new 'MemoryVFS' object from an existing tree.
-- An empty filesystem may be created by using @[]@ for the parameter.
newMemoryVFS :: [MemoryNode] -> IO MemoryVFS
newMemoryVFS s = do r <- newIORef s
                    newMemoryVFSRef r

-- | Create a new 'MemoryVFS' object using an IORef to an
-- existing tree.
newMemoryVFSRef :: IORef [MemoryNode] -> IO MemoryVFS
newMemoryVFSRef r = do
                    c <- newIORef "/"
                    return (MemoryVFS {content = r, cwd = c})



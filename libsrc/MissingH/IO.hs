{- arch-tag: I/O utilities main file
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
   Module     : MissingH.IO
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with I\/O.

There are more functions in "MissingH.IO.Binary".

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.IO(-- * Entire File\/Handle Utilities
                       -- ** Opened Handle Data Copying
                       hCopy, hCopyProgress, hLineCopy, lineCopy,
                       -- ** Disk File Data Copying
                       copyFileLinesToFile,
                       -- * Line Processing Utilities
                       hPutStrLns, hGetLines,
                       -- * Lazy Interaction
                       -- ** Character-based
                       hInteract,
                       -- ** Line-based
                       hLineInteract, lineInteract,
                   -- * Optimizations
                   optimizeForBatch, optimizeForInteraction
                        ) where

import System.IO.Unsafe
import System.IO
import Data.List
import MissingH.IO.HVIO

{- | Given a list of strings, output a line containing each item, adding
newlines as appropriate.  The list is not expected to have newlines already.
-}

hPutStrLns :: HVIO a => a -> [String] -> IO ()
hPutStrLns h = mapM_ $ vPutStrLn h

{- | Given a handle, returns a list of all the lines in that handle.
Thanks to lazy evaluation, this list does not have to be read all at once.

Combined with 'hPutStrLns', this can make a powerful way to develop
filters.  See the 'lineInteract' function for more on that concept.

Example:

> main = do
>        l <- hGetLines stdin
>        hPutStrLns stdout $ filter (startswith "1") l

-}

-- FIXME does hGetContents h >>= return.lines not work?
hGetLines :: HVIO a => a -> IO [String]
hGetLines h = unsafeInterleaveIO (do
                                  ieof <- vIsEOF h
                                  if (ieof) 
                                     then return []
                                     else do
                                          line <- vGetLine h
                                          remainder <- hGetLines h
                                          return (line : remainder)
                                 )


{- | This is similar to the built-in 'System.IO.interact', but works
on any handle, not just stdin and stdout.

In other words:

> interact = hInteract stdin stdout
-}
hInteract :: Handle -> Handle -> (String -> String) -> IO ()
hInteract finput foutput func = do
                                content <- hGetContents finput
                                hPutStr foutput (func content)

{- | Line-based interaction.  This is similar to wrapping your
interact functions with 'lines' and 'unlines'.  This equality holds:

> lineInteract = hLineInteract stdin stdout

Here's an example:

> main = lineInteract (filter (startswith "1"))

This will act as a simple version of grep -- all lines that start with 1
will be displayed; all others will be ignored.
-}
lineInteract :: ([String] -> [String]) -> IO ()
lineInteract = hLineInteract stdin stdout

{- | Line-based interaction over arbitrary handles.  This is similar
to wrapping hInteract with 'lines' and 'unlines'.

One could view this function like this:

> hLineInteract finput foutput func = 
>     let newf = unlines . func . lines in
>         hInteract finput foutput newf

Though the actual implementation is this for efficiency:

> hLineInteract finput foutput func =
>     do
>     lines <- hGetLines finput
>     hPutStrLns foutput (func lines)
-}

hLineInteract :: (HVIO a, HVIO b) => a -> b -> ([String] -> [String]) -> IO ()
hLineInteract finput foutput func =
    do
    lines <- hGetLines finput
    hPutStrLns foutput (func lines)

{- | Copies from one handle to another in raw mode (using
hGetContents).
-}
hCopy :: (HVIO a, HVIO b) => a -> b -> IO ()
hCopy hin hout = do
                 c <- vGetContents hin
                 vPutStr hout c

{- | Copies from one handle to another in raw mode (using hGetContents).
Takes a function to provide progress updates to the user.
-}

hCopyProgress :: Integral a => Handle        -- ^ Input handle
                 -> Handle              -- ^ Output handle
                 -> (Maybe a -> Integer -> Bool -> IO ()) -- ^ Progress function -- the bool is always False unless this is the final call
                 -> Int                 -- Block size
                 -> Maybe a             -- Estimated file size (passed to func)
                 -> IO Integer                -- Number of bytes copied
hCopyProgress hin hout func bsize estsize =
    let copyFunc :: String -> Integer -> IO Integer
        copyFunc [] count = return count
        copyFunc indata count =
            let block = take bsize indata
                remainder = drop bsize indata
                newcount = count + (genericLength block)
                in
                do
                hPutStr hout block
                func estsize count False
                copyFunc remainder newcount
        in
        do
        c <- hGetContents hin
        bytes <- copyFunc c 0
        func estsize bytes True
        return bytes

{- | Copies from one handle to another in text mode (with lines).
Like 'hBlockCopy', this implementation is nice:

> hLineCopy hin hout = hLineInteract hin hout id
-}

hLineCopy :: Handle -> Handle -> IO()
hLineCopy hin hout = hLineInteract hin hout id

{- | Copies from 'stdin' to 'stdout' using lines.  An alias for 'hLineCopy'
over 'stdin' and 'stdout'. -}

lineCopy :: IO ()
lineCopy = hLineCopy stdin stdout

{- | Copies one filename to another in text mode.

Please note that the Unix permission bits are set at a default; you may
need to adjust them after the copy yourself.

This function is implemented using 'hLineCopy' internally. -}

copyFileLinesToFile :: FilePath -> FilePath -> IO ()
copyFileLinesToFile infn outfn = do
                                 hin <- openFile infn ReadMode
                                 hout <- openFile outfn WriteMode
                                 hLineCopy hin hout
                                 hClose hin
                                 hClose hout
                                 return ()

{- | Sets stdin and stdout to be block-buffered.  This can save a huge amount
of system resources since far fewer syscalls are made, and can make programs
run much faster. -}
optimizeForBatch :: IO ()
optimizeForBatch = do
                   hSetBuffering stdin (BlockBuffering (Just 4096))
                   hSetBuffering stdout (BlockBuffering (Just 4096))

{- | Sets stdin and stdout to be line-buffered.  This saves resources
on stdout, but not many on stdin, since it it still looking for newlines.
-}
optimizeForInteraction :: IO ()
optimizeForInteraction = do
                         hSetBuffering stdin LineBuffering
                         hSetBuffering stdout LineBuffering

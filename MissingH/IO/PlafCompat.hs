{-# LANGUAGE CPP #-}
{- Platform Compatibility Layer
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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
   Module     : MissingH.IO.PlafCompat
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

On Unix, exports System.Posix.Types and System.Posix.Files.

On Windows, exports System.Posix.Types and "MissingH.IO.WindowsCompat".

The result should be roughly the same set of defined variables and types.

-}

module MissingH.IO.PlafCompat
    (nullFileName,
#ifdef mingw32_HOST_OS
     module MissingH.IO.WindowsCompat,
#else
     module System.Posix.Files,
#endif
     module System.Posix.Types)
where

import System.Posix.Types
#ifdef mingw32_HOST_OS
import MissingH.IO.WindowsCompat
#else
import System.Posix.Files
#endif

{- | The name of the null device.  NUL: on Windows, \/dev\/null everywhere else.
-}

nullFileName :: String
#ifdef mingw32_HOST_OS
nullFileName = "NUL:"
#else
nullFileName = "/dev/null"
#endif

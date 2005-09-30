{-# LANGUAGE CPP #-}
{-
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
   Module     : MissingH.Daemon
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen\@complete.org
   Stability  : provisional
   Portability: portable to platforms with POSIX process\/signal tools

Tools for writing daemons\/server processes

Written by John Goerzen, jgoerzen\@complete.org

Please note: Most of this module is not compatible with Hugs.

Messages from this module are logged under @MissingH.Daemon@.  See
'MissingH.Logging.Logger' for details.

Based on background
from <http://www.erlenstar.demon.co.uk/unix/faq_2.html#SEC16> and
<http://www.haskell.org/hawiki/HaskellUnixDaemon>.

This module is not available on Windows.
-}

module MissingH.Daemon (

#ifndef mingw32_HOST_OS
        detachDaemon
#endif
		   )
where
#ifndef mingw32_HOST_OS

import System.Posix.Process
import System.Posix.IO
import System.Directory
import MissingH.Logging.Logger
import System.Exit


trap = traplogging "MissingH.Daemon" ERROR "detachDaemon"

{- | Detach the process from a controlling terminal and run it in the
background, handling it with standard Unix deamon semantics.

After running this, please note the following side-effects:

 * The PID of the running process will change

 * stdin, stdout, and stderr will not work (they'll be set to 
   \/dev\/null)

 * CWD will be changed to \/

I /highly/ suggest running this function before starting any threads.

Note that this is not intended for a daemon invoked from inetd(1).
-}

detachDaemon :: IO ()
detachDaemon = trap $ 
               do forkProcess child1 
                  exitImmediately ExitSuccess

child1 :: IO ()
child1 = trap $
    do createSession
       forkProcess child2
       exitImmediately ExitSuccess

child2 :: IO ()
child2 = trap $
    do setCurrentDirectory "/"
       mapM_ closeFd [stdInput, stdOutput, stdError]
       nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
       mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
       closeFd nullFd


#endif

{- arch-tag: Generic Server Support
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
   Module     : MissingH.Network.SocketServer
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : experimental
   Portability: systems with networking

This module provides an infrastructure to simplify server design.

Written by John Goerzen, jgoerzen\@complete.org

Please note: this module is designed to work with TCP, UDP, and Unix domain
sockets, but only TCP sockets have been tested to date.
-}

module MissingH.Network.SocketServer(-- * Generic Options and Types
                                     InetServerOptions(..),
                                     simpleTCPOptions,
                                     SocketServer,
                                     HandlerT,
                                     -- * TCP server convenient setup
                                     serveTCPforever,
                                     -- * Lower-Level Processing
                                     setupSocketServer,
                                     handleOne,
                                     serveForever,
                                     -- * Combinators
                                     loggingHandler,
                                     threadedHandler,
                                     handleHandler
                                    )
where
import Network.Socket
import Network.BSD
import MissingH.Network
import Control.Concurrent
import System.IO
import qualified MissingH.Logging.Logger

{- | Options for your server. -}
data InetServerOptions  = InetServerOptions {listenQueueSize :: Int,
                                             portNumber :: Int,
                                             interface :: HostAddress,
                                             reuse :: Bool,
                                             family :: Family,
                                             sockType :: SocketType,
                                             protoStr :: String
                                            }
    deriving (Eq, Show)

type HandlerT = Socket                  -- ^ The socket to use for communication
              -> SockAddr               -- ^ Address of the remote
              -> SockAddr               -- ^ Local address
              -> IO ()
                     
{- | Get Default options.  You can always modify it later. -}
simpleTCPOptions :: Int                -- ^ Port Number
                 -> InetServerOptions
simpleTCPOptions p = InetServerOptions {listenQueueSize = 5,
                                        portNumber = p,
                                        interface = iNADDR_ANY,
                                        reuse = False,
                                        family = AF_INET,
                                        sockType = Stream,
                                        protoStr = "tcp"
                                       }

data SocketServer = SocketServer {options :: InetServerOptions,
                                  sock :: Socket}
                  deriving (Eq, Show)

{- | Takes some options and sets up the 'SocketServer'.  I will bind
and begin listening, but will not accept any connections itself. -}
setupSocketServer :: InetServerOptions -> IO SocketServer
setupSocketServer opts =
    do proto <- getProtocolNumber (protoStr opts)
       s <- socket (family opts) (sockType opts) proto
       setSocketOption s ReuseAddr (case (reuse opts) of
                                    True -> 1
                                    False -> 0)
       bindSocket s (SockAddrInet (fromIntegral (portNumber opts)) 
                     (interface opts))
       listen s (listenQueueSize opts)
       return $ SocketServer {options = opts, sock = s}
       
{- | Handle one incoming request from the given 'SocketServer'. -}
handleOne :: SocketServer -> HandlerT -> IO ()
handleOne ss func =
    let opts = (options ss)
        in    do a <- accept (sock ss)
                 localaddr <- getSocketName (fst a)
                 func (fst a) (snd a) localaddr
    
{- | Handle all incoming requests from the given 'SocketServer'. -}
serveForever :: SocketServer -> HandlerT -> IO ()
serveForever ss func =
    sequence_ (repeat (handleOne ss func))

{- | Convenience function to completely set up a TCP
'SocketServer' and handle all incoming requests.

This function is literally this:

>serveTCPforever options func =
>    do sockserv <- setupSocketServer options
>       serveForever sockserv func
 -}
serveTCPforever :: InetServerOptions     -- ^ Server options
                -> HandlerT              -- ^ Handler function
                -> IO ()                
serveTCPforever options func =
    do sockserv <- setupSocketServer options
       serveForever sockserv func

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

{- | Log each incoming connection using the interface in
"MissingH.Logging.Logger".

Log when the incoming connection disconnects.

Also, log any failures that may occur in the child handler. -}

loggingHandler :: String                -- ^ Name of logger to use
               -> MissingH.Logging.Logger.Priority -- ^ Priority of logged messages
               -> HandlerT              -- ^ Handler to call after logging
               -> HandlerT              -- ^ Resulting handler
loggingHandler hname prio nexth socket r_sockaddr l_sockaddr = 
    do sockStr <- showSockAddr r_sockaddr
       MissingH.Logging.Logger.logM hname prio 
                   ("Received connection from " ++ sockStr)
       MissingH.Logging.Logger.traplogging hname 
               MissingH.Logging.Logger.WARNING "" (nexth socket r_sockaddr 
                                                   l_sockaddr)
       MissingH.Logging.Logger.logM hname prio
                   ("Connection " ++ sockStr ++ " disconnected")
       

-- | Handle each incoming connection in its own thread to
-- make the server multi-tasking.
threadedHandler :: HandlerT             -- ^ Handler to call in the new thread
                -> HandlerT             -- ^ Resulting handler
threadedHandler nexth socket r_sockaddr l_sockaddr=
    do forkIO (nexth socket r_sockaddr l_sockaddr)
       return ()

{- | Give your handler function a Handle instead of a Socket.

The Handle will be opened with ReadWriteMode (you use one handle for both
directions of the Socket).  Also, it will be initialized with LineBuffering.

Unlike other handlers, the handle will be closed when the function returns.
Therefore, if you are doing threading, you should to it before you call this
handler.
-}
handleHandler :: (Handle -> SockAddr -> SockAddr -> IO ())      -- ^ Handler to call
              -> HandlerT
handleHandler func socket r_sockaddr l_sockaddr = 
    do h <- socketToHandle socket ReadWriteMode
       hSetBuffering h LineBuffering
       func h r_sockaddr l_sockaddr
       hClose h

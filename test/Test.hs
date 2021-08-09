{-# LANGUAGE OverloadedStrings, ExplicitForAll, TypeApplications #-}
module Main (main) where

import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket, catchJust, finally)
import Control.Monad (when, void, guard)

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
--import Data.ByteString.Char8 (pack)
import Data.Foldable (traverse_, asum)
import Data.Maybe (fromJust)

--import System.Exit (die)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files.ByteString (removeLink)
import System.Posix.IO.ByteString (createPipe, fdRead, fdWrite, closeFd)

import Test.Tasty
import Test.Tasty.HUnit

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Family.Unix
import System.Socket.Protocol.Default
import System.Socket.Protocol.TCP
import System.Socket.Protocol.UDP
import System.Socket.Type.Datagram
import System.Socket.Type.Stream

import System.Socket.Msg
import System.Socket.Ancillary.Fds

{-
main :: IO ()
main = bracket
  ( do
    server <- socket :: IO (Socket Inet Stream TCP)
    client <- socket :: IO (Socket Inet Stream TCP)
    return (server, client)
  )
  ( \(server,client)-> do
    close server
    close client
  )
  ( \(server,client)-> do
    let addr = SocketAddressInet inetLoopback port
    let helloWorld = pack "Hello world!"
    setSocketOption server (ReuseAddress True)
    bind server addr
    listen server 5
    serverRecv <- async $ do
      (peerSock, peerAddr) <- accept server
      receiveMsg peerSock 4096 [] mempty
    connect client addr
    sendMsg client [BS.take 6 helloWorld, BS.drop 6 helloWorld] [] mempty
    (msg, _, _) <- wait serverRecv
    when (msg /= helloWorld) (die "Received message was bogus.")
  )
-}

main :: IO ()
main = defaultMain $ testGroup "sendmsg/recvmsg"
  [ inband
  , passingFd
  ]

inband :: TestTree
inband = testGroup "inband"
  [ testCase "Inet/Stream/TCP" $ do
      withSockets' @Inet @Stream @TCP $ \socks@(server, client) -> do
        setSocketOption server (ReuseAddress True)
        void $ testServerClientStream [] [] inetAddr socks
  , testCase "Inet/Datagram/UDP" $ do
      withSockets' @Inet @Datagram @UDP $ \socks@(server, client) -> do
        setSocketOption server (ReuseAddress True)
        void $ testServerClientDatagram [] [] inetAddr socks
  , testCase "Unix/Stream/Default" $ do
      withSockets @Unix @Stream @Default (unlink_ unixPath) $ void . testServerClientStream [] [] unixAddr
  , testCase "Unix/Datagram/Default" $ do
      withSockets @Unix @Datagram @Default (unlink_ unixPath *> unlink_ unixClientPath) $ \socks@(server, client) -> do
        bind client unixClientAddr
        void $ testServerClientDatagram [] [] unixAddr socks
  ]

passingFd :: TestTree
passingFd = testGroup "fd"
  [ testCase "Unix/Stream/Default:pipe" $
      bracket createPipe (both closeFd) $ \(rpipe, wpipe) -> do
        cmsgs <- withSockets @Unix @Stream @Default (unlink_ unixPath) $ testServerClientStream [expectFds 1] [toControlMsg (Fds [rpipe])] unixAddr
        case asum $ map fromControlMsg cmsgs of
          Nothing -> assertFailure "No Fd ControlMsg received"
          Just (Fds []) -> assertFailure "Empty Fd ControlMsg received"
          Just (Fds [recvFd]) -> flip finally (closeFd recvFd) $ do
            n <- fdWrite wpipe pipeMessage
            pipeRead <- fdRead recvFd n
            pipeRead @?= (pipeMessage, n)
  , testCase "Unix/Datagram/Default:pipe" $
      bracket createPipe (both closeFd) $ \(rpipe, wpipe) -> do
        cmsgs <- withSockets @Unix @Datagram @Default (unlink_ unixPath *> unlink_ unixClientPath) $ \socks@(server, client) -> do
          bind client unixClientAddr
          testServerClientDatagram [expectFds 1] [toControlMsg (Fds [rpipe])] unixAddr socks
        case asum $ map fromControlMsg cmsgs of
          Nothing -> assertFailure "No Fd ControlMsg received"
          Just (Fds []) -> assertFailure "Empty Fd ControlMsg received"
          Just (Fds [recvFd]) -> flip finally (closeFd recvFd) $ do
            n <- fdWrite wpipe pipeMessage
            pipeRead <- fdRead recvFd n
            pipeRead @?= (pipeMessage, n)
  , testCase "Stream:cmsg-location" $
      bracket createPipe (both closeFd) $ \(rpipe, wpipe) -> do
        withSockets @Unix @Stream @Default (unlink_ unixPath) $ \(server, client) -> do
          bind server unixAddr
          listen server 5
          serverRecv <- async $ do
            (peerSock, peerAddr) <- accept server
            r <- receiveMsg peerSock 4096 [] mempty
            sendMsg peerSock serverMessage [toControlMsg $ Fds [rpipe, wpipe]] mempty
            pure r
          connect client unixAddr
          sendMsg client clientMessage [] mempty

          (clientMessageReceived, _, _) <- wait serverRecv
          (header, cmsgs, _) <- receiveMsg client 4 [expectFds 2] mempty
          (body, _, _) <- receiveMsg client 4096 [] mempty
--          when (hflags .&. msgControlTruncated /= mempty) $ assertFailure "ControlMsg lost on receiving header"
          header <> body @?= mconcat serverMessage
          case asum $ map fromControlMsg cmsgs of
            Nothing -> assertFailure "No Fd ControlMsg received"
            Just (Fds []) -> assertFailure "Empty Fd ControlMsg received"
            Just (Fds fds@[_,_]) -> traverse_ closeFd fds
  ]

both :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
both f (x, y) = (,) <$> f x <*> f y

withSockets :: forall f t p a b. (Family f, Type t, Protocol p) => IO b -> ((Socket f t p, Socket f t p) -> IO a) -> IO a
withSockets teardown f = bracket
  ((,) <$> socket <*> socket)
  (\ss -> both close ss *> teardown)
  f

withSockets' :: forall f t p a. (Family f, Type t, Protocol p) => ((Socket f t p, Socket f t p) -> IO a) -> IO a
withSockets' = withSockets (pure ())

testServerClientStream :: Family f => [ExpectControl f] -> [ControlMsg f] -> SocketAddress f -> (Socket f Stream p, Socket f Stream p) -> IO [ControlMsg f]
testServerClientStream expects cmsgs addr (server, client) = do
  bind server addr
  listen server 5
  serverRecv <- async $ do
    (peerSock, peerAddr) <- accept server
    r <- receiveMsg peerSock 4096 [] mempty
    sendMsg peerSock serverMessage cmsgs mempty
    pure r
  connect client addr
  sendMsg client clientMessage [] mempty

  (clientMessageReceived, _, _) <- wait serverRecv
  (serverMessageReceived, cmsgsReceived, _) <- receiveMsg client 4096 expects mempty
  clientMessageReceived @?= BS.concat clientMessage
  serverMessageReceived @?= BS.concat serverMessage
  pure cmsgsReceived

testServerClientDatagram :: Family f => [ExpectControl f] -> [ControlMsg f] -> SocketAddress f -> {-SocketAddress f ->-} (Socket f Datagram p, Socket f Datagram p) -> IO [ControlMsg f]
testServerClientDatagram expects cmsgs sAddr {-cAddr-} (server, client) = do
  bind server sAddr
--  bind client cAddr
  serverRecv <- async $ do
    r@(_, _, peerAddr, _) <- receiveMsgFrom server 4096 [] mempty
    sendMsgTo server serverMessage cmsgs mempty peerAddr
    pure r
  sendMsgTo client clientMessage [] mempty sAddr

  (clientMessageReceived, _, _, _) <- wait serverRecv
  (serverMessageReceived, cmsgsReceived, _) <- receiveMsg client 4096 expects mempty
  clientMessageReceived @?= BS.concat clientMessage
  serverMessageReceived @?= BS.concat serverMessage
  pure cmsgsReceived

clientMessage, serverMessage :: [ByteString]
clientMessage = ["client", " ", "message"]
serverMessage = ["server", " ", "message"]

pipeMessage :: String
pipeMessage = "pipe message"

port = 39000

inetAddr = SocketAddressInet inetLoopback port

unixPath :: ByteString
unixPath = "Woum5ag3oohuaLee.socket"

unixAddr :: SocketAddress Unix
unixAddr = fromJust $ socketAddressUnixPath unixPath

unixClientPath :: ByteString
unixClientPath = "Io4meo0epoquashi.socket"

unixClientAddr :: SocketAddress Unix
unixClientAddr = fromJust $ socketAddressUnixPath unixClientPath

unlink_ path = catchJust (guard . isDoesNotExistError) (removeLink path) (\_ -> pure ())


module System.Socket.Msg
  ( sendMsg
  , sendMsgTo
  , receiveMsg
  , receiveMsgFrom
  , ControlMsg
  , ExpectControl
  , ControlMsgData (toControlMsg, fromControlMsg)
  , msgTruncated
  , msgControlTruncated
  ) where

import Data.ByteString (ByteString)

import Foreign.Marshal (alloca, allocaBytes, with, fillBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

import System.Socket (Socket, SocketAddress, Family, MessageFlags)

import System.Socket.Msg.Internal
import System.Socket.Msg.Unsafe

sendMsg :: Socket f t p -> [ByteString] -> [ControlMsg f] -> MessageFlags -> IO Int
sendMsg s bss cmsgs flags =
  fmap fromIntegral . unsafeUseByteStringsAsIovec bss $ \vecptr ->
  let msghdr = defaultMsghdr {
        msg_iov = vecptr,
        msg_iovlen = fromIntegral $ length bss
      }
   in withMsghdrCmsgs msghdr cmsgs $ \msgptr ->
      unsafeSendMsg s msgptr flags

sendMsgTo :: Family f => Socket f t p -> [ByteString] -> [ControlMsg f] -> MessageFlags -> SocketAddress f -> IO Int
sendMsgTo s bss cmsgs flags addr =
  fmap fromIntegral . unsafeUseByteStringsAsIovec bss $ \vecptr ->
  with addr $ \addrptr ->
  let msghdr = defaultMsghdr {
        msg_name = castPtr addrptr,
        msg_namelen = fromIntegral $ sizeOf addr,
        msg_iov = vecptr,
        msg_iovlen = fromIntegral $ length bss
      }
   in withMsghdrCmsgs msghdr cmsgs $ \msgptr ->
      unsafeSendMsg s msgptr flags

receiveMsg :: Socket f t p -> Int -> [ExpectControl f] -> MessageFlags -> IO (ByteString, [ControlMsg f], MessageFlags)
receiveMsg s bufSize expects flags =
  alloca $ \msgptr ->
  let cmsgBufLen = cmsgBufferSpace $ map unExpect expects
   in allocaBytes (fromIntegral cmsgBufLen) $ \cmsgBuf -> do
        fillBytes cmsgBuf 0 (fromIntegral cmsgBufLen)
        bs <- createByteStringUptoNAsIovec bufSize $ \vec ->
          with vec $ \vecptr -> do
            let msghdr = defaultMsghdr {
                  msg_iov = vecptr,
                  msg_iovlen = 1,
                  msg_control = cmsgBuf,
                  msg_controllen = cmsgBufLen
                }
            poke msgptr msghdr
            fromIntegral <$> unsafeReceiveMsg s msgptr flags
        msghdr <- peek msgptr
        cmsgs <- peekCmsgs msgptr
        pure (bs, cmsgs, msg_flags msghdr)

receiveMsgFrom :: Family f => Socket f t p -> Int -> [ExpectControl f] -> MessageFlags -> IO (ByteString, [ControlMsg f], SocketAddress f, MessageFlags)
receiveMsgFrom s bufSize expects flags =
  alloca $ \msgptr ->
  alloca $ \addrptr ->
  let cmsgBufLen = cmsgBufferSpace $ map unExpect expects
   in allocaBytes (fromIntegral cmsgBufLen) $ \cmsgBuf -> do
        bs <- createByteStringUptoNAsIovec bufSize $ \vec ->
          with vec $ \vecptr -> do
            let msghdr = defaultMsghdr {
                  msg_name = castPtr addrptr,
                  msg_namelen = fromIntegral $ sizeOf' addrptr,
                  msg_iov = vecptr,
                  msg_iovlen = 1,
                  msg_control = cmsgBuf,
                  msg_controllen = cmsgBufLen
                }
            poke msgptr msghdr
            fromIntegral <$> unsafeReceiveMsg s msgptr flags
        msghdr <- peek msgptr
        addr <- peek addrptr
        cmsgs <- peekCmsgs msgptr
        pure (bs, cmsgs, addr, msg_flags msghdr)

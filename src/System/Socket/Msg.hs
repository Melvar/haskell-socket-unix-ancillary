-- |
-- Stability   :  experimental
-- Portability :  Unix
--
-- Wrappers for control messages (ancillary data) and send/receive functions which handle them.
--
-- Support for specific control message types can be added by supplying an instance of `ControlMsgData` for a type representing that control message type and a function which constructs expectations for it.
module System.Socket.Msg (
  -- * Sending and receiving
    sendMsg
  , sendMsgTo
  , receiveMsg
  , receiveMsgFrom
  -- * Control messages
  , ControlMsg
  , ExpectControl
  , ControlMsgData (toControlMsg, fromControlMsg)
  -- * Message flags
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

-- | Like `System.Socket.send`, except:
--
--   - Multiple `Data.ByteString.ByteString`s can be sent in one call without copying.
--   - Control messages (ancillary data) can be sent with the message. Note that sending at least one byte of regular data may be required to do this, depending on the socket type and type of control message.
--   - The underlying system call, which determines the possible `System.Socket.SocketException`s, is @sendmsg@.
sendMsg :: Socket f t p -> [ByteString] -> [ControlMsg f] -> MessageFlags -> IO Int
sendMsg s bss cmsgs flags =
  fmap fromIntegral . unsafeUseByteStringsAsIovec bss $ \vecptr ->
  let msghdr = defaultMsghdr {
        msg_iov = vecptr,
        msg_iovlen = fromIntegral $ length bss
      }
   in withMsghdrCmsgs msghdr cmsgs $ \msgptr ->
      unsafeSendMsg s msgptr flags

-- | `sendMsgTo` is to `sendMsg` as `System.Socket.sendTo` is to `System.Socket.send`.
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

-- | Like `System.Socket.receive`, except:
--
--   - Control messages (ancillary data) may be received. To do this, a list of control message expectations must be passed in to size the control message buffer. There is no guarantee that the control messages actually received correspond to these expectations, however.
--   - A set of `System.Socket.MessageFlags` is returned as well. Consult @man recv@ for the possible values.
--   - The underlying system call, which determines the possible `System.Socket.SocketException`s, is @recvmsg@.
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

-- | `receiveMsgFrom` is to `receiveMsg` as `System.Socket.receiveFrom` is to `System.Socket.receive`.
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

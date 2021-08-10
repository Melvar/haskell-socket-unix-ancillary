-- |
-- Stability   :  experimental
-- Portability :  Unix
module System.Socket.Msg.Unsafe (
  -- * Unsafe send and receive
    unsafeSendMsg
  , unsafeReceiveMsg
  -- * Types necessary for the C structs
  , Msghdr (..)
  , Cmsghdr (..)
  , Iovec (..)
  , CSocklen
  ) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
--import System.Posix.Types (CSsize)

import System.Socket (Socket, MessageFlags)
import System.Socket.Unsafe (tryWaitRetryLoop, waitRead, waitWrite)

import System.Socket.Msg.Platform

-- The `socket` library uses `CInt` for the return type instead of `CSsize`, reason unknown. I convert here to be able to use `tryWaitRetryLoop`. It should not cause problems unless a single message is 2 GiB in size, which seems very unlikely.
-- | Like `System.Socket.Msg.sendMsg` or `System.Socket.Msg.sendMsgTo`, but using a pointer to a @struct msghdr@ instead of bytestrings and wrapper types.
--
-- This function is /unsafe/. All the buffers pointed to from the @struct msghdr@ must be at least as big as indicated by their accompanying sizes.
unsafeSendMsg :: Socket f t p -> Ptr Msghdr -> MessageFlags -> IO CInt
unsafeSendMsg s msgPtr flags =
  tryWaitRetryLoop s waitWrite $ \fd -> fmap fromIntegral . c_sendmsg fd msgPtr flags

-- | Like `System.Socket.Msg.receiveMsg` or `System.Socket.Msg.receiveMsgFrom`, but using a pointer to a @struct msghdr@ instead of bytestrings and wrapper types.
--
-- This function is /unsafe/. All the buffers pointed to from the @struct msghdr@ must be at least as big as indicated by their accompanying sizes.
unsafeReceiveMsg :: Socket f t p -> Ptr Msghdr -> MessageFlags -> IO CInt
unsafeReceiveMsg s msgPtr flags =
  tryWaitRetryLoop s waitRead $ \fd -> fmap fromIntegral . c_recvmsg fd msgPtr flags


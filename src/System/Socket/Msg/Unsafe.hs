module System.Socket.Msg.Unsafe
  ( unsafeSendMsg
  , unsafeReceiveMsg
  ) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
--import System.Posix.Types (CSsize)

import System.Socket (Socket, MessageFlags)
import System.Socket.Unsafe (tryWaitRetryLoop, waitRead, waitWrite)

import System.Socket.Msg.Platform

-- The `socket` library uses `CInt` for the return type instead of `CSsize`, reason unknown. I convert here to be able to use `tryWaitRetryLoop`. It should not cause problems unless a single message is 2 GiB in size, which seems very unlikely.
unsafeSendMsg :: Socket f t p -> Ptr Msghdr -> MessageFlags -> IO CInt
unsafeSendMsg s msgPtr flags =
  tryWaitRetryLoop s waitWrite $ \fd -> fmap fromIntegral . c_sendmsg fd msgPtr flags

unsafeReceiveMsg :: Socket f t p -> Ptr Msghdr -> MessageFlags -> IO CInt
unsafeReceiveMsg s msgPtr flags =
  tryWaitRetryLoop s waitRead $ \fd -> fmap fromIntegral . c_recvmsg fd msgPtr flags


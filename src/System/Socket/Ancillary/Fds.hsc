-- |
-- Stability   :  experimental
-- Portability :  Unix
--
-- Serializing and deserializing file descriptors as control messages for transfer over unix domain sockets.
module System.Socket.Ancillary.Fds
  ( expectFds
  , fdMsg
  , fromFdMsg
  , msgControlCloseOnExec
  ) where

import Data.ByteString.Internal (unsafeCreate)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

import Foreign.Marshal (pokeArray, peekArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

import System.IO.Unsafe (unsafePerformIO)

import System.Posix.Types (Fd (..))

import System.Socket (MessageFlags (..))
import System.Socket.Family.Unix (Unix)

import System.Socket.Msg.ControlMsg
import System.Socket.Msg.Internal (sizeOf')

#include <sys/socket.h>

-- | Construct an expectation to receive up to this number of `System.Posix.Types.Fd`s.
expectFds :: Int -> ExpectControl Unix
expectFds n = Expect $ n * sizeOf (undefined :: Fd)

-- | Construct a control message containing `System.Posix.Types.Fd`s.
fdMsg :: [Fd] -> ControlMsg Unix
fdMsg fds = ControlMsg {
  cMsgLevel = (#const SOL_SOCKET),
  cMsgType = (#const SCM_RIGHTS),
  cMsgPayload = unsafeCreate (length fds * sizeOf' fds) $ \ptr ->
    pokeArray (castPtr ptr) fds
}

-- | If the control message is a file descriptor message, extract the `System.Posix.Types.Fd`s.
fromFdMsg :: ControlMsg Unix -> Maybe [Fd]
fromFdMsg (ControlMsg level ty payload)
  | level /= (#const SOL_SOCKET) || ty /= (#const SCM_RIGHTS) = Nothing
  | otherwise =
      unsafePerformIO . unsafeUseAsCStringLen payload $ \(ptr, len) ->
        fmap Just $ peekArray (len `quot` sizeOf (undefined :: Fd)) (castPtr ptr)

-- | Causes any received file descriptors to have their close-on-@exec@ flag set. This has the same use as other system call flags which cause the close-on-@exec@ flag to be set as soon as the file descriptor is created, such as @O_CLOEXEC@ for @open@.
msgControlCloseOnExec :: MessageFlags
msgControlCloseOnExec = MessageFlags (#const MSG_CMSG_CLOEXEC)


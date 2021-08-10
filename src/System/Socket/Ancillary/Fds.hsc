-- |
-- Stability   :  experimental
-- Portability :  Unix
--
-- Serializing and deserializing file descriptors as control messages for transfer over unix domain sockets.
{-# LANGUAGE MultiParamTypeClasses #-}
module System.Socket.Ancillary.Fds
  ( Fds (..)
  , expectFds
  , ControlMsgData (..)
  ) where

import Data.ByteString.Internal (unsafeCreate)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

import Foreign.Marshal (pokeArray, peekArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

import System.IO.Unsafe (unsafePerformIO)

import System.Posix.Types (Fd (..))

import System.Socket.Family.Unix (Unix)

import System.Socket.Msg.Internal (ControlMsg (..), ControlMsgData (..), ExpectControl (..), sizeOf')

#include <sys/socket.h>

-- | A list of `System.Posix.Types.Fd`s to transfer in a control message.
newtype Fds = Fds [Fd] deriving (Eq, Show)

-- | Construct an expectation to receive up to this number of `System.Posix.Types.Fd`s.
expectFds :: Int -> ExpectControl Unix
expectFds n = Expect $ n * sizeOf (undefined :: Fd)

instance ControlMsgData Unix Fds where
  toControlMsg (Fds fds) = ControlMsg {
    cMsgLevel = (#const SOL_SOCKET),
    cMsgType = (#const SCM_RIGHTS),
    cMsgPayload = unsafeCreate (length fds * sizeOf' fds) $ \ptr ->
      pokeArray (castPtr ptr) fds
  }
  fromControlMsg (ControlMsg level ty payload)
    | level /= (#const SOL_SOCKET) || ty /= (#const SCM_RIGHTS) = Nothing
    | otherwise = 
        unsafePerformIO . unsafeUseAsCStringLen payload $ \(ptr, len) ->
          fmap (Just . Fds) $ peekArray (len `quot` sizeOf (undefined :: Fd)) (castPtr ptr)


{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module System.Socket.Msg.Platform
  ( CSocklen (..)
  , Iovec (..)
  , Msghdr (..)
  , Cmsghdr (..)
  , msgTruncated
  , msgControlTruncated
  , c_sendmsg
  , c_recvmsg
  , c_cmsg_firsthdr
  , c_cmsg_nxthdr
  , c_cmsg_space
  , c_cmsg_len
  , c_cmsg_data
  ) where

import Data.Bits
import Data.Word (Word32)

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types (Fd (..), CSsize (..))

import System.Socket (MessageFlags (..))

#include "hs_msg.h"

-- in System.Posix.Types only since base 4.14.0.0
newtype CSocklen = CSocklen (#type socklen_t)
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, FiniteBits, Bits, Storable)

data Iovec = Iovec { iov_base :: Ptr (), iov_len :: CSize } deriving (Show)

instance Storable Iovec where
  sizeOf _ = (#size struct iovec)
  alignment _ = (#alignment struct iovec)
  peek ptr = Iovec <$> (#peek struct iovec, iov_base) ptr <*> (#peek struct iovec, iov_len) ptr
  poke ptr (Iovec base len) = (#poke struct iovec, iov_base) ptr base *> (#poke struct iovec, iov_len) ptr len

data Msghdr =
  Msghdr {
    msg_name       :: Ptr (),
    msg_namelen    :: CSocklen,
    msg_iov        :: Ptr Iovec,
    msg_iovlen     :: CSize,
    msg_control    :: Ptr (),
    msg_controllen :: CSize,
    msg_flags      :: MessageFlags -- only to return flags from recvmsg – to pass in flags there’s an arg on each syscall
  } deriving (Show)

instance Storable Msghdr where
  sizeOf _ = (#size struct msghdr)
  alignment _ = (#alignment struct msghdr)

  peek ptr = do
    msg_name <- (#peek struct msghdr, msg_name) ptr
    msg_namelen <- (#peek struct msghdr, msg_namelen) ptr
    msg_iov <- (#peek struct msghdr, msg_iov) ptr
    msg_iovlen <- (#peek struct msghdr, msg_iovlen) ptr
    msg_control <- (#peek struct msghdr, msg_control) ptr
    msg_controllen <- (#peek struct msghdr, msg_controllen) ptr
    msg_flags <- (#peek struct msghdr, msg_flags) ptr
    pure Msghdr {..}

  poke ptr Msghdr {..} = do
    (#poke struct msghdr, msg_name) ptr msg_name
    (#poke struct msghdr, msg_namelen) ptr msg_namelen
    (#poke struct msghdr, msg_iov) ptr msg_iov
    (#poke struct msghdr, msg_iovlen) ptr msg_iovlen
    (#poke struct msghdr, msg_control) ptr msg_control
    (#poke struct msghdr, msg_controllen) ptr msg_controllen
    (#poke struct msghdr, msg_flags) ptr msg_flags

data Cmsghdr =
  Cmsghdr {
    cmsg_len :: CSize,
    cmsg_level :: CInt,
    cmsg_type :: CInt
  } deriving (Show)

instance Storable Cmsghdr where
  sizeOf _ = (#size struct cmsghdr)
  alignment _ = (#alignment struct cmsghdr)

  peek ptr = do
    cmsg_len <- (#peek struct cmsghdr, cmsg_len) ptr
    cmsg_level <- (#peek struct cmsghdr, cmsg_level) ptr
    cmsg_type <- (#peek struct cmsghdr, cmsg_type) ptr
    pure Cmsghdr {..}

  poke ptr Cmsghdr {..} = do
    (#poke struct cmsghdr, cmsg_len) ptr cmsg_len
    (#poke struct cmsghdr, cmsg_level) ptr cmsg_level
    (#poke struct cmsghdr, cmsg_type) ptr cmsg_type

msgTruncated :: MessageFlags
msgTruncated = MessageFlags (#const MSG_TRUNC)

msgControlTruncated :: MessageFlags
msgControlTruncated = MessageFlags (#const MSG_CTRUNC)

foreign import ccall unsafe "hs_sendmsg"
  c_sendmsg :: Fd -> Ptr Msghdr -> MessageFlags -> Ptr CInt -> IO CSsize

foreign import ccall unsafe "hs_recvmsg"
  c_recvmsg :: Fd -> Ptr Msghdr -> MessageFlags -> Ptr CInt -> IO CSsize

-- cmsghdr manipulation macros
foreign import ccall unsafe "hs_cmsg_firsthdr"
  c_cmsg_firsthdr :: Ptr Msghdr -> IO (Ptr Cmsghdr)

foreign import ccall unsafe "hs_cmsg_nxthdr"
  c_cmsg_nxthdr :: Ptr Msghdr -> Ptr Cmsghdr -> IO (Ptr Cmsghdr)

foreign import ccall unsafe "hs_cmsg_space"
  c_cmsg_space :: CSize -> CSize

foreign import ccall unsafe "hs_cmsg_len"
  c_cmsg_len :: CSize -> CSize

foreign import ccall unsafe "hs_cmsg_data"
  c_cmsg_data :: Ptr Cmsghdr -> Ptr CChar


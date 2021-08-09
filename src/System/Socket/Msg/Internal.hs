{-# LANGUAGE MultiParamTypeClasses #-}
module System.Socket.Msg.Internal
  ( ControlMsg (..)
  , ExpectControl (..)
  , ControlMsgData (..)
  , defaultMsghdr
  , mkIovec
  , unsafeUseByteStringAsIovec
  , unsafeUseByteStringsAsIovec
  , createByteStringUptoNAsIovec
  , cmsgBufferSpace
  , peekCmsgs
  , pokeCmsgs
  , withMsghdrCmsgs
  , sizeOf'
  -- reexports:
  , Msghdr (..)
  , msgTruncated
  , msgControlTruncated
  ) where

import Control.Exception (throwIO)

import Data.ByteString (ByteString, packCStringLen)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (createUptoN)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

import Foreign.C.Types (CInt, CSize)
--import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, mallocForeignPtrBytes)
import Foreign.Marshal (allocaBytes, with, withMany, withArray, moveBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (Storable(..))

import System.Socket (Family (..))

import System.Socket.Msg.Platform

{-
data Msg f =
  Msg {
    msgAddress :: Maybe (SocketAddress f),
    msgContents :: [ByteString],
    msgControl :: [ControlMsg f]
  }
-}

data ControlMsg f =
  ControlMsg {
    cMsgLevel :: CInt,
    cMsgType :: CInt,
    cMsgPayload :: ByteString
--    cMsgPayloadLen :: Int
  }

newtype ExpectControl f = Expect { unExpect :: Int }

class Family f => ControlMsgData f a where
  toControlMsg :: a -> ControlMsg f
  fromControlMsg :: ControlMsg f -> Maybe a

defaultMsghdr :: Msghdr
defaultMsghdr =
  Msghdr {
    msg_name = nullPtr,
    msg_namelen = 0,
    msg_iov = nullPtr,
    msg_iovlen = 0,
    msg_control = nullPtr,
    msg_controllen = 0,
    msg_flags = mempty
  }

mkIovec :: Ptr a -> Int -> Iovec
mkIovec ptr len = Iovec (castPtr ptr) (fromIntegral len)

unsafeUseByteStringAsIovec :: ByteString -> (Iovec -> IO a) -> IO a
unsafeUseByteStringAsIovec bs f =
    unsafeUseAsCStringLen bs $ f . uncurry mkIovec

unsafeUseByteStringsAsIovec :: [ByteString] -> (Ptr Iovec -> IO a) -> IO a
unsafeUseByteStringsAsIovec bss f =
    withMany unsafeUseByteStringAsIovec bss $ \vecs ->
    withArray vecs f

createByteStringUptoNAsIovec :: Int -> (Iovec -> IO Int) -> IO ByteString
createByteStringUptoNAsIovec n f = createUptoN n $ \ptr -> f (mkIovec ptr n)

cmsgBufferSpace :: [Int] -> CSize
cmsgBufferSpace = sum . map (c_cmsg_space . fromIntegral)

peekCmsgs :: Ptr Msghdr -> IO [ControlMsg f]
peekCmsgs msgptr = do
    firsthdr <- c_cmsg_firsthdr msgptr
    go firsthdr
  where
    go hdrptr
      | hdrptr == nullPtr = pure []
      | otherwise = do
          cmsghdr <- peek hdrptr
          let dataptr = c_cmsg_data hdrptr
              len = fromIntegral (cmsg_len cmsghdr) - (dataptr `minusPtr` hdrptr) -- attempt to invert CMSG_LEN since no inversion is provided
--          payload <- mallocForeignPtrBytes len
--          withForeignPtr payload $ \payloadptr ->
--            moveBytes payloadptr dataptr len
          payload <- packCStringLen (dataptr, len)
          let cmsg = ControlMsg {
                cMsgLevel = cmsg_level cmsghdr,
                cMsgType = cmsg_type cmsghdr,
                cMsgPayload = payload
--                cMsgPayloadLength = len
              }
          nexthdr <- c_cmsg_nxthdr msgptr hdrptr
          fmap (cmsg :) $ go nexthdr

-- Takes the end of the control message buffer to check for buffer overflow
pokeCmsgs :: Ptr Msghdr -> Ptr () -> [ControlMsg f] -> IO ()
pokeCmsgs msgptr cmsgBufEnd allcmsgs = do
    firsthdr <- c_cmsg_firsthdr msgptr
    go firsthdr allcmsgs
  where
    go _ [] = pure ()
    go hdrptr (ControlMsg level ty payload : cmsgs)
      | hdrptr == nullPtr = throwIO $ userError "writeCmsgs: Control Message Buffer too small for header"
      | otherwise =
          let dataptr = c_cmsg_data hdrptr
           in if cmsgBufEnd `minusPtr` dataptr < BS.length payload
                 then throwIO $ userError "writeCmsgs: Control Message Buffer too small for data"
                 else do
                   unsafeUseAsCStringLen payload $ \(payloadptr, len) -> do
                     poke hdrptr Cmsghdr {
                       cmsg_len = c_cmsg_len (fromIntegral len),
                       cmsg_level = level,
                       cmsg_type = ty
                     }
                     moveBytes dataptr payloadptr len
                   nexthdr <- c_cmsg_nxthdr msgptr hdrptr
                   go nexthdr cmsgs

withMsghdrCmsgs :: Msghdr -> [ControlMsg f] -> (Ptr Msghdr -> IO a) -> IO a
withMsghdrCmsgs msghdr cmsgs f =
    let cmsgBufLen = cmsgBufferSpace . map (BS.length . cMsgPayload) $ cmsgs
     in allocaBytes (fromIntegral cmsgBufLen) $ \cmsgBuf ->
        let msghdr' = msghdr {
              msg_control = cmsgBuf,
              msg_controllen = cmsgBufLen
            }
            cmsgBufEnd = cmsgBuf `plusPtr` fromIntegral cmsgBufLen
         in with msghdr' $ \msgptr -> do
              fillBytes cmsgBuf 0 (fromIntegral cmsgBufLen)
              pokeCmsgs msgptr cmsgBufEnd cmsgs
              f msgptr

{-
allocaMsghdr :: Family f => [Int] -> (Ptr Msghdr -> IO a) -> IO (Msg f, a)
allocaMsghdr cmsgPayloadLengths f = undefined
-}

sizeOf' :: Storable a => p a -> Int
sizeOf' p = sizeOf (arg p)
  where arg :: p a -> a
        arg = undefined


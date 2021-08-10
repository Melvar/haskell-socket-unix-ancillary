-- |
-- Stability   :  experimental
-- Portability :  Unix
{-# LANGUAGE MultiParamTypeClasses #-}
module System.Socket.Msg.Internal (
  -- * Friendly control message interface
    ControlMsg (..)
  , ExpectControl (..)
  , ControlMsgData (..)
  -- * Internal utilities
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
  -- * Reexports
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

-- | A serialized control message, parameterized by the socket family it can be used with.
data ControlMsg f =
  -- | To construct a control message, the level and type must be set to the constants documented for that type of control message, and the content serialized in the corresponding format.
  --
  -- To deconstruct a control message, the level and type must both match the constants documented for the type of control message you are handling, then the content can be deserialized. Preferably also check the payload length if you do this using @Foreign.Storable.peek@.
  ControlMsg {
    cMsgLevel :: CInt, -- ^ Identifies the protocol to which the control message belongs
    cMsgType :: CInt, -- ^ Identifies the type of the control message under that protocol
    cMsgPayload :: ByteString -- ^ The actual contents of the control message

--    cMsgPayloadLen :: Int
  }

-- | An instruction to `System.Socket.Msg.receiveMsg` and `System.Socket.Msg.receiveMsgFrom` to prepare the reception of a control message.
newtype ExpectControl f =
  -- | This is the size of the payload in bytes, not including the header.
  --
  -- When defining a `ControlMsgData` instance, you should also supply a function to calculate the required space for the payload when receiving a control message of your type, possibly depending on parameters, and return it wrapped in `Expect`.
  Expect { unExpect :: Int }

-- | Types that can be serialized to and deserialized from control messages compatible with a given socket family.
class Family f => ControlMsgData f c where
  -- | Serialize a value into a control message
  toControlMsg :: c -> ControlMsg f
  -- | Check whether a control message is of a type corresponding to this instance, and deserialize it if so
  fromControlMsg :: ControlMsg f -> Maybe c

-- | A default value for `Msghdr`s. Its buffer and array pointers are null, their associated lengths and flags are zero.
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

-- | Construct an `Iovec` from an arbitrary-typed pointer and an `Int`.
mkIovec :: Ptr a -> Int -> Iovec
mkIovec ptr len = Iovec (castPtr ptr) (fromIntegral len)

-- | Use a `Data.ByteString.ByteString` with a function requiring an `Iovec`.
--
-- All the caveats and unsafety of `Data.ByteString.Unsafe.unsafeUseAsCStringLen` apply.
unsafeUseByteStringAsIovec :: ByteString -> (Iovec -> IO a) -> IO a
unsafeUseByteStringAsIovec bs f =
    unsafeUseAsCStringLen bs $ f . uncurry mkIovec

-- | Use a list of `Data.ByteString.ByteString` with a function requiring a pointer to an array of `Iovec`.
--
-- All the caveats and unsafety of `Data.ByteString.Unsafe.unsafeUseAsCStringLen` apply, for each `Data.ByteString.ByteString` in the list.
unsafeUseByteStringsAsIovec :: [ByteString] -> (Ptr Iovec -> IO a) -> IO a
unsafeUseByteStringsAsIovec bss f =
    withMany unsafeUseByteStringAsIovec bss $ \vecs ->
    withArray vecs f

-- | Given a maximum size @l@ and an action @f@ that fills the buffer of an `Iovec` and returns the actual utilized length, @`createByteStringUptoNAsIovec` l f@ returns the filled `Data.ByteString.ByteString`.
createByteStringUptoNAsIovec :: Int -> (Iovec -> IO Int) -> IO ByteString
createByteStringUptoNAsIovec n f = createUptoN n $ \ptr -> f (mkIovec ptr n)

-- | Calculate the required size of a control message buffer that contains messages with the payload lengths from the list.
cmsgBufferSpace :: [Int] -> CSize
cmsgBufferSpace = sum . map (c_cmsg_space . fromIntegral)

-- | Peek the control messages out of a @struct msghdr@ that points to a control message buffer. The C macros require the @struct msghdr@ to be able to access the buffer size.
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

-- | Poke a list of control messages into the control message buffer of a @struct msghdr@. The second argument points to the end of the control message buffer (aka the first byte after it), which is used to check against buffer overflow.
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

-- | Allocate a control message buffer, add it to the given `Msghdr`, then allocate and poke the @struct msghdr@, then poke the control messages into the buffer, giving the resulting properly-initialized @`Foreign.Ptr.Ptr` `Msghdr`@ to the argument action.
--
-- This function exists because poking the @struct msghdr@ has to be interleaved between allocating and filling the control message buffer, so there can’t be a function that does just the latter two.
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

-- | Helper to get the `Foreign.Storable.sizeOf` something from a pointer to it, proxy of it, etc.
sizeOf' :: Storable a => p a -> Int
sizeOf' p = sizeOf (arg p)
  where arg :: p a -> a
        arg = undefined


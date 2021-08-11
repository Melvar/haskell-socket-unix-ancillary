-- |
-- Stability   :  experimental
-- Portability :  Unix
--
-- This module exposes the internals of control messages. It should be imported by modules that implement a control message type.
--
-- Such a module should provide:
--
-- * If this control message type can be sent:
--
--     * A function to construct a control message of this type.
--
-- * If this control message type can be received:
--
--     * A function to test whether a control message is of this type, and extract its payload if so. This function should return a `Prelude.Maybe`, and produce `Prelude.Nothing` if the control message is not of the relevant type.
--     * A function to calculate the payload size of such a control message, in bytes and excluding the header, wrapped in `Expect`, to size the buffer for receiving the actual control message.
--
-- To construct a control message, the level and type must be set to the constants documented for that type of control message, and the content serialized to a `Data.ByteString.ByteString` in the corresponding format.
--
-- To deconstruct a control message, the level and type must both match the constants documented for the type of control message you are handling, then the content can be deserialized. Preferably also check the payload length if you do this using @Foreign.Storable.peek@.
--
-- See "System.Socket.Ancillary.Fds" for an example.
module System.Socket.Msg.ControlMsg (
    ControlMsg (..)
  , ExpectControl (..)
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt)

-- | A serialized control message, parameterized by the socket family it can be used with.
data ControlMsg f =
  ControlMsg {
    cMsgLevel :: CInt, -- ^ Identifies the protocol to which the control message belongs
    cMsgType :: CInt, -- ^ Identifies the type of the control message under that protocol
    cMsgPayload :: ByteString -- ^ The actual contents of the control message
  }

-- | An instruction to `System.Socket.Msg.receiveMsg` and `System.Socket.Msg.receiveMsgFrom` to prepare the reception of a control message.
newtype ExpectControl f =
  Expect { unExpect :: Int }

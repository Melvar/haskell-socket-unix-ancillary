# `socket-unix-ancillary`

This library is an extension to the [`socket`](https://hackage.haskell.org/package/socket) library.
It adds the ability to send and receive control messages, widely known as ancillary data, on a socket, which can be used for a variety of things depending on the socket family.

To do so, `System.Socket.Msg` contains wrappers around the `sendmsg` and `recvmsg` system calls. As a bonus, these allow sending multiple `ByteString`s in one call without needing to concatenate (and therefore copy) them first.

The representation of control messages is exposed in `System.Socket.Msg.ControlMsg`. To add support for a control message type, import this module and read its documentation.

The only control message type currently provided in this library is the one to send file descriptors via a Unix domain socket, in `System.Socket.Ancillary.Fds`.

## Portability

Currently only supports Linux, as certain struct fields seem to have different types on BSD Unix variants. It should be possible to support other Unixes by providing suitable variants of the affected type(s) via conditional compilation if anyone is interested.

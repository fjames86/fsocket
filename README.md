

# 1. Introduction
Intended to offer a socket API that wraps at a very low level the underlying BSD-sockets API.
Only supports the standard blocking I/O calls, does not support overlapped or aio_ type asynchronous calls.

# 2. Usage

## 2.1 Functions
* OPEN-SOCKET
* CLOSE-SOCKET
* SOCKET-SHUTDOWN
* SOCKET-BIND
* SOCKET-LISTEN
* SOCKET-CONNECT
* SOCKET-ACCEPT
* SOCKET-OPTION
* (SETF SOCKET-OPTION)

## 2.2 I/O
There are two types of I/O: send/recv for connected sockets and sendto/recvfrom for unconnected sockets.

* SOCKET-SEND
* SOCKET-SENDTO
* SOCKET-RECV
* SOCKET-RECVFROM

## 2.3 Polling
Provides an API based on POSIX poll(). On systems where this is available (i.e. everywhere except Windows)
it calls directly to poll(). On Windows it calls WSAEnumNetworkEvents() and iterates over each socket. The Windows
events are mapping to poll events.

* OPEN-POLL
* CLOSE-POLL
* POLL-REGISTER
* POLL-UNREGISTER
* POLL
* DOEVENTS

## 2.4 Misc

* LIST-INTERFACES
* LIST-ADAPTERS

# 3. Socket options
List all the available socket options that can be get/set here.

# 4. Advanced topics
Various advanced usage topics here.

## 4.1 Polling 

## 4.2 IP multicast 

## 4.3 Subclassing POLLFD

# 5. Supported implementations and operating systems
* SBCL (Windows, Linux, FreeBSD)
* CCL (OS/X FreeBSD)
* LispWorks (Windows)

# 6. TODO
[ ] Type all the stuff in for POSIX systems
[ ] LIST-ADAPTERS
[ ] LIST-INTERFACES
[ ] Write a whole suite of tests to check it all works
[ ] Write something non-trivial using it to make sure it has a sane API

# 6. License
Licensed under the terms of the MIT license.
Frank James 2015.

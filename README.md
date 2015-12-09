

# 1. Introduction
Intended to offer a socket API that wraps at a very low level the underlying BSD-sockets API.
Only supports the standard blocking I/O calls, does not support overlapped or aio_ type asynchronous calls.

# 2. Usage

## 2.1 Socket functions
* OPEN-SOCKET :: open a socket 
* CLOSE-SOCKET :: close a socket 
* SOCKET-SHUTDOWN :: graceful shutdown of a connected socket 
* SOCKET-BIND :: bind to local address
* SOCKET-LISTEN :: listen for connections 
* SOCKET-CONNECT :: connect 
* SOCKET-ACCEPT :: accept 
* SOCKET-OPTION :: get a socket option
* (SETF SOCKET-OPTION) :: set a socket option

## 2.2 Socket I/O
There are two types of I/O: send/recv for connected sockets and sendto/recvfrom for unconnected sockets.

* SOCKET-SEND :: send a buffer on a connected socket 
* SOCKET-SENDTO :: send a buffer on an unconnected socket
* SOCKET-RECV :: receive on a connected socket 
* SOCKET-RECVFROM :: receive on an unconneted socket 

## 2.3 Polling
Provides an API based on POSIX poll(). On systems where this is available (i.e. everywhere except Windows)
it calls directly to poll(). On Windows it calls WSAEnumNetworkEvents() and iterates over each socket. The Windows
events are mapping to poll events.

* OPEN-POLL :: open a polling context
* CLOSE-POLL :: close a polling context
* POLL-REGISTER :: register a socket with the poll context
* POLL-UNREGISTER :: unregister a socket from the poll context
* POLL :: wait for something to happen
* DOEVENTS :: iterate over each of the fds with events pending

## 2.4 Misc

* LIST-ADAPTERS :: list host adapters

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
[x] Type all the stuff in for POSIX systems (Linux,FreeBSD)
[x] LIST-ADAPTERS
[ ] Write a whole suite of tests to check it all works
[ ] Write something non-trivial using it to make sure it has a sane API

# 7. License
Licensed under the terms of the MIT license.
Frank James 2015.

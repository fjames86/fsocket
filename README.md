

# 1. Introduction
This is yet-another Common Lisp FFI to provide a portable sockets API. It is intended to offer a
socket API that wraps at a very low level the underlying BSD-sockets API provided by the host OS.
Only supports the standard blocking I/O calls, does not support overlapped or aio_ type asynchronous calls.

# 2. Usage
Basically you write exactly the same sort of code you would if writing in C.

## 2.1 Socket functions
* OPEN-SOCKET :: open a socket.
* CLOSE-SOCKET :: close a socket. 
* SOCKET-SHUTDOWN :: graceful shutdown of a connected socket. 
* SOCKET-BIND :: bind to local address.
* SOCKET-LISTEN :: listen for connections. 
* SOCKET-CONNECT :: connect to remote server.
* SOCKET-ACCEPT :: accept connection, returns connected socket. 
* SOCKET-OPTION :: get a socket option.
* (SETF SOCKET-OPTION) :: set a socket option.

## 2.2 Socket I/O
There are two types of I/O: send/recv for connected sockets and sendto/recvfrom for unconnected sockets.

* SOCKET-SEND :: send a buffer on a connected socket. 
* SOCKET-SENDTO :: send a buffer on an unconnected socket.
* SOCKET-RECV :: receive on a connected socket.
* SOCKET-RECVFROM :: receive on an unconnected socket. 

## 2.3 Polling
Provides an API based on POSIX poll(). On systems where this is available (i.e. everywhere except Windows)
it calls directly to poll(). On Windows it calls WSAEnumNetworkEvents() and iterates over each socket. The Windows
events are mapping to poll events.

* OPEN-POLL :: open a polling context.
* CLOSE-POLL :: close a polling context.
* POLL-REGISTER :: register a socket with the poll context. 
* POLL-UNREGISTER :: unregister a socket from the poll context.
* POLL :: wait for something to happen.
* DOEVENTS :: iterate over each of the fds with events pending.

## 2.4 Hardware adapters

* LIST-ADAPTERS :: list host adapters, returns a list of ADAPTER structures.

These contain a list of IP addresses associated with the adapter (i.e. logical interfaces), the
physical address of the adapter, the interface index and current status.

# 3. Socket options
The currently supported socket options are as follows.

Socket options `:SOCKET`
* SO_ACCEPTCONN `:ACCEPTCONN` boolean
* SO_BROADCAST `:BROADCAST` boolean
* SO_REUSEADDR `:REUSEADDR` boolean
* SO_SNDBUF `:SNDBUF` integer
* SO_RCVBUF `:RCVBUF` integer
* SO_SNDTIMEO `:SNDTIMEO` integer
* SO_RCVTIMEO `:RCVTIMEO` integer

IP options `:IP`
* IP_MULTICAST_LOOP `:IP-MULTICAST-LOOP` boolean
* IP_MULTICAST_IF `:IP-MULTICAST-IF` 4-octet array
* IP_MULTICAST_TTL `:IP_MULTICAST_TTL` integer
* MCAST_JOIN_GROUP `:MCAST-JOIN-GROUP` (index addr) where index is the interface index and addr is the multicast address

TCP options `:TCP`
* TCP_NODELAY `:NODELAY` boolean

# 4. Advanced topics
Various advanced usage topics here.

## 4.1 Polling 
FSOCKET provides a portable polling mechanism based on POSIX poll(). On those systems is calls poll() directly,
but on Windows we need a little bit more work. In order to provide a uniform interface on all platforms
we must do the following:

1. Allocate a *poll context*. On Windows this calls WSACreateEvent(). On POSIX it does nothing.
2. For each socket you wish to poll, call POLL-REGISTER. On Windows this calls WSAEventSelect(). This associates
a POLLFD instance (which encapsulates the socket) with the poll context.
3. Call POLL. On Windows this calls WSAWaitForMultipleEvents() and then WSAEnumNetworkEvents() for each socket.
On POSIX it simply calls poll().
4. Iterate over each of the input POLLFD descriptors and process the pending events.

## 4.2 IP multicast 
IPv4 UDP multicast is implemented and working. See test/test2.lisp.

## 4.3 Subclassing POLLFD
When you call POLL-REGISTER you provide an instance of the class POLLFD, which encapsulates the socket
and the events you wish to poll for. However, typically you will also want to associate other information with
the socket, for instance and receive buffer or other contextual information. All you have to do is subclass
POLLFD and provide instances of your own class to POLL-REGISTER. For an example see TCP-ECHO in test/test1.lisp
where I provide different subclasses for the listening TCP socket and its connections. 


# 5. Implementations and operatings systems
Because we are calling directly into the host API using CFFI, the implementation portability issues
are handled by CFFI. I intend to support Windows, Linux, FreeBSD and maybe OSX.

* Tested: SBCL x86-64 1.2.11 Windows 
* Tested: SBCL x86-64 1.2.9 FreeBSD-10.2
* TODO: SBCL Linux
* TODO: CCL (Linux, OSX, FreeBSD)
* TODO: LispWorks (Windows)
* TODO: others

# 6. TODO
- [x] Type all the stuff in for POSIX systems (Linux,FreeBSD)
- [x] LIST-ADAPTERS
- [ ] Write a whole suite of tests to check it all works
- [ ] Write something non-trivial using it to make sure it has a sane API

# 7. License
Licensed under the terms of the MIT license.
Frank James 2015.

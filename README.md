# fsocket

## 1. Introduction
This is yet-another Common Lisp FFI to provide a portable sockets API. It is intended to offer a
socket API that wraps at a very low level the underlying BSD-sockets API provided by the host OS.
Only supports the standard blocking I/O calls, does not support overlapped or aio_ type asynchronous calls.

## 2. Usage
Basically you write exactly the same sort of code you would if writing in C.

### 2.1 Socket functions
* OPEN-SOCKET :: open a socket.
* CLOSE-SOCKET :: close a socket. 
* SOCKET-SHUTDOWN :: graceful shutdown of a connected socket. 
* SOCKET-BIND :: bind to local address.
* SOCKET-LISTEN :: listen for connections. 
* SOCKET-CONNECT :: connect to remote server.
* SOCKET-ACCEPT :: accept connection, returns connected socket.
* SOCKET-NAME :: get local address of bound socket.
* SOCKET-OPTION :: get a socket option.
* (SETF SOCKET-OPTION) :: set a socket option.

```
;; open the socket 
(let ((fd (open-socket :type :datagram)))
  (socket-bind fd (make-sockaddr-in))
  ;; do something with the socket 

  ;; close it
  (close-socket fd))
```

### 2.2 Socket I/O
There are two types of I/O: send/recv for connected sockets and sendto/recvfrom for unconnected sockets.

* SOCKET-SEND :: send a buffer on a connected socket. 
* SOCKET-SENDTO :: send a buffer on an unconnected socket.
* SOCKET-RECV :: receive on a connected socket.
* SOCKET-RECVFROM :: receive on an unconnected socket. 

```
(let ((fd (open-socket :type :datagram))
      (buffer (make-array 1024)))
  (socket-bind fd (make-sockaddr-in :port 9000))
  (multiple-value-bind (count raddr) (socket-recvfrom fd buffer)
    (format t "Received ~A bytes from ~A~%" count raddr)
    (socket-sendto fd buffer raddr :end count))
  
  (close-socket fd))
```

### 2.3 Polling
Provides an API based on POSIX poll(). On systems where this is available (i.e. everywhere except Windows)
it calls directly to poll(). On Windows it calls WSAEnumNetworkEvents() and iterates over each socket. The Windows
events are mapping to poll events.

* OPEN-POLL :: open a polling context.
* CLOSE-POLL :: close a polling context.
* POLL-REGISTER :: register a socket with the poll context.
* POLL-UNREGISTER :: unregister a socket from the poll context.
* POLL :: wait for something to happen.
* DOEVENTS :: iterate over each of the fds with events pending.

```
(let ((pc (open-poll)))
  (poll-register pc (make-instance 'pollfd :fd fd :events (poll-events :pollin)))
  (doevents (pollfd event) (poll pc :timeout 1000)
    ;; process the events
    )
  (close-poll pc))
```

### 2.4 Hardware adapters

* LIST-ADAPTERS :: list host adapters, returns a list of ADAPTER structures.

These contain a list of IP addresses associated with the adapter (i.e. logical interfaces), the
physical address of the adapter, the interface index and current status.

## 3. Socket options
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

## 4. Advanced topics
Various advanced usage topics here.

### 4.1 Polling 
FSOCKET provides a portable polling mechanism based on POSIX poll(). On those systems is calls poll() directly,
but on Windows we need a little bit more work. In order to provide a uniform interface on all platforms
we must do the following:

1. Allocate a *poll context*. On Windows this calls WSACreateEvent(). On POSIX it does nothing.
2. For each socket you wish to poll, call POLL-REGISTER. On Windows this calls WSAEventSelect(). This associates
a POLLFD instance (which encapsulates the socket) with the poll context.
3. Call POLL. On Windows this calls WSAWaitForMultipleEvents() and then WSAEnumNetworkEvents() for each socket.
On POSIX it simply calls poll().
4. Iterate over each of the input POLLFD descriptors and process the pending events.

#### 4.1.1 Side-effects of `POLL-REGISTER`
* On Windows, `POLL-REGISTER` calls WSAEventSelect, which will convert
the socket to non-blocking mode. 
* On POSIX systes `POLL-REGISTER` does nothing other than pushing the pollfd
instance onto a list of pollfds stored with the poll context.
* To preserve semantics, in posix systems the socket is put into non-blocking mode by calling `fcntl()` 
* Therefore users should be aware that once they have registered with the
poll context, the socket will now be in non-blocking mode.

## 4.2 IP multicast 
IPv4 UDP multicast is implemented and working. See test/test2.lisp.

* `MULTICAST-JOIN` ::= join the socket to the multicast group on all ethernet interfaces.
* `OPEN-MULTICAST-SOCKET` ::= open a socket to send multicast datagrams.

### 4.3 Subclassing POLLFD
When you call POLL-REGISTER you provide an instance of the class POLLFD, which encapsulates the socket
and the events you wish to poll for. However, typically you will also want to associate other information with
the socket, for instance and receive buffer or other contextual information. All you have to do is subclass
POLLFD and provide instances of your own class to POLL-REGISTER. For an example see TCP-ECHO in test/test1.lisp
where I provide different subclasses for the listening TCP socket and its connections. 

### 4.4 UDP broadcast
There can be issues when performing UDP broadcast to address 255.255.255.255 on FreeBSD.
This is due to the way FreeBSD implements UDP broadcasting, which is different to Linux and Windows in this respect. 

### 4.5 Unix Domain sockets 
On POSIX platforms (for fsocket, that means Linux, FreeBSD, Darwin i.e. everywhere but Windows) unix domain sockets
are also supported as a family `:UNIX` to `OPEN-SOCKET`. Currently only `:STREAM` type sockets are supported, datagram 
type unix domain sockets may be supported at some point in the future. 
Unlike `:INET` and `:INET6` families, `:UNIX` sockets are addressed using a string which represents a file
on the host. Otherwise their usage is the same. See `test/test-unix.lisp` for an example.

## 5. Implementations and operatings systems
Because we are calling directly into the host API using CFFI, the implementation portability issues
are handled by CFFI. I intend to support Windows, Linux, FreeBSD and OSX. Currently the following have been
tested. A small amount of work will be required to support other systems (e.g. checking structs have the same
layout, adjusting constants etc.). 

* Tested: SBCL x86-64 1.2.11 Windows 7
* Tested: SBCL x86-64 1.2.9 FreeBSD-10.2
* Tested: SBCL x86 1.2.7 Linux 
* Tested: LispWorks Personal Edition 6.1.1 Windows 8.1
* Tested: CCL x86-64 1.11 OSX
* TODO: CCL (Windows, Linux, FreeBSD)
* TODO: arch other than x86 or x86-64.
* TODO: others

## 6. TODO
- [x] Type all the stuff in for POSIX systems (Linux,FreeBSD)
- [x] LIST-ADAPTERS
- [ ] Write a whole suite of tests to check it all works
- [x] Write something non-trivial using it to make sure it has a sane API
- [ ] Support raw sockets? Unlikely to happen anytime soon.
- [x] Unix domain sockets

## 7. License
Licensed under the terms of the MIT license.
Frank James 2015.

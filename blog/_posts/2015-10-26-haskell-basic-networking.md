---
layout: post
title: "Haskell Basic Networking"
tags: [computer network, haskell]
---

This post is a set of notes from  [Chapter 27](http://book.realworldhaskell.org/read/sockets-and-syslog.html) of *Real World Haskell*. In this chapter, the authors discuss basic network programming using Haskell. It presents two simple client-server communication examples: one using UDP and the other TCP.

In this post, we'll start by revising some basic computer network concepts and then will comment on different parts of the examples presented in the book.

### Theory

**The Transport Layer**



The communication between two computers is often organized in multiple layers, following the [OSI model](https://en.wikipedia.org/wiki/OSI_model) standard. One of the layers is the transport layer. This layer is responsible for transferring data from a source to a destination, offering different levels of guarantees. The most famous transport layer protocols are UDP and TCP.

[UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) stands for User Datagram Protocol and [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol) for Transmission Control Protocol.

**TCP and UDP**



UDP provides a lightweight abstraction to send data from one host to another, by sending pieces of information, called [Datagram](https://en.wikipedia.org/wiki/Datagram), one at a time. According to [[2](https://docs.oracle.com/javase/tutorial/networking/datagrams/definition.html)]:

> A datagram is an independent, self-contained message sent over the network whose arrival, arrival time, and content are not guaranteed.

Because of this, we have no guarantee the packet will arrive in order or that the packets will arrive at all. UDP uses checksum to verify whether a given packets arrived to the host was corrupted.

TCP offers more guarantees than UDP, but is less performant. It first establishes a connection between the client and the server and then sends [TCP segments](https://en.wikipedia.org/wiki/Transmission_Control_Protocol#TCP_segment_structure). Within a connection, TCP in the server is able to sort the segments in the order they were sent by the client. Also, it can retransmit segments if it doesn't receive confirmation.

**Network sockets**



A network socket is the endpoint of inter-process communication between computers in a network.

The sockets types include:

* `Datagram` which uses the User Datagram Protocol (UDP)
* `Stream`, which uses the Transmission Control Protocol (TCP) or Stream Control Transmission Protocol (SCTP).
* `Raw sockets`, which bypass the transport layer.

Unix-based systems use the [Berkeley sockets API](https://en.wikipedia.org/wiki/Berkeley_sockets) which uses file descriptors (integers) to identify a socket.

### Client-server using UDP

Let's study the code. As the authors mention [1], the functions provided by the [Network.Socket](https://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket.html) module, are corresponding to the low-level functions in C, so we can refer to those for documentation.

**The getaddrinfo() function**



The [getaddrinfo()](http://linux.die.net/man/3/getaddrinfo) function takes a node (hostname), a service (port) and a set of hints flags as inputs and returns a list of structures called addrinfo as output. It will try to find all the addresses matching the constraints provided from the inputs.

There are two modes we're interested in here: *listening* and *publishing*. For the listening mode, we can provide a flag `AI_PASSIVE` to the hints flags and a null value to node. According to the man page:

> If the AI_PASSIVE flag is specified in hints.ai_flags, and node is NULL, then the returned socket addresses will be suitable for bind(2)ing a socket that will accept(2) connections

In Haskell we're doing exactly that for the server:

{% highlight haskell %}

addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing
               (Just port)

{% endhighlight %}

For the publishing mode, the docs say:

> If the AI_PASSIVE flag is not set in hints.ai_flags, then the returned socket addresses will be suitable for use with connect(2), sendto(2), or sendmsg(2)
> 

In our client code we then do:

{% highlight haskell %}

addrinfos <- getAddrInfo
               Nothing
               (Just hostname)
               (Just port)

{% endhighlight %}

**The socket() function**



A socket is like a network file descriptor. The [socket()](http://linux.die.net/man/3/socket) function takes the family domain, type of socket and the protocol. It's not clear from the docs what this protocol refers to, expect that 0 is the default protocol and it's dependent of the address family (first parameter). [[3](http://stackoverflow.com/questions/2368580/socket-protocol-fundamentals)] Suggests it's the application layer protocol (e.g. HTTP, POP3).

Since we're going to use UDP, the arguments passed to the socket function in our Haskell code are:

{% highlight haskell %}

sock <- socket
          (addrFamily serveraddr)
          Datagram
          defaultProtocol

{% endhighlight %}

**Server: Listening**



With the socket file descriptor, we can bind an address to it using the [bind](http://linux.die.net/man/2/bind) function. It takes a socket file descriptor and the address and returns 0 on success or -1 on error.

To receive the messages, we use the [recvfrom()](http://linux.die.net/man/2/recvfrom) function, which takes the socket, the maximum size of the packet and will return the message and the address of the sender. In the Haskell version, we have recvFrom implemented in `Network.Socket`. The documentation has the following warning though:

> Do not use the send and recv functions defined in this module in new code, as they incorrectly represent binary data as a Unicode string. As a result, these functions are inefficient and may lead to bugs in the program. Instead use the send and recv functions defined in the ByteString module.
> 

We can use the ByteString version by doing

{% highlight haskell %}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

{% endhighlight %}

We also need to update all the places we use `Strings` with `ByteString`.

**Client: Sending data**



From the client side, we can use the [sendto()](http://linux.die.net/man/2/sendto) function, providing the socket file descriptor, the data and the address of the server. The function will return the number of bytes sent.

In our Haskell code, we have

{% highlight haskell %}

sendTo (slSocket syslogh) omsg (slAddress syslogh)

{% endhighlight %}

Where `slSocket` gets the socket, `osmg` is the message, and `slAddress` the host address. This call might not send the entire message at once, so we have to keep calling this function until the message is completely sent.

**Debugging**



After trying running the code above for the client and server, I was not able to have the server print out the messages sent from the client in a Mac OS X. My first suspicion was that the server code had some missing configuration or bug.

I've tried using *netcat*, a tool for reading or writing to network connections via UDP or TCP. To listen to port 1514 using UDP we can do it by running:

`nc -u -l -k 1514`

The `u` flag indicates we're using UDP (default is TCP). The `l` flag indicates we're listening instead of sending, and `k` tells netcat not to disconnect after the client disconnects. So we now basically have a simple server on `localhost:1514`.

I've made a binary for the `syslogclient.hs` code example, by simply adding a main function and compiling it using ghc:

{% highlight haskell %}

main = do
  message <- getLine
  h <- openlog "localhost" "1514" "syslogclient"
  syslog h USER INFO message
  closelog h

{% endhighlight %}

When running:

{% highlight haskell %}

$ ghc syslogclient.hs
$ ./syslogclient
hello world

{% endhighlight %}

I didn't see any output from the netcat side. The next test was verifying if the client code had an issue. I took a similar approach with the `syslogserver.hs` code, adding the main function and generating a binary:

{% highlight haskell %}

main = do
  putStrLn "Starting server...\n"
  serveLog "1514" plainHandler

{% endhighlight %}

Then started the server up:

{% highlight haskell %}

$ ghc syslogserver.hs
$ ./syslogserver

{% endhighlight %}

This time I used netcat to send the message using UDP. The command I ended up using was

{% highlight haskell %}

echo "hello world" | nc -4u localhost 1514

{% endhighlight %}

As in the listening mode, the `u` flag here tells netcat to use the UDP protocol and 4 forces it to use IPv4 only. And this finally worked!

At this point there were a couple of questions hanging: what configuration is missing from the client code and why the server only displays the message if I force it to use IPv4 addresses?

**Trace.** One tool I've been missing from Haskell was the ability to print variables values at specific points in code. I've found on StackOverflow an interesting discussion which points out Debug.Trace.trace as a simple function to do this.

It's an impure function and also messes up with lazy evaluation, so it's recommended only for debugging purposes. It can be used in a neat way. Say we have a function

{% highlight haskell %}

someFunction x y = x + y

{% endhighlight %}

and we want to print the contents of x and y during runtime. We can just add one line, with minimal modification to existing code:

{% highlight haskell %}

someFunction | trace ("Value of x: " ++ x ++ " and y: " ++ y) False = undefined
someFunction x y = x + y

{% endhighlight %}

Because `trace` prints its first argument and returns the second, we basically using this syntax

{% highlight haskell %}

someFunction x y | False = undefined
someFunction x y = x + y

{% endhighlight %}

We'll try to do the pattern matching with the first form, but since it returns `False`, we'll end up executing the second form of `someFunction()`. Another option is to create a standalone `print()` function to print a given value. For example,

{% highlight haskell %}

print x = trace ("Value of x: " ++ x) x

{% endhighlight %}

With this trick in our toolkit, we can inspect which addresses returned by `getAddrInfo()` in the client by adding

{% highlight haskell %}

traceAddrs :: [AddrInfo] -> [AddrInfo]
traceAddrs addrs = trace (intercalate ", " (map (show . addrAddress) addrs)) addrs

{% endhighlight %}

When running the client code again, we get the following output:

{% highlight haskell %}

[::1]:1514, [::1]:1514, 127.0.0.1:1514, 127.0.0.1:1514

{% endhighlight %}

The first two values, "`::1`", represent an IPV6 address (0000:0000:0000:0000:0000:0000:0000:0001). According to [Wikipedia](https://en.wikipedia.org/wiki/IPv6#Address_representation),

> Consecutive sections of zeroes are replaced with a double colon (::). The double colon may only be used once in an address, as multiple use would render the address indeterminate 

Since we pick up the first address returned by getAddrInfo, we're using IPv6 to connect to the server. We can force it to use IPv4 by passing the AF_INET flag:

{% highlight haskell %}

addrinfos <- getAddrInfo
    -- set it to use IPv4
    (Just (defaultHints {addrFamily = AF_INET}))
    Nothing
    (Just port)

{% endhighlight %}

We can now run the client and send a message, and it will successfully be sent to the server.

Doing a similar investigation on the server code, we get:

{% highlight haskell %}

0.0.0.0:1514, 0.0.0.0:1514, [::1]:1514, [::1]:1514,

{% endhighlight %}

Since we're picking the head of the list, the server is actually listening on an IPv4 address. We can force it to use IPv6 by passing the `AF_INET6` flag.

{% highlight haskell %}

addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE], addrFamily = AF_INET6}))
    Nothing (Just port)

{% endhighlight %}

Now the server can listen to requests of both IPv4 and IPv6 clients. Mystery solved!

### Client-server using TCP

**Server: Multi-threaded Listening**



There are a couple of differences between the TCP and UDP server.

1. The socket type we use is *Stream* instead of a *Datagram*.

2. Second, we call the [listen](http://linux.die.net/man/2/listen) function, which marks the socks as accepting connections. The second argument is the maximum size of the connection queue:

{% highlight haskell %}

listen sock 5

{% endhighlight %}

3. Instead of `recvFrom()`, we then call [accept](http://linux.die.net/man/2/accept), which picks the first of the pending connections in the queue, and creates a new socket. The server then spawns a new thread to handle that socket, so that the main thread can continue processing more connections.

{% highlight haskell %}

procRequests :: MVar () -> Socket -> IO ()
procRequests lock mastersock = do
  (connsock, clientaddr) <- accept mastersock
  forkIO $ procMessages lock connsock clientaddr
  procRequests lock mastersock

{% endhighlight %}

4. Use a file handle instead of a socket. Because we keep a stick connection, we can use a file handle to abstract the reading from the socket.

Each thread reads the message from the connection

{% highlight haskell %}

-- Converts a socket (connsock) to a handle
connhdl <- socketToHandle connsock ReadMode
-- Set handle to buffering mode
hSetBuffering connhdl LineBuffering
-- Read contents
messages <- hGetContents connhdl
-- Print messages
mapM_ (handle lock clientaddr) (lines messages)
-- Close connection
hClose connhdl

{% endhighlight %}

Here we use an `MVar` as a lock to guarantee that at most one thread is writing to stdout at a time. Otherwise we would see messages from different threads mixed up. This is the exact same approach we used in our [Haskell Concurrent Programming]({{site.url}}/blog/2015/05/14/haskell-concurrent-programming.html) post, when talking about using *MVar as a lock*.

**Client: Sticky connection**



Our TCP client also looks similar to the UDP counterpart, with a couple of differences.

1. As we did with the TCP server, we use Stream instead of Datagram.

2.  We also mark the socket as keep-alive:

{% highlight haskell %}

setSocketOption sock KeepAlive 1

{% endhighlight %}

which is basically telling the OS to periodically send packages to probe the server we're connected to. This serves both as a check to see if the server is still alive or to prevent the connection from being dropped due to inactivity [[4](http://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO)].

3. We establish a stick connection with the server:

{% highlight haskell %}

connect sock (addrAddress serveraddr)

{% endhighlight %}

4. As in the TCP server, we use a file handle instead of a socket:

{% highlight haskell %}

h <- socketToHandle sock WriteMode

{% endhighlight %}

which provides us using common IO file functions like `hPutStrLn()`.

Every time we type a line, we want to send that string to the server. In the code below, we write a line to your file and flush it so it is sent to the server immediately.

{% highlight haskell %}

hPutStrLn (slHandle syslogh) sendmsg
hFlush (slHandle syslogh)

{% endhighlight %}

5. Keep sending read lines from stdio until EOF

I've added a simple main function to the code so we can compile the client code into a binary, and also added a function, `readData()`, to read lines from stdio until we send an `EOF` character:

{% highlight haskell %}

import Control.Monad
...
readData :: SyslogHandle -> IO ()
readData h = do
  done <- isEOF
  unless done readLine
  where
    readLine = do
                 message <- getLine
                 syslog h USER INFO message
         	 readData h

main = do
  h <- openlog "localhost" "1514" "syslogtcpclient"
  readData h
  closelog h

{% endhighlight %}

**Testing**



Given that we have our binaries, I've started a server first and then ran two client binaries. I was able to type messages in each of the clients and verified the server was handling them properly.

### Conclusion

Writing this post, I've learned about network programming and debugging in Haskell. I've had classes about network programming back in college, but it didn't seem fun at the time. When we study things for our own curiosity, it's much more interesting.

Also, in studying this chapter, I've tried using a more "curious mindset", always questioning why things are this way or another, and this forced me to do more research and learning things beyond those the book provided.

### References

* [[1](http://book.realworldhaskell.org/read/sockets-and-syslog.html)] Real World Haskell - Chapter 27. Sockets and Syslog
* [[2](https://docs.oracle.com/javase/tutorial/networking/datagrams/definition.html)] Oracle Java - What Is a Datagram?
* [[3](http://stackoverflow.com/questions/2368580/socket-protocol-fundamentals)] StackOverflow - Socket Protocol Fundamentals
* [[4](http://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO)] TCP Keepalive HOWTO

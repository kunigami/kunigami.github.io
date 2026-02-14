---
layout: post
title: "HTTP Basics"
tags: [computer network]
---

In this post we'll cover basic concepts of the HTTP protocol, and its variants, including the HTTP over TSL, commonly known as HTTPS. We'll also talk about related features such as cookies and the next generation of HTTP.

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2016-01-18-http-basics/2016_01_internet.png" alt="internet" />
</figure>

### Introduction

HTTP is the acronym for **Hypertext Transfer Protocol** [[1](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)]. It's a application level protocol, intended to be used between a client and a server. A common use case is a web browser acting as a client and a remote machine providing the contents of a given website acting as the server.

It assumes a reliable underlying transfer level protocol, and this is often TCP (we talked a bit about transport protocols in a [previous post]({{site.url}}/blog/2015/10/26/haskell-basic-networking.html)).

The original version of HTTP was defined in [RFC1945](https://tools.ietf.org/html/rfc1945) in 96. The current most popular implementation is of HTTP/1.1 ([RFC2068](https://tools.ietf.org/html/rfc2068), in 97). The HTTP/2.0 spec was recently finished ([RFC7540](https://tools.ietf.org/html/rfc7540) in 2015).

The protocol consists of two parts: first, the client sends a request to a server. Then, the server sends a response back.

### HTTP Request

When a client want to connect to a server, it create a request message, that has a common format:

{% highlight text %}

GET /index.html HTTP/1.1
Host: www.example.com

{% endhighlight %}

There are a specific commands a requester can send, defined in the HTTP/1.1 specification, including:

* [GET](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods) - should be use to request data. GET requests should not have major side effects, for example writing to databases, though bumping a counter to track the number of visitors, is fine. This is a guideline though, but is not enforced. It's up to the application to protect against malicious GET requests.

* HEAD - similar to GET, but the response should only contain the head of the response, without the body

* [POST](https://en.wikipedia.org/wiki/POST_(HTTP)) - should be able to cause side effects. Often times the results of forms are sent to the server as a POST request. Additional data is sent in the request body message as opposed to GET requests, where data is sent as part of the URL.

* OPTIONS - returns the list of available HTTP commands this server implements.

* PUT - replaces the content of the address specified by the URI.

* DELETE - deletes the content of the address specified by the URI.

* TRACE - displays the request message got by the server

GET and POST are the most common commands in the context of Browser/Server communication. Methods such as PUT and DELETE can be seen in applications like Elasticsearch.

We can test some of these commands using the curl command line:

{% highlight text %}

> curl -X GET https://www.google.com/
...
HTML contents
...

{% endhighlight %}

If we try TRACE, we get an error back (405):

{% highlight text %}

> curl -X TRACE https://www.google.com/
...
Error 405 (Method Not Allowed)!!1

{% endhighlight %}

### HTTP Response

After the server processes the request, it will return a response. The first line of the response it the status code and a textual reason phrase (note that this phrase are not standard, so clients should not rely on error messages).

The most common response header is

{% highlight text %}

HTTP/1.1 200 OK

{% endhighlight %}

and

{% highlight text %}

HTTP/1.1 404 File not found

{% endhighlight %}

The HTTP specification defines 5 groups of response status code, based on the response nature. The status code always has 3 digits, and the first digit represents the group:

* Informational `1XX`
* Successful `2XX`
* Redirection `3XX`
* Client Error `4XX`
* Server Error `5XX`

And example response, for example [[1](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)]:

{% highlight text %}

Date: Mon, 23 May 2005 22:38:34 GMT
Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
ETag: "3f80f-1b6-3e1cb03b"
Content-Type: text/html; charset=UTF-8
Content-Length: 138
Accept-Ranges: bytes
Connection: close



  An Example Page


  Hello World, this is a very simple HTML document.



{% endhighlight %}

The first lines represent the response header. After a blank line follows the response.

### HTTPS - HTTP over TSL

[HTTPS](https://en.wikipedia.org/wiki/HTTPS) was specified over SSL (**Secure Sockets Layer**), but SSL has security flaws, and have been since evolved to a more robust layer, [TSL](https://en.wikipedia.org/wiki/Transport_Layer_Security), which stands for **Transport Layer Security**.

<figure class="center_children">
    <img src="{{site.url}}/resources/blog/2016-01-18-http-basics/2016_01_15423276943_1591eea673_z.jpg" alt="Image credits: www.perspecsys.com" />
    <figcaption> Image credits: www.perspecsys.com</figcaption>
</figure>

**Motivation.** One problem with HTTP is that the request/response are exchanged over an unsecured network. It's possible for an attacker to intercept an user connection with the server and to have access to both request and responses. This is a common form of attack known at [Man-in-the-middle attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack).

By adding an encryption layer, the entire HTTP message can be encrypted using TSL. TSL idea is the following [[6](https://technet.microsoft.com/en-us/library/cc781476(v=ws.10).aspx)]:

* The client sends connection request to the server, providing a list of ciphers it can use (e.g. RSA) and a list of ways it can hash the data for integrity check (e.g. MD5 and SHA-1).

* The server sends as response a digital certificate and its public key,

* In one implementation, the client generates a random number associated with the current session, encrypts it using the server's public key, and send it to the server. Both client and server now have a shared key to encrypt and decrypt data, so they can use a symmetric key encryption such as AES ([Advanced Encryption Standard](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)).

The advantage of this above only using the public key of the server to encrypt the message (like a plain RSA implementation), associating this to the session adds [forward secrecy](https://en.wikipedia.org/wiki/Forward_secrecy). This is to add some security to the scenario in which someone saves all the encrypted messages and some day manages to steal the server's private key. In that case, it would be able to decrypt all the stored messages, but by generating an unique key every time, the attacker would need to also store the initial message containing the session key and associate the session key to the message.

The digital certificate is issued by a Certificate Authority (CA), a common trusted third party such as Symantec, which will attest the site integrity. The structure of the certificate is defined by the [X.509 standard](https://en.wikipedia.org/wiki/X.509).

Browsers already ship with a pre-installed list of trusted CAs, so when receiving a certificate from a trusted CA, the browser can look at the CA signature (which was encrypted with the CA private key) and decrypt it using the CA public key. The decrypted signature should match the information the browser has.

For untrusted CAs, it's up to the user to decide whether to trust a particular CA.

In Chrome, to inspect the Certificates, we can go to Settings... &gt; HTTPS/SSL &gt; Manage Certificates... (on Mac it will open the Keychan application). In the Keychan app, look for System Roots and the Certificates Categories, we can see a list of trusted CAs, like the one below, form VeriSign:

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/6666/12/screen-shot-2016-01-03-at-9-51-11-pm.png"><img src="{{site.url}}/resources/blog/2016-01-18-http-basics/6666_12_screen-shot-2016-01-03-at-9-51-11-pm.png" alt="Sample Certificate" /></a>
    <figcaption> Sample Certificate</figcaption>
</figure>

**Let's Encrypt.** In an ideal world, all HTTP connections would be secure, so no one could eavesdrop while you're browsing the web. Several companies like Google encourage its use, by having Chrome force the use of HTTPS whenever possible and also [boosting the rank of websites with https support](https://googlewebmastercentral.blogspot.com/2014/08/https-as-ranking-signal.html).

One major obstacle for this is having to rely on a non-free and complicated procedure with third-party CAs. To address this problem, recently the *Internet Security Research Group* (ISRG) proposed a new way to issue certificates for free.

The key is how to simplify the process of proving a given agent owns a given domain, for example [https://www.example.com.](https://www.example.com.) Let's encrypt will ask the agent to perform an action only the domain owner can do, for example putting a file under it (say, [https://www.example.com/file.txt)](https://www.example.com/file.txt)) [[7](https://letsencrypt.org/howitworks/technology/)].

LE will then do a regular HTTPS request to get that file. Note that LE doesn't have a trusted certificate for that domain, but it doesn't need to this initial stage.

In addition, LE needs the agent's public key and needs to validate it. This is simple: LE gets the agent's public key, generates a random string and encrypts it with the agent's public key. The agent will be able to decrypt the message using its own private key and then it encrypts it again using LE's public key. It finally sends it back, and LE can also decrypt it. If the resulting key is the same it sent originally, it will associate the agent public key to the domain.

Now LE can issue certificates to that particular domain. Major browsers already trust LE as a Certificate Authority, so this require no extra work from the agent.

### HTTP Cookies

One of the characteristics of HTTP requests is that they're stateless. That means that in theory an HTTP request is independent from the previous HTTP request. One way to simulate a state is having the browser and server pass (meta)data around carrying state information. This extra data is basically implemented as an [HTTP cookie](https://en.wikipedia.org/wiki/HTTP_cookie).

<figure class="center_children">
    <a href="https://kunigami.files.wordpress.com/6666/12/5452373550_6a0cb59fbf_o.jpg"><img src="{{site.url}}/resources/blog/2016-01-18-http-basics/6666_12_5452373550_6a0cb59fbf_o.jpg" alt="The term cookie came from magic cookie (another programming term), which came from fortune cookie" /></a>
    <figcaption> The term cookie came from magic cookie (another programming term), which came from fortune cookie</figcaption>
</figure>

Cookies are part of the [HTTP protocol](https://tools.ietf.org/html/rfc6265). A server can send a cookie to the browser in the HTTP response's header:

{% highlight text %}

HTTP/1.0 200 OK
Set-Cookie: lu=Rg3vHJZnehYLjVg7qi3bZjzg; Expires=Tue, 15-Jan-2013 21:47:38 GMT; Path=/; Domain=.example.com; HttpOnly
Set-Cookie: made_write_conn=1295214458; Path=/; Domain=.example.com
Set-Cookie: reg_fb_gate=deleted; Expires=Thu, 01-Jan-1970 00:00:01 GMT; Path=/; Domain=.example.com; HttpOnly

{% endhighlight %}

In the example above, it's sending three cookies back. The first part of the cookie is an expression `=`. The others are attributes like expiration date and path + domain. Path and domain are used to let the client know it should use these cookies for requests with the URLs matching that path and domain.

Once the cookie is set on the client, subsequent requests to the server will contain the cookie information. For example:

{% highlight text %}

GET /spec.html HTTP/1.1
Host: www.example.com
Cookie: made_write_conn=1295214458; reg_fb_gate=deleted
...

{% endhighlight %}

**Types.** Cookies can be named based on their characteristics:

* Session and Persistent cookies: when a cookie doesn't have the expiration time set, it last only within the current session. That is, if the user reloads the page the cookie is gone. As opposed to a session cookie, a persistent cookie has the expiration time attribute set and will last until that time.

* Secure cookie: have the `Secure` attribute. It indicates that the browser should only use a cookie if it was sent in a secure connection (usually HTTPS).

* HTTP-Only cookie: have the `HttpOnly` attribute. It indicates that a cookie should only be transmitted via HTTP. This informs the browser to block non-HTTP APIs (such as JavaScript APIs) from modifying a cookie.

* First-party and Third-party cookies: if the Domain/Path attributes in the cookie is different from the server address, it's called a third-party cookie. This can be used for user tracking. For example, an ads agency can partner with several websites so they deliver cookies for this agency. It this way, the ads agency can track user activity across different sites.

This is a bit controversial. In Chrome there is an option to disallow third-party cookies.

> Settings &gt; Show Advanced Settings... &gt; Content Settings... &gt; Block third-party cookies and site data

A lot of websites use cookies for creating authenticated sessions. It's even more important to only use HTTPS connections in this scenario, because cookies are sent as plain text in the HTTP request header. There are many attacks that can be performed exploiting cookies:

**Man-in-the-middle attacks.** that can be used in a LAN network or public Wi-Fi network to hijack a cookie, by intercepting the HTTP requests and obtaining the cookies, as explained in detail [here](http://greyhatsspeak.blogspot.com/2013/07/sniffing-http-on-lan-mitm-attack.html).

**DNS Poisoning.** Since browsers use the Domain/Path to decide whether to send a cookie in a request, attackers can hack the DNS server to make the domain specified in the cookie point to the attacker server, which would send the cookies to the attacker. If it's an HTTPS connection, the request wouldn't go through because the attacker won't have a valid certificate.

**Cross-site Scripting.** The server might contain HTML poisoned with malicious JavaScript code which has access to cookies, and could send those as plain text to an attacker server:

{% highlight text %}

<a href="#">Click here!</a>

{% endhighlight %}

This would work even if the site we got the HTML from had a secure connection. This attack can be prevented if the cookies containing sensitive information have the `httpOnly` property.


### SPDY and HTTP/2

SPDY is a protocol created by Google aiming to improve the performance of HTTP requests. The overview provided in their [draft](https://tools.ietf.org/html/draft-mbelshe-httpbis-spdy-00#section-1) is very descriptive:

> 
> One of the bottlenecks of HTTP implementations is that HTTP relies on multiple connections for concurrency.  This causes several problems, including additional round trips for connection setup, slow-start delays, and connection rationing by the client, where it tries to avoid opening too many connections to any single server.  HTTP pipelining helps some, but only achieves partial multiplexing.
> 
> SPDY adds a framing layer for multiplexing multiple, concurrent streams across a single TCP connection (or any reliable transport stream).  The framing layer is optimized for HTTP-like request-response streams, such that applications which run over HTTP today can work over SPDY with little or no change on behalf of the web application writer.
> 
> The SPDY session offers four improvements over HTTP:
> 
> * Multiplexed requests: There is no limit to the number of requests that can be issued concurrently over a single SPDY connection.
> 
> * Prioritized requests: Clients can request certain resources to be delivered first.  This avoids the problem of congesting the network channel with non-critical resources when a high-priority request is pending.
> 
> * Compressed headers: Clients today send a significant amount of redundant data in the form of HTTP headers.  Because a single web page may require 50 or 100 subrequests, this data is significant.
> 
> * Server pushed streams: Server Push enables content to be pushed from servers to clients without a request.
> 

HTTP/2 is inspired on SPDY ideas. The majority of the browsers already support the HTTP/2 protocol, though only a bit over [6% of the websites use it](http://w3techs.com/technologies/details/ce-http2/all/all) as of January 2016.

### Conclusion

While reading the material for this post, we've learned a lot of things, including

* Details of TLS, plus history of SSL and TLS
* Symmetric key encryption
* Man in the middle attacks
* Several network interception tools
* Forward secrecy
* Third-party HTTP cookies

Regarding encryption algorithms, I was familiar with RSA, and heard about elliptic curve encryption, though I have no idea how they work. I'm interested in learning more about the [elliptic curve Diffie-Hellman](https://en.wikipedia.org/wiki/Elliptic_curve_Diffie%E2%80%93Hellman) algorithm.

There also several topics we didn't cover like HTTP pipeline or general web attacks, such as heartbleed. This [Wikipedia list](https://en.wikipedia.org/wiki/Category:Web_security_exploits) is an interesting follow-up reading.

Overall it was very interesting to read about internet security.

## Related Posts

Mentioned by [Network Sockets]({{blog}}/2020/03/07/sockets.html).

### References

* [[1](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)] Wikipedia - Hypertext Transfer Protocol
* [[2](https://en.wikipedia.org/wiki/HTTPS)] Wikipedia - HTTPS
* [[3](https://en.wikipedia.org/wiki/Transport_Layer_Security)] Wikipedia - Transport Layer Security
* [[4](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)] Wikipedia - Advanced Encryption Standard
* [[5](https://en.wikipedia.org/wiki/X.509)] Wikipedia - X.509 Standard
* [[6](https://technet.microsoft.com/en-us/library/cc781476(v=ws.10).aspx)] Microsoft - Overview of SSL/TLS Encryption
* [[7](https://letsencrypt.org/howitworks/technology/)] Let's Encrypt - Technical Overview
* [[8](https://en.wikipedia.org/wiki/HTTP_cookie)] Wikipedia - HTTP cookie
* [[9](http://greyhatsspeak.blogspot.com/2013/07/sniffing-http-on-lan-mitm-attack.html)] Grey Hats Speak - Sniffing HTTP On A LAN, MITM Attack
* [[10](https://en.wikipedia.org/wiki/SPDY)] Wikipedia - SPDY
* [[11](https://en.wikipedia.org/wiki/HTTP/2)] Wikipedia - HTTP/2

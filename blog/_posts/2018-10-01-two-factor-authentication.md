---
layout: post
title: "Two-factor authentication"
tags: [computer security]
---

In this post we'll talk about some popular security measures to protect user accounts on the web via two-factor authentication. The term refers to the requirement of two methods of authentication for logging in into a given account. The first method is mostly always a password, and the second is one of the methods we'll describe in this post.

### Why do we need an additional form of authentication?

In an ideal world, people would have strong ([long, not complex](https://xkcd.com/936/)) passwords, which would never get stolen and people would never forget them. In the real world, applications have to deal with two scenarios: 1) someone else knows your password or 2) you forgot your password.

**Scenario 1: They are not who they claim to be**


If someone else knows your password, the system needs to somehow know that this person is not you.

They can then employ a secondary method of authentication to verify that you are yourself. In theory they could ask for a secondary password or ask a security question. The problem with these is that they're exposed to the same set of vulnerability that might have compromised the original password in the first place, for example, the password is too easy to crack or there was a breach of database storing plain text passwords. In addition, since these secondary methods are to be used in very rare occasions, it's extremely likely you'll incur in the second problem, i.e. forget your password.

**Physical devices.** Nowadays, security systems can almost always rely on the fact that even if someone has your password, they do not have your physical belongings (e.g. cellphone). Some websites allow users to setup the requirement to use both a password *and* a secondary authentication to access the account.

**Scenario 2: I'm who I claim to be**


To address the problem of a user losing a password, some websites offers a recovery mechanism, usually by sending a secure email with a link to re-set the password or, in case of email applications like GMail, allowing the secondary authentication method as an alternative to inputing your password.

Websites such as GMail and Github also have a set of auto-generated "master passwords" that you can print and store in a safe place. After used, these passwords become invalid. This is one of the safest options, but it also requires more effort from the user (printing and making sure they can find the printed document when needed).

The ability to recover password is a necessary usability feature. This comes at a cost, though. As in a chain, the security system is as strong as its weakest link. If you have a way to recover the password of your online bank account via email, and there's a alternative authentication method to your email, then your bank account is vulnerable to the weakest between: your bank account password, your email password, or the secondary authentication mechanism used by your email.

Scenario 1 deals with security, and Scenario 2 deals with usability (recovering passwords), and these are usually at odds with which other. Security systems have to find the balance between the two.

We'll now cover three popular secondary authentication mechanisms: SMS (text messages), third-party app authentication and hardware authentication.

### SMS

<figure class="image_float_left">
    <a href="https://pixabay.com/en/text-mobile-chat-sms-980031/"><img src="{{site.url}}/resources/blog/2018-10-01-two-factor-authentication/2018_09_sms1.jpg" alt="sms" /></a>
</figure>

In the SMS (*Short Message Service*) method, the server generates a short code and sends it to the user via a text (SMS) message which is valid for a few minutes. The user can then copy the code from the phone to the computer and send the code to the server which can then authenticate the initial request.

During this period of time, the user account is technically vulnerable to a very weak code (a 6-digit number) which is very easy to crack. However, this period is very narrow, which great limits the ability of a bad agent to take any action.

**Vulnerabilities**


The real danger of the SMS method is a bad agent being able to intercept the SMS message that is supposed to go to the user. According to [this Wired article](https://www.wired.com/2017/05/fix-ss7-two-factor-authentication-bank-accounts/), the telecoms use a network called SS7 (*Signaling System No. 7*) to transport text messages. This network relies on trust to implement features such as roaming, which enables a person from New York to receive/send text messages when they're traveling to Berlin. In this case a carrier in Berlin could request the user's carrier back in New York to receive the text messages so it can deliver to the user.

This system has a vulnerability because a hacked carrier could be used to intercept the text messages by pretending it's doing so on behalf of a user. The carriers might not do any checks to verify the authenticity of the request. Hence, if an attacker knows your email, phone number and has access to a hacked carrier, they could technically hack into your account.

### App Authentication

<figure class="image_float_left">
    <img src="{{site.url}}/resources/blog/2018-10-01-two-factor-authentication/2018_09_google_auth.png" alt="google_auth" />
</figure> Another authentication method is to install a third-party app that can be used to generated the authentication codes. One popular option is the Google Authenticator App, which you can install on your phone (Android or iOS).

It uses the *Time-based One-time Password* algorithm or TOTP [2, 3]. The general idea is to perform a one-time registration between your phone and the server which consists of having both store a secret.

Whenever the client needs to authenticate itself, it uses the current timestamp and the secret to generate a hash, and from this hash it extracts a simpler code (6 characters) that the user copies and sends to the server. The server performs the same operation and if the generated code matches, it accepts the authentication.

The precision of the timestamp defines on how much time the user has to copy and send the code to the server. For example, the server can define the timestamp granularity to be 30 seconds. This also defines how long the server is vulnerable, since the code is usually short and hence easier to crack via brute force, so it cannot be too long.

### Hardware Authentication

<figure class="image_float_left">
    <a href="https://www.yubico.com/product/yubikey-4-series/"><img src="{{site.url}}/resources/blog/2018-10-01-two-factor-authentication/2018_09_yubikey.png" alt="yubikey" /></a>
</figure>

A more recent approach to authentication is using a dedicated piece of hardware. YubiKey is an example of such device, which can be connected to the USB port. One way it can be used is part of the open authentication protocol called [Universal 2nd Factor](https://www.yubico.com/solutions/fido-u2f/) (U2F), developed by Google and Yubico (the company that manufactures YubiKey). We'll describe this protocol next. In the discussion that follows we'll refer to the Yubikey device generically as **U2F**.

The general flow consists of a *enrollment phase*, where the use registers the U2F in the target webpage. The webpage asks for a confirmation, which the user can do by tapping the U2F, which sends some information to the webpage, which stores it.

The other part is the *signing phase*. When this webpage needs to verify the user, say during login, it can ask the user to tap the U2F, which will send information that can be validated by the webpage to make sure it's the same device that was registered in the first step.

**Implementation details**


One of the designs of this system is to be cross compatible and require no extra configuration from the user, like installing drivers. To achieve that, the communication between the U2F and the server is mediated via the browser. That means that the website calls a browser API (via JavaScript) which in turn communicates with the U2F. Henceforth when we refer to the communication between the U2F and the server, we're implicitly assuming it's done via the browser.

During the enrollment process, the device generates a pair of public and private keys ([public-key cryptography](https://en.wikipedia.org/wiki/Public-key_cryptography)). It sends the public key to the server which stores it together with other information. During the signing phase the server can generate a challenge (string), encrypt with the public key and send it to the U2F, which can decode it. At this point, the user is asked to tap the U2F. Once that it's done, it sends the challenge back to the server encrypted with its private key. If the server can then decode the message, it can trust the U2F and authenticate the user.

The reason a new public and private key is generated at every enrollment is for privacy reasons (not security). This is to prevent the case of different websites that enable U2F, to share data between them and be able to track the user. For example, if the same public key was used for all enrollments, a site A and B would be able to identify the user via their public key and share this information among themselves. If site A is a online shopping, it could use this information to show targeted ads in site B.

**Stateless U2F.** The problem of having to generate a pair of public/private keys every time is that now the U2F has to store them somehow. Since another important part of design is for the U2F to be very accessible, the implication is that they also have to be cheap. Hence, the protocol cannot assume the device has embedded storage. The solution is to send the pair for the server to store!

This seems to defeat the whole purpose of using cryptography but this information is sent to the server encrypted, which only the U2F itself can decode. Now, in addition to the server storing the public key, it has to store this extra information which the protocol calls *Key Handle* [5]. During the signing phase it sends not only the encrypted challenge, but also the *Key Handle*.

**Man-in-the-middle.** One potential security hole could be a scam website that looks like the real one and acts as a man-in-the-middle. First, the user will provide the scam site with the username and password. The scam site can then forward these to the real site to trigger the secondary factor request, which will send down the Key Handle and encrypted challenge. The scam site will forward it back to the U2F. Then the U2F would encrypt the challenge, which would be sent to the scam site, which in turn would relay it to the real site, finally allowing the bad actor to login as the user.

To prevent that, the site origin can be stored in the Key Handle as well. Before deciding to send data back, the U2F can check if the origin of the server and match it against the data in the Key Handle. The site origin is hard to tamper with when using an HTTPS connection unless the real site's certificates are compromised.

**Vendor reliability.** Another component of the security is the trust in the manufacturer of the device. It could have malicious intent or flawed implementation. To address that concern, the U2F should also contain an extra pair of **attestation** public-private pair of keys. The attestation is to prove the identity of the manufacturer/vendor. During the enrollment, the public key that is generated is encrypted with the private attestation key. The public attestation key is made available by some trusted organization for the server to consult. If it's able to decode the generated public key, then it can trust the U2F vendor.

### Conclusion

In this post we covered 3 methods of extra protection to the online identity. We saw that SMS has serious vulnerability while third-party and hardware authentication are much safer, which is no surprise since SMS were not initially designed to serve as a secure channel for authentication. No method is 100% secure but recent authentication mechanisms go to great lengths to reduce the vulnerable surface area to a minimum.

Note how all these methods assume the possession of a physical device separate from the computer where we're trying to log into. Physical devices are much harder to steal compared to piece of information like passwords.

### References

* [[1](https://security.stackexchange.com/questions/35157/how-does-google-authenticator-work)] Information Security - How does Google Authenticator work?
* [[2](https://en.wikipedia.org/wiki/HMAC-based_One-time_Password_algorithm)] Wikipedia - HMAC-based One-time Password algorithm
* [[3](https://en.wikipedia.org/wiki/Time-based_One-time_Password_algorithm)]  Wikipedia - Time-based One-time Password algorithm
* [[4](https://www.wired.com/2017/05/fix-ss7-two-factor-authentication-bank-accounts/)] Wired - Fixing the cell network flaw that lets hackers drain bank accounts
* [[5](https://docs.google.com/document/d/12AdwNDIhs6blXGTCOReaUGviBqCtsVrGMtrxGeCCxPU/edit)]  Google U2F (Gnubby) Documents - Snapshot prior to joining FIDO

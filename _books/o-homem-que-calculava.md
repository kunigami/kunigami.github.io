---
layout: books
title: "O Homem que Calculava"
vanity: o-homem-que-calculava
---

{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{site.url}}/resources/books/o-homem-que-calculava.jpg" alt="Book cover" />
</figure>

I read this book in Portuguese. This is a book by Malba Tahan which is the pen name of Júlio César de Mello e Souza. It recounts the adventures of the Persian mathematician known as Beremiz Samir, who astounds everyone with his mathematical prowess.

The backdrop of the story in the Abbasid Caliphate, under the reign of the sultan al-Mu'tasim in Baghdad. While most characters are fictitious, the historical context seems very well researched.

Throughout the book, several mathematical puzzles are presented as part of real problems Beremiz is asked to solve.

## Puzzles

**Problem 1.** Suppose a father leaves 35 camels to his three sons and wants them divided in proportion to 1/2, 1/3 and 1/9. How can one do it?

<spoiler>
Beremiz's solution is to add an imaginary camel, and make up 36. Then one part is 36/2 = 18, the other part is 36/3 = 12 and the last 36/9 = 4. But this adds up to 18 + 12 + 4 = 34, so Beremiz earns a spare camel for himself.
<br /><br />
The misleading factor here is that 1/2, 1/3 and 1/9 don't add up to 1, but to 17/18.
</spoiler>

**Problem 2.** In the story 3 people split a 25 dinars bill by putting down 10 dinars each. The waiter returned 5 dinar coins and accepted 2 dinars as tip. So each person got 1 dinar back and thus paid 9 dinars each, for a total of 27 dinars and the 2 dinars is with the waiter, adding up to 29 dinars. Where did the remaining dinar go?

<spoiler>
I've seen this problem many times. The wording of the problem is misleading. The trick is that the 2 to the waiter is expense, so it's included in the 27 dinars bill.
</spoiler>

**Problem 3.** The problem asks three sisters to sell 90 apples. The oldest must sell 50 of the apples, the middle one 30 and the youngest 10. But when they sell, they must sell at the same price. At the end they must make the same amount of money. How is it possible?

<spoiler>
The key of the solution is that they don't have to sell all their apples in one go and at the same price each time. A solution is for them to sell one batch, (49, 28, 7) for 1/7 dinars/apple and another (1, 2, 3) for 3 dinars/apple. So each makes 10 dinars in total.
</spoiler>

**Problem 4.** Suppose you have 10 beads and you want to line them up in 5 groups of 4. How to do it?

<spoiler>
The solution is to make them the vertices of a [pentagram](https://en.wikipedia.org/wiki/Pentagram). The key insight is that beads are shared between groups.
</spoiler>

**Problem 5.** A father left pearls to be divided among his daughters with the following process: the oldest takes one pearl + 1/7 of the remainder and passes to the next. The second eldest then takes 2 pearls + 1/7 of the remainder, until the youngest daughter takes the remainder. The father was fair with all his daughters. How many pearls and daughters are there?

<spoiler>
The answer is 36 and 6, respectively. This is easy to verify. We can find the number of pearls by comparing the number of pearls between the first and second daughter. Let $N$ be the number of pearls. The first daughter took: $1 + (N-1)/7$, and the remainder is $N - (N-1)/7$ or $(6N - 1)/7$.
<br /><br />
The second daughter took: $2 + ((6N - 1)/7 - 2)/7$ or $(6N - 20)/49$. Since the daughters got the same amount, we can solve for $N$ and find $36$ and that the first daughter received $6$ pearls so that must be $6$ daughters.

</spoiler>

## Topics

### Amicable Numbers

A pair of numbers $n$ and $m$ are considered amicable if the sum of the divisors of $n$ equals $m$ and vice versa.

Example: 220 is divisible by 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110, adding up to 284, whereas 284 is divisible by 1, 2, 4, 71 and 142, adding to 220, so 220 and 284 are amicable numbers.

According to [Wikipedia](https://en.wikipedia.org/wiki/Amicable_numbers) it's unknown whether there are infinitely many pairs of amicable numbers.

### Sessa, Chess and Exponentials

The book recounts the myth surrounding the creation of the game of chess in India (then called chaturanga) by the Brahmin [Sissa](https://en.wikipedia.org/wiki/Sissa_(mythical_brahmin)).

As part of the myth, the king decides to reward Sessa for inventing the game. Sessa asks that the king puts one grain of wheat on the first square of the chessboard and then two on the next, four on the next, doubling every time until the last square and gives him the amount as reward. The king laughs at him for such a modest request not realizing that at the end he will have to give Sessa 2^64 grains of wheat.

This reminded me of the book [The Art of Thinking Clearly](https://www.kuniga.me/books/the-art-of-thinking-clearly) which discusses different psychological traps such as not being able to grok exponentials.

### Stoicism

In a few passages references to stoicism can be noted. For example, Sessa declares:

> Only the true wise person, instructed in spiritual principles, rises above these vicissitudes and hovers above all these fluctuations.

Someone commented on a poor theologian and the response was:

> Allah deprives sages of riches, because wisdom and wealth rarely appear together.

### Bhaskara and Līlāvatī

I had heard of Bhaskara before, but not about the book Līlāvatī which according to the legend he wrote for his daughter, which has a [nice story](https://en.wikipedia.org/wiki/L%C4%ABl%C4%81vat%C4%AB).

### History of Numbers Systems

The book describes the history of how numbers came about, describing how the Greeks and Romans wrote down numbers and only later when the symbol for 0 was invented did we get our elegant decimal system.

### Cyclic Number

The number 142,857 is known as the *cyclic number* because when multiplied by n=2-6 causes it to "rotate" by some amount of digits, for example, 142,857 x 4 = 571,428 (3-digit right rotation).

The Portuguese edition calls it the *cabalistic number*, because when multiplied by 7 it gives out 999,999.

### Eratosthenes and Pentathlon

Tahan suggests that Eratosthenes was not only a mathematician, but also an athlete of pentathlon, which I found very interesting! Upon checking though, it looks like he was a polymath, so his followers nicknamed him *Pentathlos*, so probably why the author got confused.

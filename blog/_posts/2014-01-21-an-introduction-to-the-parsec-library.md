---
layout: post
title: "An Introduction to the Parsec Library"
tags: [automata, compilers, haskell, introduction, parsec]
---

### Introduction
The process of compiling or interpreting programs requires, as one of its steps, parsing the source code and structuring them. More specifically, we have a step to convert a string into a set of tokens, called lexical analysis, which is carried out by a **lexer** or **tokenizer**.

After that, we structure those tokens in a such a way that encodes meaning of these tokens, such as abstract syntax trees (AST). This step is called parsing and is out by a **parser**.

**The parsec library**

**Parse combinators** are high-order functions that combine smaller combinators (parsers) to build more complex ones. This resembles the idea of context free grammars (CFG), which we talked about in a [previous post](2013/12/28/turing-machines-and-undecidability/), where we have productions of form

$$S \rightarrow A \mid B$$

Here, $$S$$ can be formed by either from $$A$$ or $$B$$, which in turn, can be other productions or terminals.

The parsec library is an implementation of a parser combinator in Haskell. We talked about combinators in Haskell [previously](http://kuniga.wordpress.com/2012/08/05/paul-graham-e-combinatores-em-haskell/) (in portuguese).

**Parsec vs. Bison/Yacc/Antlr**

All Bison, Yacc and Antlr are not actual parsers, but rather parsers generators. They take a grammar file and generate parsers for the languages that can be described by those grammars.

Parsec, on the other hand, is a parser that you write yourself.

In this post we'll go through several basic concepts of the parsec library, using as the main reference, the book Real World Haskell, [Chapter 16](http://book.realworldhaskell.org/read/using-parsec.html).

The source code for all the examples to follow can be found on the blog's github [repository](https://github.com/kunigami/blog-examples/blob/master/2014-01-21-parsec/).
### Basic combinators
The basic idea of a parser is that it takes an input (for example, a string), it consumes the characters of this string until it's done with what it's supposed to parse and then pass the remaining (unparsed) string along, which might be used as input to subsequent parsers.

One of the simples parsers is the one that only consumes a single specific character. There's a parser named `char` at `Data.Char` library, so let's write one that parses the letter `a`:

{% highlight haskell %}

import Data.Char (char)
charAParser = (char 'a')

{% endhighlight %}

To test our parser with an input, we use the `parse` function form `Text.Parsec`

{% highlight haskell %}

import Text.Parsec
-- parsedInput = parse someParser "source name" "some input"

{% endhighlight %}

This function takes as input a parser, a source name and the actual input. The source name parameter is not important for us now, so we can pass the empty string. Let's write a simple wrapper to avoid boilerplate:

{% highlight haskell %}

test p = parse p ""

{% endhighlight %}

We can now test our parser with same sample inputs:

{% highlight haskell %}

> test charAParser "a"
Right 'a'
> test charAParser "ab"
Right 'a'
> test charAParser "ba"
Left (line 1, column 1):
unexpected "b"
expecting "a"

{% endhighlight %}

It extracts the first character of the input string if it's the `'a'` character, otherwise it throws an error. If we want to match any char, there's also the function `anyChar`. Running it with the same examples:

{% highlight haskell %}

> test anyChar "a"
Right 'a'
> test anyChar "ab"
Right 'a'
> test anyChar "ba"
Right 'b'

{% endhighlight %}

Note that it doesn't fail for strings starting with `'b'`. So far our parsers only match one character, so for example, the string `"ab"`, it only returns the first character.

We can use a parser for the string too. There's the `string` combinator but let's develop our own and show how we can combine combinators to form new ones. There's the `many` combinator that applies the combinator passed as argument until it fails.

Thus, we can write a string parser as `many anyChar`:

{% highlight haskell %}

stringParser:: Parsec String st String
stringParser = many anyChar

{% endhighlight %}

Now let's try it with the string `"ab"`:

{% highlight haskell %}

> test stringParser "ab"
Right "ab"

{% endhighlight %}

More useful than matching all characters is matching all except some, so we know when to stop parsing. For that, we can use `noneOf` instead of `anyChar`. It takes a list of characters as parameter and matches any character that is not on that list.

Let's now write a `wordParser`, which keeps parsing all characters until it finds an whitespace:

{% highlight haskell %}

wordParser:: Parsec String st String
wordParser = many $ noneOf [' ']

{% endhighlight %}

Let's try it on the most classical string example:

{% highlight haskell %}

> test wordParser "hello world"
Right "hello"

{% endhighlight %}

Note that our parsers are throwing away all the unparsed strings. How can we parse the remaining, unparsed string?
### The two flavors of the Parsec library: Monads and Applicatives Functors
We've talked about [Functors](2012/09/09/functors/) and [Monads](2013/05/26/monads-in-haskell-part-i/) before, but not about Applicatives functors.

Intuitively, they are a structure in between Functors and Monads, that is, they're more complex and general than Functors but less than Monads. We can also make the analogy of wrappers that we did for monads.

Originally, the Parsec library was written with Monads in mind, but Applicative functors were introduced after that and using them to write parsers usually leads to more clear syntax. So, in this post, we'll use the applicative flavor to write our parsers.

Here, we'll only provide an overview of some of the main applicative operators. For further details, the book [Learn You a Haskell for Great Good](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors) has a nice introduction to Applicatives.

**Operators Cheat Sheet.** We can use the `Maybe` typeclass to illustrate the main applicative operators, since it implements an applicative functor.

`()` Unwrap the contents of both sides, combine them and wrap again

{% highlight haskell %}

> Just (+3)  Just 9
Just 12

{% endhighlight %}

`(*>)` Unwrap the contents of both sides, but discard the result on the left

{% highlight haskell %}

> Just (+3) *> Just 9
Just 9
> Just 7 *> Just 8
Just 8

{% endhighlight %}

`(<*)` Unwrap the contents of both sides, but discard the result on the right.

{% highlight haskell %}

> Just 7 <* Just 8
Just 7

{% endhighlight %}

`()` Unwrap the contents of the right, combine the left and right arguments and return

{% highlight haskell %}

> (+3)  Just 9
Just 12

{% endhighlight %}

`(<$)` Unwrap the contents of the right, but only wrap the one to the left

{% highlight haskell %}

> 3 <$ Just 9
Just 3

{% endhighlight %}

This analogy of wrappers applied to parsers is not as natural though. In this case, we can think of unwrapping as executing the parser, by consuming the input and wrapping the result as getting the parsed token. The unparsed string is always carried over from parser to parser.

Hopefully with the next examples this will become clearer:

**Parsing the second word**

If we are to get the token from the second parser instead of the first, we need to execute both parsers but ignore the result of the first. Thus, we can use the operator `(*>)` to obtain something like

{% highlight haskell %}

wordParser *> wordParser

{% endhighlight %}

This won't quite work because the first parser doesn't consume the whitespace, so the second parser will stop before consuming anything. We can fix that by consuming the whitespace:

{% highlight haskell %}

wordParser *> (char ' ') *> wordParser

{% endhighlight %}

So let's write:

{% highlight haskell %}

secondWordParser:: Parsec String st String
secondWordParser = wordParser *> (char ' ')  *> wordParser

{% endhighlight %}

and now we can test:

{% highlight haskell %}

> test secondWordParser "ab cd"
> cd

{% endhighlight %}

**Parsing two words**

We can also return both tokens if we use the operator `()` and then combine them into a list:

{% highlight haskell %}

twoWordsParser:: Parsec String st [String]
twoWordsParser = listfy  wordParser  ((char ' ') *> wordParser)
                   where listfy a b = [a, b]

{% endhighlight %}

**Parsing multiple words**

Generalizing, we can parse multiple words with the aid of the `many` combinator:

{% highlight haskell %}

wordsParser:: Parsec String st [String]
wordsParser = (:)  wordParser  many ((char ' ') *> wordParser)

{% endhighlight %}

We could actually write this using the `sepBy1` parser, which parses a list of tokens separated by a separator and requires the list to have at least one element:

{% highlight haskell %}

wordsParserAlt:: Parsec String st [String]
wordsParserAlt = sepBy1 (char ' ')

{% endhighlight %}

**Simple CSV parser**

With what we've seen so far, we can write a very basic CSV parser in 4 lines of code.

{% highlight haskell %}

csvParser:: Parsec String st [[String]]
csvParser = lineParser `endBy` newline <* eof
              where lineParser = cellParser `sepBy` (char &#039;,&#039;)
                    cellParser = many $ noneOf ",\n"

{% endhighlight %}

Note that it doesn&#039;t handle some corner cases like escaped commas within cells. For a full example, refer to either [1] or [2].
### Choosing combinators
Recall that in Context Free Grammars, we can have production rules of the type:

$$S \rightarrow A \mid B$$

which means that S can be generated either from $$A$$ or $$B$$. In Parsec, we can express this option using the `()` operator. Let's write a simple parser that parses either the `"cat"` or `"dog"` strings:

{% highlight haskell %}

dogCatParser:: Parsec String st String
dogCatParser = (string "dog")  (string "cat")

{% endhighlight %}

Testing on some inputs:

{% highlight haskell %}

> test dogCatParser "dog"
Right "dog"
> test dogCatParser "cat"
Right "cat"
> test dogCatParser "elephant"
Left (line 1, column 1):
unexpected "e"
expecting "cat" or "dog"

{% endhighlight %}

Let's write another example with different animal names:

{% highlight haskell %}

camelCatParser:: Parsec String st String
camelCatParser = (string "camel")  (string "cat")

{% endhighlight %}

and try again with the input `"cat"`:

{% highlight haskell %}

> test camelCatParser "cat"
Left (line 1, column 1):
unexpected "t"
expecting "camel"

{% endhighlight %}

The parser failed because the strings have common prefix. It started matching the camel parser, but it also consumed the `"ca"` characters and then it failed to match the cat parser.

**The try combinator**

To avoid this, problem, there's the `try` combinator, which will make a parser to not consume its input if it fails to match:

{% highlight haskell %}

camelCatTryParser:: Parsec String st String
camelCatTryParser = try (string "camel")  (string "cat")

{% endhighlight %}

which works as expected:

{% highlight haskell %}

> test camelCatTryParser "cat"
Right "cat"

{% endhighlight %}

We can see that it's straightforward to convert a standard context free grammar into a haskell program using parsec.
### Simple Expression Parser
So far we our parsers have only returned strings and list of strings. We can use data types to structure our parsed data in a way that is easier to evaluate later.

For our example, we'll build a very simple parser for expressions that only contain `+` and `-` binary operators, terminals are all integers and all binaries are surrounded by parenthesis so we don't have to handle precedence. Examples of valid expressions are `"12"`, `"(1+2)"`, `"((3+4)-5)"`, whereas `"1+2"` is invalid (no parenthesis).

The first thing we want to do is to define our data types. Our number type, `TNumber`, is just an alias to `Int`. Our operator type, `TOperator` can be one of addition (`TAdd`) or subtraction (`TSubtract`). Finally, the expression is either binary (`TNode`) or a number (`TTerminal`).

{% highlight haskell %}

type TNumber = Int

data TOperator = TAdd
               | TSubtract
                 deriving (Eq, Ord, Show)

data TExpression = TNode (TExpression) TOperator (TExpression)
                 | TTerminal TNumber
                   deriving (Show)

{% endhighlight %}

From what we've seen so far, it's not very complicated to write parsers for `TNumber` and `TOperator`:

{% highlight haskell %}

numberParser:: Parsec String st TNumber
numberParser = read  (many $ oneOf "0123456789")

operatorParser:: Parsec String st TOperator
operatorParser = chooseOp  (oneOf "+-")
                   where chooseOp '+' = TAdd
                         chooseOp '-' = TSubtract

{% endhighlight %}

For the expression we have two choices. Either we parse another expression enclosed in parenthesis or we parse a terminal. In the first case, we call the `binaryExpressionParser` which looks for the left expression, the operator and then the right expression.

{% highlight haskell %}

expressionParser:: Parsec String st TExpression
expressionParser = (between (char '(') (char ')') binaryExpressionParser)
                   (TTerminal  numberParser)

binaryExpressionParser:: Parsec String st TExpression
binaryExpressionParser = TNode  expressionParser  operatorParser  expressionParser

{% endhighlight %}

And that's it! We can now run an example with a valid expression:

{% highlight haskell %}

> test expressionParser "(123+(324-456))"
Right (JNode (JTerminal 123) JAdd (JNode (JTerminal 324) JSubtract (JTerminal 456)))

{% endhighlight %}

The advantage of having this AST is that it's now very simple to evaluate:

{% highlight haskell %}

evaluate:: TExpression -> TNumber
evaluate (TNode exp1 TAdd exp2)      = (evaluate exp1) + (evaluate exp2)
evaluate (TNode exp1 TSubtract exp2) = (evaluate exp1) - (evaluate exp2)
evaluate (TTerminal v)               = v

{% endhighlight %}

And the final test:

{% highlight haskell %}

> let Right x = expressionParser "(123+(324-456))"
> evaluate x
-9

{% endhighlight %}

It works! We implemented a simple parser and interpreter for a very limited arithmetic expression. There are much better tools to do expression parsing (see * [[5]("http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements")] for a tutorial), but it's out of the scope of this post.
### Conclusion
We've learned the basics of the Parsec library and built some non-trivial parsers gluing together basic parsers using combinators. We even started scratching the parsing of programming languages by writing a parser for arithmetic expressions.

The Parsec applications presented in the Real World Haskell book are great. I felt that the content was a bit hard to follow, but writing helped me get a better understanding of the subject.
### References
* [[1]("http://book.realworldhaskell.org/read/using-parsec.html")] Real World Haskell - Chapter 16
* [[2]("https://www.barrucadu.co.uk/posts/2013-05-27-a-gentle-introduction-to-parsec.html")] A gentle introduction to Parsec
* [[3]("http://stackoverflow.com/questions/5055370/parsec-vs-yacc-bison-antlr-why-and-when-to-use-parsec")] StackOverflow - Parsec vs Yacc/Bison/Antlr: Why and when to use Parsec?
* [[4]("http://en.wikipedia.org/wiki/Parser_combinator")] Wikipedia - Parser Combinator
* [[5]("http://www.haskell.org/haskellwiki/Parsing_expressions_and_statements")] Haskell Wiki - Parsing expressions and statements

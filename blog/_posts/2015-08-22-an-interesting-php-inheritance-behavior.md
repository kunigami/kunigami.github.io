---
layout: post
title: "An interesting PHP Inheritance behavior"
tags: [php]
---

I've been working with PHP/Hack for a while. It's a relatively easy to use language, and since I've been using it in straightforward ways, I usually don't run into problems.

A few surprises I can remember I've had in the past include using references in [foreach](http://php.net/manual/en/control-structures.foreach.php), because the reference lives beyond the loop. Another was the difference between [self vs. static](http://stackoverflow.com/questions/5197300/new-self-vs-new-static).

More recently, I ran into an interesting behavior related to inheritance. In this post, we'll briefly describe the issue and understand the behavior. I've verified it with PHP5.6 using HHVM.

PHP supports single inheritance [[1](http://php.net/manual/en/language.oop5.inheritance.php)]. Deriving classes can overwrite the methods in the base class, as long as they have the same interface. They can relax the visibility, but not increase it (that is, private can become protected or public, and protected can become public).

{% highlight php %}

class Base {
  public function myMethod() {
    echo "Base\n";
  }
  public function run() {
    $this->myMethod();
  }
}

class Derived extends Base {
  public function myMethod() {
    echo "Derived\n";
  }
}
$d = new Derived();
$d->run();

{% endhighlight %}

The code above will print:

{% highlight text %}

Derived

{% endhighlight %}

Derived inherited the method `run()` from the base class, but since it overwrote `myMethod()`, since `$this` is an instance of `Derived`, `Derived::myMethod()` will be called. Now, consider a small modification. Let's make `Base::myMethod()` private:

{% highlight php %}

class Base {
  private function myMethod() {
    echo "Base\n";
  }
  public function run() {
    $this->myMethod();
  }
}

class Derived extends Base {
  public function myMethod() {
    echo "Derived\n";
  }
}

$d = new Derived();
$d->run();

{% endhighlight %}

The code above will print:

{% highlight text %}

Base

{% endhighlight %}

This was a bit surprising. The best explanation I found was this answer in StackOverflow [[2](http://stackoverflow.com/questions/3756609/strange-behavior-when-overriding-private-methods)]:

>
> Look in the method table of the class of the instance.
> * If this yields a private method:
>   * If the scope where the method was defined is the same as the scope of the calling function and is the same as the class of the object, use it.
>   * Otherwise, look in the parent classes for a private method with the same scope as the one of the calling function and with the same name.
>   * If no method is found that satisfies one of the above requirements, fail.
> * If this yields a public/protected method:
>   * If the scope of the method is marked as having changed, we may have overridden a private method with a public/protected method. So in that case, and if, additionally, there's a method with the same name that is private as is defined for the scope of the calling function, use that instead.
>   * Otherwise, use the found method.


Our examples fall in the second category. In the second example, the called method `$this->myMethod()` would resolve to `Derived::myMethod()`, which is public. But this method is marked as changed, since the visibility changed (from private to public).

The scope of the calling function, `run()`, is Base, because it's defined in that class. So, according to the rules, since `myMethod()` is also defined in `Base`, it's `Base::myMethod()` that gets called.

In the first example, since we kept the same function signature, we use the method found, which is `Derived::myMethod()` itself.

### Conclusion

In this post we covered one interesting behavior of inheritance in PHP. The explanation we found was taken from a StackOverflow, which unfortunately didn't provide any references.

There's an initiative of the PHP community in coming up with a PHP language spec [[3](https://github.com/php/php-langspec)], but I couldn't find anything explicitly covering this specific behavior.

### References

* [[1](http://php.net/manual/en/language.oop5.inheritance.php)] PHP - Object Inheritance
* [[2](http://stackoverflow.com/questions/3756609/strange-behavior-when-overriding-private-methods)] StackOverflow - Strange behavior when overriding private methods
* [[3](https://github.com/php/php-langspec)] PHP Language Spec

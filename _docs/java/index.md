---
layout: doc
title: "Java Cheatsheet"
---

# Basic Types


## List

Size:

{% highlight java %}
myList.size();
{% endhighlight %}

# Flow Control

## Exceptions

Basic try/catch:

{% highlight java %}
try {
    throw new Exception("Exception message");
} catch (Exception e) {
    e.printStackTrace();
}
{% endhighlight %}

# Object-Oriented

## Inheritance

{% highlight java %}
class Base {
}

class Child extends Base {
}

class Child implements Interface {

}
{% endhighlight %}

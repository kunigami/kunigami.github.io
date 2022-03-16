---
layout: doc
title: "Unit Test"
---

# Template

{% highlight python %}
import unittest
from unittest.mock import patch

class MyTestCase(unittest.TestCase):

  def test_my_method(self):
    self.assertEqual(1, 1)

  @patch('my.module.program.func')
  def test_my_method_with_path(self, mock_func):
    mock_func.return_value = 3

if __name__ == '__main__':
    unittest.main()
{% endhighlight %}

# Patching

We should always patch the variables in the scope of the program we're testing, even if itself imported them from somewhere else. Example:

`my/module/program.py`:

{% highlight python %}
from lib import func
{% endhighlight %}

It imported `func` from `lib`.

`test.py`:

{% highlight python %}

...
@patch('my.module.program.func')
def test_my_method(self, mock_func):
  pass
{% endhighlight %}

We patch `func` from `my.module.program`, not `lib`.

## Return Different Values

If we assign a list to `side_effect` it returns the i-th value each time the function is called.

`my/module/program.py`:

{% highlight python %}
def f():
  return 1

def g():
  print(f(), f(), f())
{% endhighlight %}

{% highlight python %}
from my.module.program import g
...

@patch('my.module.program.f')
def test_my_method(self, mock_func):
  mock_func.side_effect = [1, 2, 3]
  g() # prints 1 2 3
{% endhighlight %}

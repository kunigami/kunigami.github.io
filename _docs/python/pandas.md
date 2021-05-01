---
layout: doc
title: "Pandas"
---

## Create

From dictionary:

{% highlight python %}
d = {'col1': [1, 2], 'col2': [3, 4]}
df = pd.DataFrame(data=d)
{% endhighlight %}

From dictionary of numpy arrays:

{% highlight python %}
df = pd.DataFrame(
    {
        'a': np.array([1, 2, 3]),
        'b': np.array([4, 5, 6]),
    }
)
{% endhighlight %}


From numpy matrix:

{% highlight python %}
df = pd.DataFrame(
    np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
    columns=['a', 'b', 'c']
)
{% endhighlight %}

## Filter rows

Filter rows where a given column satisfies a predicate:

{% highlight python %}
filtered_df = df[df['value'] > 80]
{% endhighlight %}

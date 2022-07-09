---
layout: post
title: "From WordPress to Markdown"
tags: [meta, python]
---

I've recently [transitioned from writing on WordPress](https://kunigami.blog/2020/05/30/so-long-wordpress-and-thanks-for-all-the-fish/) to a self-hosted solution using GitHub pages, which in turn uses Jekyll for static content generation, which in turn uses Liquid for markdown processing.

There were about a hundred posts and my old blog and I wanted to bring them all into my new site to keep everything in one place although I'm keeping the wordpress blog for historical purposes).

In this meta post we'll describe some of the steps taken to convert WordPress posts into a markdown one and changes I needed to make to support features from WordPress.

I used Python for the processing.

## XML file

Wordpress offers a way to download all the posts and their metadata as XML which is very convenient. Python on its turn has a library to parse XML files:

{% highlight python %}
xml.etree.ElementTree
tree = ET.parse(filename)
root = tree.getroot()
{% endhighlight %}

`root` can be used to search elements within the XML tree. If you used JavaScript DOM APIs like `node.getElementById()`, the idea is very similar:

{% highlight python %}
root.findall('./channel/item')
{% endhighlight %}

One difficulty I had was matching tags containing namespaces such as:

{% highlight xml %}
<wp:post_type>post</wp:post_type>
{% endhighlight %}

Where `wp` is the namespace. Running:

{% highlight python %}
node.find("wp:post_type")
{% endhighlight %}

Won’t do, you need to provide an additional parameter, a list of the namespaces. They’re listed in the XML itself (`xmlns` attributes), in my case:

{% highlight xml %}
<rss version="2.0"
  xmlns:excerpt="http://wordpress.org/export/1.2/excerpt/"
  xmlns:wp="http://wordpress.org/export/1.2/"
>
{% endhighlight %}

And can be retrieved as

{% highlight python %}
namespaces = dict(
    [node for _, node in ET.iterparse(filename, events=['start-ns'])]
)
{% endhighlight %}

Finally we can search by `wp:post_type`:

{% highlight python %}
node.find("wp:post_type", namespaces)
{% endhighlight %}


## Translation

To facilitate the translation between wordpress HTML-ish (it also includes non-HTML tags such as `[sourcecode]`, `[caption]`, etc) and markdown I wrote a parser to generate an intermediate AST and then a markdown generator to convert the AST to markdown.

The hardest case was converting images with caption, since it had several cases some of which required re-structuring the AST.

{% highlight html %}
[caption]
  <a>
    <img />
  </a>
  Some caption
[/caption]
{% endhighlight %}

The ast was:

{% highlight javascript %}
{
  type: 'caption',
  children: [
    {type: 'a', children: [{type: 'img', props: ...}]},
    {type: 'text', text: 'some caption'}
  ]
}
{% endhighlight %}

Became

{% highlight html %}
<figure>
<a><img /></a>
<figcaption>Some caption</figcaption>
</figure>
{% endhighlight %}

There was also

{% highlight html %}
<a>
  <img />
</a>
{% endhighlight %}

To

{% highlight html %}
<figure>
  <a>
     <img />
  </a>
</figure>
{% endhighlight %}

And finally plain images

{% highlight html %}
<img />
{% endhighlight %}

to

{% highlight html %}
<figure>
  <img />
</figure>
{% endhighlight %}

I opted to wrap them all into {{ "<figure>" | escape }} for consistency and to have less output formats.


## Latex


Liquid has good integration with MathJax. The only change needed is to include

{% highlight html %}
<script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"></script>
{% endhighlight %}

[This post](https://quuxplusone.github.io/blog/2018/08/05/mathjax-in-jekyll/) was very helpful.

## Gists

I started using Gists in wordpress because the `[sourcecode][/sourcecode]` code from wordpress was subpar. Gists have their own set of issues, like the extra frame and sometimes failing to load on mobile. Liquid has nice support for code highlighting, so I just needed to download the contents.

Github has a REST API to download Gists but the rate limit is very low (1 gist/min) if logged out, while logged in it’s ~100x higher. The github official documentation was not very friendly on explaining how to programmatically authenticate with your own user, but [this post](https://dev.to/gr2m/github-api-authentication-personal-access-tokens-53kd) was very helpful.


We can then use the `requests` Python library to get the data:

{% highlight python %}
import requests

# Careful to not include your token in code!
token = os.environ['GITHUB_TOKEN']

resp = requests.get(
    f"https://api.github.com/gists/{gist_id}",
    headers={
        'content-type':'application/json',
        'accept':'application/vnd.github.v3+json',
        'Authorization': f"token {token}",
    }
)

if resp.status_code != 200:
    message = resp.json().get('message')
    raise Exception(f'Failed to fetch gist {gist_id}. Reason: {message}')

payload = resp.json()
file = list(payload.get('files').values())[0]

code = file['content']
lang = file['language']
{% endhighlight %}


## HTTPS

I wanted to change my blog to use HTTPS, since Chrome now shows a "not secure" warning on HTTP-only sites.

Github [supports HTTPS on its pages](https://docs.github.com/en/github/working-with-github-pages/securing-your-github-pages-site-with-https) but if you have custom domain it doesn’t work out of the box.

I have a custom domain, kuniga.me, which I register using DreamHost. I don’t fully grasp the intricacies of CNAME and A records, but [this post](https://medium.com/@melissamcewen/getting-github-pages-to-work-with-a-dreamhost-domain-5fcefac93063) made it work.

## Comments

I opted to use [Facebook's comment plugin](https://developers.facebook.com/docs/plugins/comments/) to allow social interaction with my posts. The reason is that most people have a Facebook account and it handles spam / bots. I might have been partial on that choice ;)

The integration is very easy. It associates the posts to a given URL, so I added the following Liquid snippet at the end of a post template:

{% highlight htnl %}
<div class="fb-comments" data-href="http://kuniga.me/{{ page.url }}" />
{% endhighlight %}

The only downside was to lose all the comments from previous posts.

## Analytics

I opted to use [Google Analytics](https://analytics.google.com/analytics/web/) functionality. I just need to embed some script on top of every page template and it automatically gathers data which is helpful to determine the popularity of the posts.

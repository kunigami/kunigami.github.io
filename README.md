http://kuniga.me/


## Details

* `_layout` includes the definition of templates. A template can customize the HTML structure and things like including JavaScript libraries.

## Building

### Dependencies

* Upgrade ruby. the MacOS version is outdated:

`brew install ruby`

* Use brew's version:

`echo 'export PATH="/usr/local/opt/ruby/bin:$PATH"' >> ~/.bash_profile`

* Install jekyll

`gem install --user-install bundler jekyll`

### Run site locally

* Get repo

`git clone git@github.com:kunigami/kunigami.github.io.git`

* Run website on localhost

`bundle exec jekyll serve`

## References

* https://jekyllrb.com/docs/installation/macos/

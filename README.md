Source code for: http://kuniga.me/


## Details

* `_layout` includes the definition of templates. A template can customize the HTML structure and things like including JavaScript libraries.

## Building

* Get repo

    git clone git@github.com:kunigami/kunigami.github.io.git

Setup:

    git config --local core.hooksPath .githooks/

### MacOS

* Upgrade ruby. the MacOS version is outdated:

`brew install ruby`

* Use brew's version:

`echo 'export PATH="/usr/local/opt/ruby/bin:$PATH"' >> ~/.bash_profile`

* Install dependencies:

`bundle install`

### Linux (Ubuntu)

* Get ruby dev:

`apt install ruby-dev`

* Install dependencies:

`bundle install`

## Run

* Run website on localhost

`bundle exec jekyll serve`

* Open `https://localhost:4000` in the browser

## References

* https://jekyllrb.com/docs/installation/macos/

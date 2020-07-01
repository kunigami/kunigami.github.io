---
layout: post
title: "Developing a project in Javascript"
tags: [javascript]
---

I've worked with several small Javascript side-projects. The amount of Javascript libraries and frameworks is overwhelming, especially in recent times.

In the past, I would write a couple of stand-alone Javascript files from scratch. As applications get bigger and more complex, new libraries for improving project development have been created.

I decided to look around for best practices to develop open source JavaScript applications these days. This post is a set of notes describing my findings.

We'll discuss libraries that solves different needs for software projects including libraries, modularization, automated building, linter and finally testing frameworks.

### Packages/Libraries

<figure class="image_float_left">
    <a href="https://kunigami.files.wordpress.com/4444/09/npm-logo.png"><img src="{{site.url}}/resources/blog/2016-02-15-developing-a-project-in-javascript/4444_09_npm-logo.png" alt="npm-logo" /></a>
</figure>

Javascript doesn't have an official package management. There has been an effort to standartize how Javascript libraries are distributed. With *Node.js*, came its package manager that npm (node package manager), that was initially indented for *Node.js* packages, but can also be used for general libraries, independent of *Node.js* itself.

To work with npm, we need to write a configuration file, called `package.json`. In this file, which is a JSON, we can define metadata when building a library, including title, version and the dependencies of other libraries. A sample configuration looks like this:

{% highlight js %}

{
  "name": "my_project",
  "version": "0.0.1",
  "description": "Description of my project",
  "devDependencies": {
    "browserify": "~5.11.0",
    "uglifyify": "^2.5.0"
  }
}

{% endhighlight %}

**Dependencies**



In the dependencies, we have to specify the versions. A version (more specifically semantic version or **semver**) consists of three parts numbers separated by `'.'`. The last number should be bumped on small changes, like bug fixes, without change on functionality. The middle number, aka minor version, should be bumped whenever new features are added, but that are back-compatible. Finally, the first number, aka major version, should be bumped whenever back-incompatible changes are made [[1](https://docs.npmjs.com/getting-started/semantic-versioning)].

In `package.json`, you can specify a hard-coded version number or be more relaxed. If we use the `'~'` in front of the version, for example `~5.11.0`, it means we accept the most recent version of form `5.11.x`. On the other hand, if we use the `'^'`, for example `^2.5.0`, we accept the most recent version of the form `2.x.x`.

The dependencies of a package can be either production or development dependencies. In our case, `browserify` and `uglify` are only used for building our package and not a dependency our code has, so it doesn't make sense to ship those to the user of the library.

To parse the configuration in `package.json`, we can run:

{% highlight text %}

npm install --save-dev

{% endhighlight %}

This will download the dependencies listed under devDependencies locally in the directory node_modules (created in the same directory the `package.json` is). To run the production dependencies, we can do:

{% highlight text %}

npm install --save

{% endhighlight %}

### Modules

Modules are useful for splitting code in related units and enables reuse. JavaScript doesn't have a native module system, so some libraries were built to address the modularization problem. There are three main types of module systems around: **AMD** (*Asynchronous Module Definition*), **CommonJS** and the **ES6 loader**. Addy Osmani discusses the differences between those in [[2](http://addyosmani.com/writing-modular-js/)].

There are several implementations for modules, including [RequireJS](http://requirejs.org/) (AMD), [browserify](http://browserify.org/) (uses the `node.js` module system, which uses `CommonJS`). [SystemJS](https://github.com/systemjs/systemjs) is able to work with all these different types.

I had been working with *browserify*, but it seems better to adopt the ES6 standards, so I've switched to *SystemJS*. Another advantage of SystemJS is that is also allows ES6 syntax by transpiling the code using [BabelJS](https://babeljs.io/).

To use *SystemJS* we need to define a configuration file (analogous to `package.json`), named [config.js](https://github.com/systemjs/systemjs/blob/master/docs/config-api.md) (don't worry about it for now).

**Exporting**



**Named exports.** We can have multiple export statements within a single file or provide all exports within a single statement [[3](https://github.com/ModuleLoader/es6-module-loader)]. Example:

{% highlight js %}

/**
 * my_module.js
 */
function foo() {
  console.log('foo');
}
function bar() {
  console.log('bar');
}
// Nested export
export {
  foo,
  // baz is what will be available externally
  bar as baz,
};
// Flat, inline export
export function derp() {
  console.log('derp');
}

{% endhighlight %}

**Default exports.** We can export default items in a module (the reason will be clear when we talk about importing next). We show the syntax for both the inline and the named exports:

{% highlight js %}

// Nested
export {
  foo as default,
};
// Inline export
export default function() {
  console.log('derp');
}

{% endhighlight %}

**Importing**



We have 3 basic ways to import from a module.

1. Name all items we want to pick from the module.

{% highlight js %}

import {foo, baz as 'bar'} from 'my_module';

{% endhighlight %}

2. Do not provide any specific item, in which case we'll import the default export:

{% highlight js %}

import the_default_export from 'my_module';
// Equivalent to
import {default as 'the_default_export'} from 'my_module'

{% endhighlight %}

3. Import all item from the module under a 'namespace', basically

{% highlight js %}

import * as everything from 'my_module'
// 'everything' is an object
// {
//    foo,
//    baz,
//    default,
//    derp
// }

{% endhighlight %}

**NPM Packages**



To be able to import NPM packages, we have to download them first and for that we can use the [jspm.io](http://jspm.io/) tool. For example, I was interested in the [point-in-polygon](https://www.npmjs.com/package/point-in-polygon) package. Instead of running the npm command, we can use *jspm*:

{% highlight text %}

// Download jspm
npm install --global jspm
// Install the desired npm package
jspm install npm:point-in-polygon

{% endhighlight %}

Running *jspm* will write to the `config.js` file (it creates one if it doesn't exist). This will write a map from where the module got installed and the name you can use in code to import it. Since npm packages use the *CommonJS* syntax and *SystemJS* understands it, in code we can simply do:

{% highlight js %}

import {default as pointInsidePolygon} from 'point-in-polygon';

{% endhighlight %}

### Building

The process of running commands like *SystemJS* can be automated. One idea is writing Makefiles to run command line. Another option is to use JavaScript frameworka, such as [Grunt](http://gruntjs.com/) and [Gulp](http://gulpjs.com/). In this post we'll stick to *Grunt*.

To configure a build, we need to provide another configuration file, called `Gruntfile.js` (should live in the same directory as the `package.json`). You provide an object to `grunt.initConfig()`, which contains tasks configurations.

{% highlight js %}

module.exports = function(grunt) {

  var taskList = ['systemjs', 'uglify'];

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    systemjs: {
        options: {
            'configFile': './config.js'
        },
        dist: {
            'src': '<root JS file>',
            'dest': '<compiled file with all dependencies together>'
        }
    },
  });

  grunt.loadNpmTasks("grunt-systemjs-builder");
  grunt.registerTask('default', ['systemjs']);
};
{% endhighlight %}

With `grunt.registerTask('default', ['systemjs'])` we're telling grunt to run the systemjs task whenever we run `grunt` from the command line.

It's possible to run grunt automatically upon changes to JS files via the watch task. First, we need to install the plugin:

{% highlight text %}

npm install grunt-contrib-watch --save-dev

{% endhighlight %}

Then we configure it in `Gruntfile.js`:

{% highlight js %}

grunt.initConfig({
    ...
    watch: {
      browser_js: {
        files: ['**/*.js', '!node_modules/**/*', '!dist/**/*'],
        tasks: taskList,
      }
    },
});
...
grunt.loadNpmTasks('grunt-contrib-watch');

{% endhighlight %}

Here `taskList` is an array of task names. It can be the same one provided to the `default` task. Make sure to blacklist some directories like `dist`, which is the output directory of the `systemjs` task (otherwise we'll get an infinite loop). Finally we run:

{% highlight text %}

grunt watch

{% endhighlight %}

Now, whenever we perform a change to any JS file it will run the task.

### Minification

Since Javascript code is interpreted on the client (browser), the source code must be downloaded from the server. Having a large source code is not efficient from a network perspective, so often these libraries are available as a minified file (often with extension `min.js` to differentiate from the unminified version).

The source code can be compressed by removing extra spaces, renaming variables, etc, without changing the program. One popular tool to achieve this is [UglifyJS](http://lisperator.net/uglifyjs/).

To use it with *Grunt*, we can install the `grunt-contrib-uglify` module:

{% highlight text %}

npm install grunt-contrib-uglify --save-dev

{% endhighlight %}

And in our `Gruntfile.js`:

{% highlight js %}

grunt.initConfig({
    ...
    uglify: {
        compact: {
            files: {
                './dist/<project>.min.js': ['./dist/<project>.js']
            }
        }
    },
    ...
}
grunt.loadNpmTasks('grunt-contrib-uglify');
grunt.registerTask('default', ['systemjs', 'uglify']);

{% endhighlight %}

### Linting

Lint tools help us avoiding bugs, sticking to code conventions and improving code quality. One popular tool for linting is [jshint](http://jshint.com/). Other alternatives include [jslint](http://www.jslint.com/). *JSHint* has a *Grunt* plugin:

{% highlight js %}

grunt.initConfig({
    ...
    jshint: {
        files: [
            '**/*.js',
            '!node_modules/**/*',
            '!jspm_packages/**/*',
            '!dist/**/*'
        ],
        options: {
            'esnext': true,
        }
    },
    ...
}
grunt.loadNpmTasks('grunt-contrib-jshint');
grunt.registerTask('lint', ['jshint']);

{% endhighlight %}

The basic configuration here makes sure to blacklist "production" directories like `node_module` and `dist`. Also, since we've been adopting ES6, we can set the `esnext` flag to tell *jshint* to account for the new syntax.

We probably don't want to run the lint every time we update the JS file. We can run it less often, for example before sending code for review. Thus, we can create a separate registry for it using `grunt.registerTask('lint', ['jshint'])`. We can now run *jshint* via the command line:

{% highlight js %}

grunt lint

{% endhighlight %}

### Testing

Another practice to avoid bugs is testing, including unit tests. Again, there are several libraries and frameworks that makes the job of unit testing less painful, for example easy ways to mock dependencies so we can test isolated functionality. In this case, I've picked [Jest](https://facebook.github.io/jest/), which has a grunt task available in npm, which we can install via:

{% highlight js %}

npm install grunt-jest --save-dev

{% endhighlight %}

(NOTE: this will also install the `jest-cli` binary which depends on a Node.js version `>= 4`, so you might need to update your Node.js).

We can configure the grunt task with default configs in the following way:

{% highlight js %}

grunt.initConfig({
    ...
    jest: {
    },
    ...
}
grunt.loadNpmTasks('grunt-jest');

{% endhighlight %}

With this setup we can run the following command to run jest tests:

{% highlight js %}

grunt jest

{% endhighlight %}

Unfortunately, jest uses the *CommonJS* require syntax. It used to be possible to use `babel-jest` but after version 5.0 this setup [doesn't work anymore](https://github.com/babel/babel-jest/issues/16).

### Conclusion

The JavaScript environment changes extremely fast and it's very hard to keep on top of the latest frameworks/practices, etc.

To make things worse, for every task like module system, linting, testing, there are many alternatives and none of them is a clear best choice.

I'm happy that there's an effort of standardization with ES6. I think the more we stick to one convention the more we reduce re-inventing the wheel, the less  syntax differences to learn, etc.

### References

* [[1](https://docs.npmjs.com/getting-started/semantic-versioning)] Semantic versioning and npm
* [[2](http://addyosmani.com/writing-modular-js/)] Writing Modular JavaScript With AMD, CommonJS &amp; ES Harmony
* [[3](http://www.2ality.com/2014/09/es6-modules-final.html)] ECMAScript 6 modules: the final syntax

### Further Reading

[Generation Javascript](http://manuel.bernhardt.io/2014/12/30/generation-javascript/). Manuel Bernhardt discusses the current state of JavaScript libraries and how the low friction nature of developing in JavaScript has its downsides.

[Essential JavaScript Links](https://github.com/ericelliott/essential-javascript-links#essential-javascript-links). Eric Elliott's list of links to several books, articles and tools concerning to JavaScript. It provides a much more comprehensive list of options for the different topics we covered in this post.
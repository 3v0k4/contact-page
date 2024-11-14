---
title: Ruby gem activation
description:
author: Riccardo
tags:
  - Ruby
---

This is how you use a Ruby gem usually:

```ruby
require 'foo'
Foo.bar
```

For the `require` to work, the gem needs to be available on the `LOAD_PATH`:

```bash
ruby -e 'puts $LOAD_PATH'
```

The act of adding a gem to the `LOAD_PATH` is called "activating a gem".

In a vanilla Ruby situation, you can add a directory to the `LOAD_PATH` with:

```bash
ruby -I ./foo/lib -e "puts(require 'foo')"
# true
```

More often, you deal with a bundle that contains a Gemfile:

```ruby
gem 'foo'
```

To activate the gems from the Gemfile (taking into account the versions from Gemfile.lock):

```ruby
require 'bundler/setup'
require 'foo'
```

or (equivalent):

```ruby
require 'bundler'
Bundler.setup
require 'foo'
```

Notice that the `setup` step will first clear the `LOAD_PATH` and activate *only* the gems in the bundle.

For example, Rails does it [here](https://github.com/rails/rails/blob/9a9222874ade2d6f300647897a8ad99e1f05aa32/railties/lib/rails/generators/rails/app/templates/config/boot.rb.tt#L3) and [here](https://github.com/rails/rails/blob/9a9222874ade2d6f300647897a8ad99e1f05aa32/railties/lib/rails/generators/rails/app/templates/config/application.rb.tt#L7).

If you've ever wondered what the hell is up with `bundle exec ...`, the answer is simple: Bundler activates the gems in the bundle with something like

```bash
RUBYOPT="-rbundler/setup" ruby ...
```

Of course, things are more complex than that. But this should give you a starting point.

[Bundler: How to use Bundler with Ruby](https://bundler.io/guides/bundler_setup.html)

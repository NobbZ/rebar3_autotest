rebar3_autotest
===============

A rebar3 plugin to run tests automatically when there are changes.

Build
-----

```sh
$ rebar3 compile
```

Installing
----------

This plugin is meant to be used from your global rebar3 configuration, but of course you can add it to your projects
plugin list also.

After you added it to your plugin list, it will be pulled by rebar3 when necessary.

### Latest version from [hex.pm](http://hex.pm/)

Add the plugin to your rebar config:

```erl
{plugins, [{rebar3_autotest, "0.1.1"}]}.
```

### Latest version from [github](https://github.com/NobbZ/rebar3_autotest)

Add the plugin to your rebar config:

```erl
{plugins, [
  {rebar3_autotest, "0\.1\..*", {git, "https://github.com/NobbZ/rebar3_autotest.git", {branch, "master"}}}
]}.
```

### Latest development version from [github](https://github.com/NobbZ/rebar3_autotest)

Be aware of the fact, that living on the edge may let you use versions which are heavily bugged!

Add the plugin to your rebar config:

```erl
{plugins, [
  { rebar3_autotest, ".*", {git, "https://github.com/NobbZ/rebar3_autotest.git", {branch, "develop"}}}
]}.
```

Usage
-----

Then just call the plugin directly in an existing application:

```sh
$ rebar3 autotest
===> Fetching autotest
===> Compiling autotest
<Plugin Output>
```

While it is running, it will restart `rebar3`'s `eunit` command whenever files below `src`, `include`, `priv`, or `test`
have changed.

You can stop it by pressing `CTRL`-`C` twice, or since it is running in an REPL-process, just enter `q().` as you would
in `erl`.

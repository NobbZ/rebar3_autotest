myplugin
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { myplugin, ".*", {git, "git@host:user/myplugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 myplugin
    ===> Fetching myplugin
    ===> Compiling myplugin
    <Plugin Output>

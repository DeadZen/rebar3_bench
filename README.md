rebar3_bench
=====

A rebar plugin for performing benchmarks more easily.

Make sure to install R to generate the pretty graphs.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_bench, ".*", {git, "git://github.com/DeadZen/rebar3_bench.git", {branch, "master"}}}
    ]}.

Configure
---------

Configure your benchmarks

    {benchmarks, [
        {ets, "./test/ets_bench.config"}
    ]}.


Then just start your benchmark

    $ rebar3 bench ets

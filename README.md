erl_tidy
=====

Format your Erlang modules.

Use
---

Add the following to your `rebar.config`

```erlang
{plugins, [{erl_tidy_prv_fmt, ".*", {git, "git://github.com/tsloughter/erl_tidy.git", {branch, "master"}}}]}.
```

And run:

    $ rebar3 fmt

Run `rebar3 help fmt` to learn about formatting options.

You also may specify options in your `rebar.config` like so:
```erlang
{fmt_opts, [
  {auto_list_comp, false},
  {keep_unused, true}
]}.
```
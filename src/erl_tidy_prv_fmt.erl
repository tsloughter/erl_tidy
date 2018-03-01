-module(erl_tidy_prv_fmt).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, fmt).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:state()) -> {ok, rebar_state:state()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 fmt"},
                                                               {short_desc, "format modules."},
                                                               {desc, ""},
                                                               {opts, fmt_opts()}])),
    {ok, State1}.

-spec do(rebar_state:state()) -> {ok, rebar_state:state()}.
do(State) ->
    ConfigFileOpts = rebar_state:get(State, fmt_opts, []),
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    % CLI opts take precedence over config file
    Opts = rebar_utils:tup_umerge(ConfigFileOpts, CliOpts),

    ProjectApps = rebar_state:project_apps(State),
    format_apps(Opts, ProjectApps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

format_apps(Opts, Apps) ->
    lists:foreach(fun(AppInfo) ->
                          SrcDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
                          rebar_log:log(info, "Formating ~s...", [rebar_app_info:name(AppInfo)]),
                          erl_tidy:dir(SrcDir, Opts)
                  end, Apps).

fmt_opts() ->
    [{test, undefined, "test", {boolean, false},
      "do not modify files"},
     {verbose, undefined, "verbose", {boolean, true},
      "progress messages will be output while the program is running, "
      "unless the `quiet' option is set"},
     {quiet, undefined, "quiet", {boolean, false},
      "all information messages and warning messages will be suppressed"},
     {auto_list_comp, undefined, "auto_list_comp", {boolean, false},
      "calls to `lists:map/2' and `lists:filter/2' will be rewritten "
      "using list comprehensions"},
     {keep_unused, undefined, "keep_unused", {boolean, false},
      "unused functions will not be removed from the code"},
     {new_guard_tests, undefined, "new_guard_tests", {boolean, true},
      "guard tests will be updated to use the new names, "
      "e.g. `is_integer(X)' instead of `integer(X)'"},
     {old_guard_tests, undefined, "old_guard_tests", {boolean, false},
      "guard tests will be changed to use the old names "
      "instead of the new ones, e.g. `integer(X)' instead of `is_integer(X)'"},
     {no_imports, undefined, "no_imports", {boolean, false},
      "all import statements will be removed "
      "and calls to imported functions will be expanded "
      "to explicit remote calls"}
    ].

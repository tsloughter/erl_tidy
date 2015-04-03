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
    State1 = rebar_state:add_provider(State, rebar_provider:create([{name, ?PROVIDER},
                                                                    {module, ?MODULE},
                                                                    {bare, false},
                                                                    {deps, ?DEPS},
                                                                    {example, "rebar3 fmt"},
                                                                    {short_desc, "format modules."},
                                                                    {desc, ""},
                                                                    {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:state()) -> {ok, rebar_state:state()}.
do(State) ->
    ProjectApps = rebar_state:project_apps(State),
    format_apps(ProjectApps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

format_apps(Apps) ->
    lists:foreach(fun(AppInfo) ->
                          SrcDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
                          rebar_log:log(info, "Formating ~s...", [rebar_app_info:name(AppInfo)]),
                          erl_tidy:dir(SrcDir, [])
                  end, Apps).

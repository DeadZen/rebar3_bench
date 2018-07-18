-module(rebar3_bench_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bench).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 bench ..."},
            {opts, []},
            {short_desc, "A rebar plugin for running custom benchmarks"},
            {desc, "A rebar plugin for running custom benchmarks"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:add_deps_to_path(State),
    Config = rebar_state:get(State, benchmarks, []),
    {Args, _} = rebar_state:command_parsed_args(State),
    Dir = rebar_state:dir(State),
    App = hd(rebar_state:project_apps(State)),
    PluginDeps = rebar_state:all_plugin_deps(State),

    code:add_path(rebar_app_info:ebin_dir(App)),
    case handle_args(Args, Config) of
        {ok, _} -> file:set_cwd(Dir),
                   maybe_generate(PluginDeps);
        {error, _} ->
            rebar_api:info("Configured benchmarks are: ~p~n", [benchmarks(Config)])
    end,
    {ok, State}.


handle_args([], Config) ->
    case benchmarks(Config) of
        [] ->
            rebar_api:error("No available benchmarks to execute~n", []),
            {error, undefined};
        _ ->
            rebar_api:error("No benchmark specified to execute~n", []),
            {error, undefined}
    end;
handle_args(Args, Config) ->    
    {_, Benchmark} = proplists:lookup(task, Args),
    case lists:keyfind(list_to_atom(Benchmark), 1, Config) of
        {_, Command} ->
            rebar_api:debug("Running ~p benchmark ~n", [Benchmark]),
            application:ensure_all_started(folsom),
            basho_bench:main(Command),
            {ok, Benchmark};
        _ ->
            rebar_api:error("Specified benchmark ~p not found~n", [Benchmark]),
            {error, undefined}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

benchmarks(Config) ->
    [ element(1, B) || B <- Config ].

maybe_generate(false) ->
    rebar_api:error("Problem generating graph: ~p", [{error, benchmarker_notfound}]);
maybe_generate(PluginDeps) ->
    {ok, {Ret, _}} = exec("which Rscript"),
    BenchApp = lists:keyfind(<<"basho_bench">>, 2, PluginDeps),
    Priv = rebar_app_info:priv_dir(BenchApp),
    R = filename:join([Priv, "summary.r"]),
    case Ret of
            1 -> rebar_api:info("Please install R to auto-generate benchmark graphs");
            0 -> exec("Rscript --vanilla " ++ R ++ " -i tests/current"),
                 determine_outcome()
    end,
    ok.

determine_outcome() ->
    case exec("test -f tests/current/summary.png") of
	{ok, {0, _}} ->
	    rebar_api:info("Completed generating graph: tests/current/summary.png", []);
	{ok, {_, Problem}} ->
	    rebar_api:error("Problem generating graph: ~p", [Problem])
    end.

exec(Run) ->
    rebar_api:debug("Running ~p", [Run]),
    Port = open_port({spawn, Run}, [stream, in, eof, hide, exit_status]),
    {RC, RD} = get_data(Port, []),

    case RC of
        0 -> ok;
        _ -> rebar_api:debug("Non-zero exit code returned: ~p", [RC])
    end,
    io:format("~s", [RD]),
    {ok, {RC, RD}}.	

get_data(Port, Acc) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Acc|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            ExitCode = receive
                           {Port, {exit_status, Code}} ->
                               Code
                       end,
            {ExitCode, Acc}
    end.

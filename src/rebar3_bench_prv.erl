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
    handle_args(Args, Config),
    {ok, State}.


handle_args([], Config) ->    
    case [ element(1, B) || B <- Config ] of
        [] -> 
            rebar_api:error("No available benchmarks to execute~n", []);
        Benchmarks -> 
            rebar_api:error("No benchmark specified to execute~nConfigured benchmarks: ~p.~n", [Benchmarks])
    end;
handle_args(Args, Config) ->    
    {_, Benchmark} = proplists:lookup(task, Args),
    case lists:keyfind(list_to_atom(Benchmark), 1, Config) of
        {_, Command} ->
            rebar_api:debug("Running ~p benchmark ~n", [Benchmark]),
            application:ensure_all_started(folsom),
            basho_bench:main(Command),
            ok = maybe_generate();
        _ ->
            Benchmarks = [ element(1, B) || B <- Config ],
            rebar_api:error("Specified benchmark not found~nConfigured benchmarks: ~p~n", [Benchmarks])
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


maybe_generate() -> 
	{ok, {Ret, _}} = exec("which Rscript"), 
	R = "_build/default/plugins/basho_bench/priv/summary.r",
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


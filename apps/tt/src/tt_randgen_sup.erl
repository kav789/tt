%%%-------------------------------------------------------------------
%% @doc tt randgen supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tt_randgen_sup).
-behaviour(supervisor).
-export([start_link/0, stop/1]).
-export([init/1]).
-define(SERVER, ?MODULE).
-define(Psec, 1000).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(Pid) ->
	exit(Pid, kill).

init([]) ->
	{ok,App} = tt_lib:get_application(),
	{ok,Rps} = application:get_env(App,confRandRPS),
	Rl = [X || X <- lists:seq(1,min(?Psec,Rps)), (?Psec rem X == 0) and (Rps rem X == 0) and (?Psec div X > 1) and (Rps div X > 0)],
	Rlm = if
		length(Rl) == 1 -> lists:nth(1, Rl);
		true -> lists:nth((length(Rl) div 2), Rl)
	end,
	N   = Rps  div Rlm,
	To  = 1000 div Rlm,
	SupFlags = #{strategy => one_for_one, intensity => 2, period => 1000},
	ChildSpecs = [
		#{id => I,
		start => {tt_randgen_wrk, start_link, [I,To]},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [tt_randgen_wrk]}
	|| I <- lists:seq(1,N)],
%	ChildSpecs = [],
	{ok, {SupFlags, ChildSpecs}}.



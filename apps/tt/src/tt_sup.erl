%%%-------------------------------------------------------------------
%% @doc tt top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tt_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{strategy => one_for_one, intensity => 2, period => 1000},
	ChildSpecs = [
		#{id => tt_filter_sup,
		start => {tt_filter_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [tt_filter_sup]}
		,
		#{id => tt_randgen_sup,
		start => {tt_randgen_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [tt_randgen_sup]}
	],
	{ok, {SupFlags, ChildSpecs}}.




%%%-------------------------------------------------------------------
%% @doc tt filter supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tt_filter_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{strategy => one_for_one, intensity => 2, period => 1000},
	ChildSpecs = [
		#{id => tt_filter_wrk,
		start => {tt_filter_wrk, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [tt_filter_wrk,eredis]}],
	{ok, {SupFlags, ChildSpecs}}.



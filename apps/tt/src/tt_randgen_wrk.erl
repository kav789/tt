%%%-------------------------------------------------------------------
%% @doc tt randgen worker
%% @end
%%%-------------------------------------------------------------------

-module(tt_randgen_wrk).
-behaviour ( gen_server ).
-export([start_link/2, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {to,lim,qk,rc,i}).

start_link(I,To) ->
	gen_server:start_link(?MODULE, [I,To], []).

start(I,To) ->
	gen_server:start(?MODULE, [I,To], []).

stop(Pid) ->
	gen_server:call(Pid, stop).

init([I,To]) ->
	{ok,App}   = tt_lib:get_application(),
	{ok,Rhost} = application:get_env(App,confRedisHost),
	{ok,Rport} = application:get_env(App,confRedisPort),
	{ok,Rdb}   = application:get_env(App,confRedisDB),
	{ok,Lim}   = application:get_env(App,confN),
	{ok,Qk}    = application:get_env(App,confQueueKey),
	{ok,Rc}    = eredis:start_link(Rhost,Rport,Rdb),
	S = #state{to = To, lim = Lim, qk = Qk, rc = Rc ,i = I},
	{ok, S, 0}.

handle_call(stop, _F, S) ->
	eredis_client:stop(S#state.rc),
	{stop, normal, ok, S};
handle_call(_M, _F, S) ->
	{noreply, S, S#state.to}.

handle_cast(_M, S) ->
	{noreply, S, S#state.to}.

handle_info(timeout, S) ->
	T0 = erlang:monotonic_time(),
	{ok,_} = eredis:q(S#state.rc, ["LPUSH", S#state.qk, rand:uniform(S#state.lim - 1)+1]),
	{noreply, S, S#state.to - ((erlang:monotonic_time() - T0) div 1000000) -1 };
handle_info(_Message, S) ->
	{noreply, S, S#state.to}.

terminate(_R, _S) -> ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


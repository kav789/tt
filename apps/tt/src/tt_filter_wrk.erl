%%%-------------------------------------------------------------------
%% @doc tt filter worker.
%% @end
%%%-------------------------------------------------------------------

-module(tt_filter_wrk).
-behaviour ( gen_server ).
-define(SERVER, ?MODULE).
-export([start_link/0,  stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {rsk,qk,rc}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(Pid) ->
	gen_server:call(Pid, stop).

init([]) ->
	{ok,App}   = application:get_application(),
	{ok,Rhost} = application:get_env(App,confRedisHost),
	{ok,Rport} = application:get_env(App,confRedisPort),
	{ok,Rdb}   = application:get_env(App,confRedisDB),
	{ok,Qk}    = application:get_env(App,confQueueKey),
	{ok,Rsk}   = application:get_env(App,confResultSetKey),
	{ok,Rc}    = eredis:start_link(Rhost,Rport,Rdb),
	S = #state{ qk = Qk,  rsk = Rsk, rc = Rc },
	{ok, S, 0}.

handle_call(stop, _F, S) ->
	eredis_client:stop(S#state.rc),
	{stop, normal, ok, S};
handle_call(_M, _F, S) ->
	{noreply, S, 0}.

handle_cast(_M, S) ->
	{noreply, S, 0}.

handle_info(timeout, S) ->
	try
		{ok,Ret} = eredis:q(S#state.rc, ["BLPOP", S#state.qk, 1]),
		if
			Ret /= undefined ->
			N = list_to_integer(binary_to_list(lists:nth(2,Ret))),
			spawn_link( fun() -> isprime(N) andalso  (?MODULE ! {primetest,N,true}) end );
			true -> ok
		end
	catch
		exit:{timeout, _} -> ok
	end,
	{noreply, S, 0};
handle_info({primetest, N, true}, S) ->
	{ok,_} = eredis:q(S#state.rc, ["SADD",S#state.rsk, N]),
	{noreply, S, 0};
handle_info(_M, S) ->
	{noreply, S, 0}.

terminate(_R, _S) -> ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

isprime(N) when N == 2 -> true;
isprime(N) when N == 3 -> true;
isprime(N) when N rem 2 == 0 -> false;
isprime(N) when N rem 3 == 0 -> false;
isprime(N) -> isprime(N,6,trunc(math:sqrt(N))+1).

isprime(_,I,I2) when I - 1 >= I2 -> true;
isprime(N,I, _) when N rem (I + 1) == 0 -> false;
isprime(N,I, _) when N rem (I - 1) == 0 -> false;
isprime(N,I,I2) -> isprime(N,I+6,I2).


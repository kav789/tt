%%%-------------------------------------------------------------------
%% @doc tt filter worker.
%% @end
%%%-------------------------------------------------------------------

-module(tt_filter_wrk).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-behaviour ( gen_server ).
-define(SERVER, ?MODULE).
-export([start_link/0, start/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {rsk,qk,rc,rcw,chc,to}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
	gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop(Pid) ->
	gen_server:call(Pid, stop).

init([]) ->
	{ok,App}   = tt_lib:get_application(),
	{ok,Rhost} = application:get_env(App,confRedisHost),
	{ok,Rport} = application:get_env(App,confRedisPort),
	{ok,Rdb}   = application:get_env(App,confRedisDB),
	{ok,Qk}    = application:get_env(App,confQueueKey),
	{ok,Rsk}   = application:get_env(App,confResultSetKey),
	process_flag(trap_exit, true),
	{ok,Rc}    = eredis:start_link(Rhost,Rport,Rdb),
	{ok,Rcw}   = eredis:start_link(Rhost,Rport,Rdb),
	{ok, #state{ qk = Qk,  rsk = Rsk, rc = Rc, rcw = Rcw, chc = 0,to = 0 }, 0}.

handle_call(stop, _F, S) ->
	if
		S#state.chc == 0 ->
			{stop, normal, ok, S};
		true ->
			{noreply, S#state{to = infinity},infinity}
	end;
handle_call(_M, _F, S) ->
	{noreply, S, S#state.to}.

handle_cast(_M, S) ->
	{noreply, S, S#state.to}.

handle_info(timeout, S) ->
	try
		{ok,Ret} = eredis:q(S#state.rc, ["BLPOP", S#state.qk, 1]),
		if
			Ret =/= undefined ->
				N = list_to_integer(binary_to_list(lists:nth(2,Ret))),
				spawn_link( fun() -> isprime(N) andalso  ( {ok,_} = eredis:q(S#state.rcw, ["SADD",S#state.rsk, N]) ) end ),
				{noreply, S#state{chc = S#state.chc + 1}, S#state.to};
			true ->
				{noreply, S, S#state.to}
		end
	catch
		exit:{timeout, _} ->
			{noreply, S, S#state.to}
	end;
handle_info({'EXIT',Pid,_}, S) ->
	Chc =
	if
		(Pid =/= S#state.rc) and (Pid =/= S#state.rcw) ->
			S#state.chc - 1;
		true ->
			S#state.chc
	end,
	if
		(S#state.to =:= infinity ) and ( Chc == 0) ->
			{stop,normal, S};
		true ->
			{noreply, S#state{chc = Chc}, S#state.to}
	end;
handle_info(_M, S) ->
	{noreply, S, S#state.to}.

terminate(R, S) ->
	eredis_client:stop(S#state.rc),
	wterminate(R,S).


wterminate(R,S) ->
	if
		S#state.chc > 0 ->
			receive
				{'EXIT',Pid,_} ->
					if
						(Pid =/= S#state.rc) and (Pid =/= S#state.rcw) ->
							wterminate(R,S#state{chc = S#state.chc -1 });
						true ->
							wterminate(R,S)
					end
			end;
		true ->
			eredis_client:stop(S#state.rcw),
			ok
	end.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

isprime(N) when N =< 1 -> false;
isprime(N) when N == 2 -> true;
isprime(N) when N == 3 -> true;
isprime(N) when N rem 2 == 0 -> false;
isprime(N) when N rem 3 == 0 -> false;
isprime(N) -> isprime(N,6,trunc(math:sqrt(N))+1).

isprime(_,I,I2) when I - 1 >= I2 -> true;
isprime(N,I, _) when N rem (I + 1) == 0 -> false;
isprime(N,I, _) when N rem (I - 1) == 0 -> false;
isprime(N,I,I2) -> isprime(N,I+6,I2).

-ifdef(TEST).

isprime_test_() -> [
	?_assert(isprime(0) =:= false),
	?_assert(isprime(7) =:= true),
	?_assert(isprime(2147483647) =:= true),
	?_assert(isprime(600) =:= false),
	?_assert(isprime(4) =:= false),
	?_assert(isprime(5) =:= true),
	?_assert(isprime(922483585259) =:= true),
	?_assert(isprime(828737779087) =:= true),
	?_assert(isprime(733880908597) =:= true),
	?_assert(isprime(733880908598) =:= false)

	].

-endif.

-module(filter_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([filter_test1/1, filter_test2/1, filter_test3/1]).

-define(RandLimit1, 100000000000).
-define(RandLimit2, 1000000000000).
-define(RandLimit3, 10000000000000).

-define(RandCount, 50000).

all() -> [filter_test1,filter_test2,filter_test3].

suite() -> 
	[{timetrap,{minutes,10}}].

init_per_suite(Config) ->
	{ok,App}   = tt_lib:get_application(),
	{ok,Rhost} = application:get_env(App,confRedisHost),
	{ok,Rport} = application:get_env(App,confRedisPort),
	{ok,Rdb}   = application:get_env(App,confRedisDB),
	{ok,Qk}    = application:get_env(App,confQueueKey),
	{ok,Rs}    = application:get_env(App,confResultSetKey),
	{ok,Rc}    = eredis:start_link(Rhost,Rport,Rdb),
	unlink(Rc),
	[{rc, Rc}, {rs, Rs}, {qk, Qk}| Config].


init_per_testcase(Case, Config) ->
	Rc = ?config(rc, Config),
	Qk = ?config(qk, Config),
	{ok,_} = eredis:q(Rc, ["DEL", Qk]),
	Rs = ?config(rs, Config),
	{ok,_} = eredis:q(Rc, ["DEL", Rs]),
	Lim = if 
		Case == filter_test1 -> ?RandLimit1;
		Case == filter_test2 -> ?RandLimit2;
		Case == filter_test3 -> ?RandLimit3;
		true                 -> 3
	end,
	Pr = fillrand(?RandCount,Rc,Qk,Lim),
	[{lim,Lim},{pr,Pr} | Config].

end_per_testcase(_, Config) ->
	proplists:delete(lim,proplists:delete(pr,Config)).



end_per_suite(Config) ->
	Rc = ?config(rc, Config),
	Rs = ?config(rs, Config),
	{ok,_} = eredis:q(Rc, ["DEL", Rs]),
	eredis_client:stop(Rc),
	ok.



fillrand(I,Rc,Qk,Lim) -> fillrand(I,Rc,Qk,Lim,[]).

fillrand(0,_,_,_,Pr) -> Pr;
fillrand(I,Rc,Qk,Lim,Pr) ->
	R = rand:uniform(Lim-1)+1,
	{ok,_} = eredis:q(Rc, ["LPUSH", Qk, R]),
	Ipr = isprime(R),
	if
		Ipr  -> fillrand(I-1,Rc,Qk,Lim,[R|Pr]);
		true -> fillrand(I-1,Rc,Qk,Lim,Pr)
	end.

isprime(N) when N =< 1 -> false;
isprime(N) when N == 2 -> true;
isprime(N)             -> isprime(N,2,trunc(math:sqrt(N))+1).

isprime(_,I,I2) when I >= I2      -> true;
isprime(N,I, _) when N rem I == 0 -> false;
isprime(N,I,I2)                   -> isprime(N,I+1,I2).


filter_test1(Config) ->
	test_speed(Config),
	test_result(Config).

filter_test2(Config) ->
	test_speed(Config),
	test_result(Config).

filter_test3(Config) ->
	test_speed(Config),
	test_result(Config).

test_speed(Config) ->
	{ok,Pid} = tt_filter_wrk:start_link(),
	Rc = ?config(rc, Config),
	Qk = ?config(qk, Config),
	{ok,Bval} = eredis:q(Rc, ["LLEN", Qk]),
	Val1 = list_to_integer(binary_to_list(Bval)),
	{Val2,T} = test_speed(Rc,Qk),
	Sp = (Val1 - Val2)/T,
	Lim = ?config(lim, Config),
	io:format("filter speed: ~p for Lim ~p ~n", [Sp,Lim]),
	process_flag(trap_exit, true),
	tt_filter_wrk:stop(Pid),
	receive {'EXIT',Pid,_} -> ok end,
	ok.

test_speed(Rc,Qk) -> test_speed(Rc,Qk,0,0).

test_speed(Rc,Qk,Pval,T) ->
	timer:sleep(1000),
	{ok,Bval} = eredis:q(Rc, ["LLEN", Qk]),
	Val = list_to_integer(binary_to_list(Bval)),
	if
		Val == 0 -> {Pval,T};
		true -> test_speed(Rc,Qk,Val,T+1)
	end.

test_result(Config) ->
	Rc = ?config(rc, Config),
	Rs = ?config(rs, Config),
	Pr = ?config(pr, Config),
	Pr2 = test_result(Rc,Rs,Pr),
%	io:format("test_result ~p ~n", [Pr2]),
	?assert(length(Pr2) == 0).

test_result(Rc,Rs,Pr) ->
	{ok,Bval} = eredis:q(Rc, ["SPOP", Rs]),
	if
		Bval /= undefined ->
			Val1 = list_to_integer(binary_to_list(Bval)),
			Pr2 = lists:delete(Val1,Pr),
			?assert(length(Pr) > length(Pr2)),
			test_result(Rc,Rs,Pr2);
		true -> Pr
	end.


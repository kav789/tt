-module(randgen_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([randgen_speed1/1, randgen_speed2/1]).

-define(TestPeriod1, 1000).
-define(TestCount1,  30).
-define(MaxDisp1,    10.0).

-define(TestPeriod2, 50).
-define(TestCount2,  200).
-define(MaxDisp2,    8.0).


all() -> [randgen_speed1, randgen_speed2].


suite() -> 
	[{timetrap,{minutes,10}}].

init_per_suite(Config) ->
	{ok,App} = tt_lib:get_application(),
	{ok,Rhost} = application:get_env(App,confRedisHost),
	{ok,Rport} = application:get_env(App,confRedisPort),
	{ok,Rdb}   = application:get_env(App,confRedisDB),
	{ok,Rps}   = application:get_env(App,confRandRPS),
	{ok,Qk}    = application:get_env(App,confQueueKey),
	{ok, Rc}   = eredis:start_link(Rhost,Rport,Rdb),
	unlink(Rc),
	{ok,_} = eredis:q(Rc, ["DEL", Qk]),
	{ok,Pid} = tt_randgen_sup:start_link(),
	unlink(Pid),
	[{pid, Pid }, {rc, Rc}, {rps, Rps}, {qk, Qk}| Config].

end_per_suite(Config) ->
	Rc = ?config(rc, Config),
	Qk = ?config(qk, Config),
	Pid = ?config(pid, Config),
	link(Pid),
	process_flag(trap_exit, true),
	tt_randgen_sup:stop(Pid),
	receive {'EXIT',Pid,_} -> ok end,
	{ok,_} = eredis:q(Rc, ["DEL", Qk]),
	eredis_client:stop(Rc),
	ok.



randgen_speed1(Config) ->
	R = test_speed(?TestPeriod1,?TestCount1,Config),
	?assert(R < ?MaxDisp1 ).

randgen_speed2(Config) ->
	R = test_speed(?TestPeriod2,?TestCount2,Config),
	?assert(R < ?MaxDisp2).


test_speed(To,I,Config) ->
	Rc  = ?config(rc, Config),
	Qk  = ?config(qk, Config),
	Rps = ?config(rps, Config),
	T0 = ( Rps*To ) div 1000,
	{ok,Bval} = eredis:q(Rc, ["LLEN", Qk]),
	Pval = list_to_integer(binary_to_list(Bval)),
	Tst = test_speed(To,Rc,Qk,Pval,I,[]),
	io:format("randgen_speed: expected ~p per ~p ms, result:  ~p ~n", [T0,To,Tst]),
	R = math:sqrt(lists:sum([(T-T0)*(T-T0) || T <- Tst])/length(Tst)),
	io:format("randgen_speed: per ~p ms: dipersion of values relativ to setting: ~p ~n", [To,R]),
	R.


test_speed(_,_,_,_,0,R) -> R;
test_speed(To,Rc,Qk,Pval,I,R) ->
	timer:sleep(To),
	{ok,Bval} = eredis:q(Rc, ["LLEN", Qk]),
	Val = list_to_integer(binary_to_list(Bval)),
	test_speed(To,Rc,Qk,Val,I-1,[(Val-Pval)|R]).


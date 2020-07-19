-module(tt_lib).
-export([get_application/0]).

get_application() ->
	App = application:get_application(),
	
	if
		App == undefined -> {ok,test};
		true -> App
	end.

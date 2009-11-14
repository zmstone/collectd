-module(collectd).
-export([dispatch_values/1, register_read/1]).

-include("collectd.hrl").

dispatch_values (#value_list{} = VL) ->
	call_cnode (dispatch_values, VL).

register_read (Callback) ->
	call_cnode (register_read, Callback).

call_cnode(Func, Args) ->
	{any, 'collectd@leeloo.lan.home.verplant.org'} ! {Func, Args},
	receive
		{ error, Message } ->
			io:format ("Function ~w failed: ~s~n", [Func, Message]),
			error;
		success ->
			success
	end.

-module(cexample).
-export([my_init/0]).

-include("collectd.hrl").

dispatch_reductions ({Total, _}) ->
	collectd:dispatch_values (#value_list{host="localhost", plugin="erlang",
			type="counter", type_instance="reductions",
			values=[Total]}).

dispatch_memory ({Type, Size}) ->
	collectd:dispatch_values (#value_list{host="localhost", plugin="erlang",
			type="memory", type_instance=atom_to_list (Type),
			values=[Size]}).

my_read() ->
	dispatch_reductions (statistics (reductions)),
	lists:foreach (fun dispatch_memory/1, erlang:memory()),
	success.

my_init() ->
	collectd:register_read (fun my_read/0).

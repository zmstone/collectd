% collectd - bindings/erlang/cexample.erl
% Copyright (C) 2009  Florian octo Forster
% 
% This program is free software; you can redistribute it and/or modify it
% under the terms of the GNU General Public License as published by the
% Free Software Foundation; only version 2 of the License is applicable.
% 
% This program is distributed in the hope that it will be useful, but
% WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% General Public License for more details.
% 
% You should have received a copy of the GNU General Public License along
% with this program; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
% 
% Authors:
%   Florian octo Forster <octo at verplant.org>

-module(cexample).
-export([my_init/0]).

-include("collectd.hrl").

dispatch_reductions ({Total, _}) ->
	collectd:dispatch_values (#value_list{host="zmdt", plugin="erlang",
			plugin_instance="example",type="gauge", type_instance="reductions",
			values=[Total]}).

dispatch_memory ({Type, Size}) ->
  collectd:dispatch_values (#value_list{host="zmdt", plugin="erlang",
      type="memory", type_instance=atom_to_list (Type),
			values=[Size]}).

my_read() ->
	dispatch_reductions (statistics (reductions)),
	lists:foreach (fun dispatch_memory/1, erlang:memory()),
	success.

my_init() ->
	collectd:register_read (fun my_read/0).

% collectd - bindings/erlang/collectd.hrl
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

-record(value_list, {host="localhost", plugin, plugin_instance, type, type_instance, interval=10, values}).
-record(data_source, {ds_name, type, min, max}).
-record(data_set, {type, sources}).

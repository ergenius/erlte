%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Record holding compiled templated
-record(erlte_compiled, {
	format = undefined :: undefined | atom(),
	fragments = [] :: list()
}).

% @doc Record used to keep trimming options
-record(erlte_trim, {
	dir = both :: string:direction(),
	characters = [] :: [char()]
}).

% @doc Record used to keep start and end tags
-record(erlte_tags, {
	start_tag = undefined :: undefined | list(),
	end_tag = undefined :: undefined | list()
}).

% @doc Record containing node specs
% You can use node specs to easily match all elements recognized by the erlte parser
% including single line comments, multiple lines comments, variables or import directives
-record(erlte_node_specs, {
	type = undefined :: undefined | variable | import | comment,
	tags = undefined :: undefined | erlte:tags_specs(),
	keep_tags = false :: true | false,
	trim = undefined :: undefined | erlte:trim_specs(),
	quotes = undefined :: undefined | [char()]
}).

% @doc Record containing compiler specifications for a specific file format
-record(erlte_format_specs, {
	name = undefined :: atom(),
	file_extension = undefined :: undefined | unicode:chardata(),
	nodes = undefined :: undefined | [erlte:node_specs()]
}).


%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com> <www.ergenius.com>
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
-module(erlte_utils).
-author("Madalin Grigore-Enescu").

-include("../include/erlte.hrl").

-export([variable_name_to_binary/1]).
-export([html_entities_encode/1]).
-export([list_remove_start_list/2]).

% @doc Convert variable name to binary
variable_name_to_binary(Name) when erlang:is_binary(Name) -> Name;
variable_name_to_binary(Name) when erlang:is_atom(Name) -> erlang:atom_to_binary(Name, utf8);
variable_name_to_binary(Name) when erlang:is_list(Name) -> unicode:characters_to_binary(Name, utf8, utf8).

% @doc Simple version of html entities encode
% TODO: replace with erhtmle
html_entities_encode(Value) -> html_entities_encode(Value, []).
html_entities_encode([H|T], Acum) -> html_entities_encode(T, [html_entity_encode(H) | Acum]);
html_entities_encode([], Acum) -> lists:flatten(lists:reverse(Acum)).

html_entity_encode($<) -> "&lt;";
html_entity_encode($>) -> "&gt;";
html_entity_encode($&) -> "&amp;";
html_entity_encode($") -> "&quot;";
html_entity_encode(C) -> C.

% @doc Remove a list from the begining of another list.
% If the list is not found false is returned.
% If the is found the remaining of the second list is returned.
list_remove_start_list([T|TT], [T|IT]) -> list_remove_start_list(TT, IT);
list_remove_start_list([], Input) -> {true, Input};
list_remove_start_list(_Tag, _Input) -> false.    
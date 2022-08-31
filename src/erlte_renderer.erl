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
-module(erlte_renderer).
-author("Madalin Grigore-Enescu").

-include("../include/erlte.hrl").

-export([render/2]).
-export([render_to_compiled/2]).
-export([render_to_file/2, render_to_file/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

-spec render(Compiled, Variables) -> {ok, Result} | {error, Error} when
    Compiled :: erlte:compiled(),
    Variables :: list(),
    Result :: binary(),
    Error :: term().
%% @doc Render the compiled template replacing the specified variables
%% There is no error return case now but some maybe added in the future so for compatibility
%% reasons you should handle a generic error case too.
render(#erlte_compiled{fragments = Fragments}, Variables) when Variables =:= undefined; Variables =:= [] -> 
    NewFragments = render_missing(Fragments),
    {ok, erlang:iolist_to_binary(lists:reverse(NewFragments))};
render(Compiled, Variables) -> render(Compiled, lists:reverse(Variables), 0).

render(Compiled = #erlte_compiled{
    format = Format, 
    fragments = Fragments}, [H|T], Iterations) ->
    NewFragments = variable_render(Format, H, Fragments),
    render(Compiled#erlte_compiled{fragments = NewFragments}, T, Iterations+1);

render(#erlte_compiled{fragments = Fragments}, _, Iterations) -> 
    NewFragments = render_missing(Fragments),
    % Save another one lists:reverse
    Remainder = (Iterations+1) rem 2,
    case Remainder of
        1 -> {ok, erlang:iolist_to_binary(lists:reverse(NewFragments))};
        _ -> {ok, erlang:iolist_to_binary(NewFragments)}
    end.

render_missing(Fragments) -> render_missing(Fragments, []).
render_missing([{v, Name}|T], Acum) -> render_missing(T, [Name|Acum]);
render_missing([H|T], Acum) -> render_missing(T, [H|Acum]);
render_missing([], Acum) -> Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render_to_compiled
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

-spec render_to_compiled(Compiled, Variables) -> {ok, NewCompiled} | {error, Error} when
    Compiled :: erlte:compiled(),
    Variables :: list(),
    NewCompiled :: erlte:compiled(),
    Error :: term().
%% @doc Render the compiled template replacing the specified variables and return a new compiled template
%% There is no error return case now but some maybe added in the future so for compatibility
%% reasons you should handle a generic error case too.
render_to_compiled(Compiled, Variables) when Variables =:= undefined; Variables =:= [] ->  {ok, Compiled};
render_to_compiled(Compiled, Variables) -> render_to_compiled(Compiled, lists:reverse(Variables), 0).

render_to_compiled(Compiled = #erlte_compiled{
    format = Format,
    fragments = Fragments}, [H|T], Iterations) ->
    NewFragments = variable_render(Format, H, Fragments),
    render_to_compiled(Compiled#erlte_compiled{fragments = NewFragments}, T, Iterations+1);

render_to_compiled(Compiled = #erlte_compiled{fragments = Fragments}, _, Iterations) -> 
    % Save another one lists:reverse
    Remainder = Iterations rem 2,
    case Remainder of
        1 -> {ok, render_to_compiled_collapse(Compiled#erlte_compiled{fragments = lists:reverse(Fragments)})};
        _ -> {ok, render_to_compiled_collapse(Compiled)}
    end.

render_to_compiled_collapse(Compiled) -> render_to_compiled_collapse(Compiled, [], []).
render_to_compiled_collapse(Compiled = #erlte_compiled{
    fragments = [H|T]}, TempAcum, FragmentsAcum) when erlang:is_binary(H) ->
    render_to_compiled_collapse(Compiled#erlte_compiled{
        fragments = T}, [TempAcum, H], FragmentsAcum);

render_to_compiled_collapse(Compiled = #erlte_compiled{
        fragments = [H|T]}, TempAcum, FragmentsAcum) ->
    NewTempAcum = erlang:iolist_to_binary(TempAcum),
    NewFragmentsAcum = [FragmentsAcum, NewTempAcum, H],
    render_to_compiled_collapse(Compiled#erlte_compiled{
            fragments = T}, [], NewFragmentsAcum);
render_to_compiled_collapse(Compiled = #erlte_compiled{
        fragments = []}, TempAcum, FragmentsAcum) ->
    case TempAcum of
        [] -> Compiled#erlte_compiled{fragments = lists:flatten(FragmentsAcum)};
        _ -> 
            NewTempAcum = erlang:iolist_to_binary(TempAcum),
            Compiled#erlte_compiled{fragments = lists:flatten([FragmentsAcum, NewTempAcum])}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render_to_file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      

-spec render_to_file(Filename, Compiled) -> {ok, Result} | {error, Reason} when
    Filename :: file:name_all(),
    Compiled :: erlte:compiled(),
    Result :: binary(),    
    Reason :: file:posix() | badarg | terminated | system_limit.
%% @doc Render compiled template to the specified file
render_to_file(File, Compiled) -> render_to_file(File, Compiled, undefined).

-spec render_to_file(Filename, Compiled, Variables) -> {ok, Result} | {error, Reason} when
    Filename :: file:name_all(),
    Compiled :: erlte:compiled(),
    Variables :: undefined | list(),
    Result :: binary(),
    Reason :: file:posix() | badarg | terminated | system_limit.
%% @doc Render compiled template replacing the specified variables and save the result into the specified file
render_to_file(Filename, Compiled, Variables) ->
    case render(Compiled, Variables) of
        {ok, Result} -> 
            BOM = unicode:encoding_to_bom(utf8),
            case file:write_file(Filename, <<BOM/binary, Result/binary>>) of 
                ok -> {ok, Result};
                WriteFileError -> WriteFileError
            end;
        Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variable_render
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% @doc Render variable
variable_render(Format, {Name, Value}, Fragments) -> 
    BinaryName = variable_name_to_binary(Name),
    BinaryValue = variable_value_to_binary(Format, BinaryName, Value),
    variable_render_all(BinaryName, BinaryValue, Fragments, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variable_render_all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         

variable_render_all(Name, BinaryValue, [{v,Name}|T], Acum) ->
    NewAcum = [BinaryValue|Acum],
    variable_render_all(Name, BinaryValue, T, NewAcum);
variable_render_all(Name, BinaryValue, [H|T], Acum) ->
    NewAcum = [H|Acum],
    variable_render_all(Name, BinaryValue, T, NewAcum);
variable_render_all(_Name, _BinaryValue, [], Acum) -> 
    % We don't reverse acumulator here, 
    % we do this later when all variables where iterated.
    % This way we save a reverse on each variable iteration.
    Acum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variable_name_to_binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% @doc Convert variable name to binary
variable_name_to_binary(Name) when erlang:is_binary(Name) -> Name;
variable_name_to_binary(Name) when erlang:is_list(Name) -> unicode:characters_to_binary(Name, utf8, utf8);
variable_name_to_binary(Name) when erlang:is_atom(Name) -> erlang:atom_to_binary(Name, utf8);
variable_name_to_binary(Name) when erlang:is_integer(Name) -> erlang:integer_to_binary(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variable_value_to_binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% @doc Sanitize the variable value according to the specified template file format
variable_value_to_binary(Format, Name, {Value, Options}) -> variable_value_to_binary_options(Format, Name, Value, Options);
variable_value_to_binary(_Format, Name, Value) when erlang:is_integer(Value) -> 
    variable_value_to_binary(_Format, Name, erlang:integer_to_list(Value));
variable_value_to_binary(_Format, Name, Value) when erlang:is_float(Value) -> 
    variable_value_to_binary(_Format, Name, erlang:float_to_list(Value));
variable_value_to_binary(_Format, Name, Value) when erlang:is_atom(Value) -> 
    variable_value_to_binary(_Format, Name, erlang:atom_to_list(Value));
variable_value_to_binary(_Format, Name, Value) when erlang:is_binary(Value) -> 
    variable_value_to_binary(_Format, Name, unicode:characters_to_list(Value, utf8));
variable_value_to_binary(Format, _Name, Value) when erlang:is_list(Value)-> 
    variable_sanitize(Format, Value).

variable_value_to_binary_options(Format, Name, Value, Options) when 
    erlang:is_float(Value); erlang:is_list(Options) ->
    variable_value_to_binary(Format, Name, erlang:float_to_list(Value, Options));
variable_value_to_binary_options(Format, Name, Value, {f, Module, Function, Arg}) when 
    erlang:is_atom(Module); erlang:is_atom(Function) ->
    Result = erlang:apply(Module, Function, [Format, Name, Value, Arg]),
    true = erlang:is_binary(Result),
    Result.

variable_sanitize(html, Value) -> unicode:characters_to_binary(erlte_utils:html_entities_encode(Value));
variable_sanitize(_, Value) -> unicode:characters_to_binary(Value, utf8, utf8).

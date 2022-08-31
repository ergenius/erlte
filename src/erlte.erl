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
-module(erlte).
-author("Madalin Grigore-Enescu").

-include("../include/erlte.hrl").

-export([compile/1, compile/2, compile/3]).
-export([compiled_write_file/2, compiled_read_file/1]).
-export([render/1, render/2]).
-export([render_to_compiled/2]).
-export([render_to_file/2, render_to_file/3]).

-export_type([compiled/0, trim_specs/0, tags_specs/0, node_specs/0, format_specs/0]).

-type compiled() :: #erlte_compiled{}.
-type trim_specs() :: #erlte_trim{}.
-type tags_specs() :: #erlte_tags{}.
-type node_specs() :: #erlte_node_specs{}.
-type format_specs() :: #erlte_format_specs{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   

-spec compile(Input) -> {ok, Compiled} | {error, Error} when
    Input :: binary() | list() | {file, file:name_all()},
    Compiled :: erlte:compiled(),
    Error :: term().
% @doc Compile the specified input with no variables and default format specifications
compile(Input) -> compile(Input, undefined, undefined).

-spec compile(Input, Variables) -> {ok, Compiled} | {error, Error} when
    Input :: binary() | list() | {file, file:name_all()},
    Variables :: list(),    
    Compiled :: erlte:compiled(),
    Error :: term().
% @doc Compile the specified input with variables and default format specifications
compile(Input, Variables) -> compile(Input, Variables, undefined).

-spec compile(Input, Variables, FormatSpecs) -> {ok, Compiled} | {error, Error} when
    Input :: binary() | list() | {file, file:name_all()},
    Variables :: list(),
    FormatSpecs :: undefined | erlte:format_specs(),
    Compiled :: erlte:compiled(),
    Error :: term().
% @doc Compile the specified input according to the format specification
compile(Input, Variables, Options) -> erlte_compiler:compile(Input, Variables, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compiled
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   

-spec compiled_write_file(Filename, Compiled) -> ok | {error, Reason} when
    Filename :: file:name_all(),
    Compiled :: erlte:compiled(),
    Reason :: file:posix() | badarg | terminated | system_limit.
% @doc Write the compiled template to the specified file
compiled_write_file(Filename, Compiled = #erlte_compiled{}) -> 
    CompiledBinary = erlang:term_to_binary(Compiled, [{compressed, 9}, {minor_version, 1}]),
    file:write_file(Filename, CompiledBinary).

-spec compiled_read_file(Filename) -> {ok, Compiled} | {error, Reason} when
    Filename :: file:name_all(),
    Compiled :: erlte:compiled(),
    Reason :: file:posix() | badarg | terminated | system_limit.
% @doc Read the compiled template from the specified file
compiled_read_file(Filename) -> 
    case file:read_file(Filename) of
        {ok, Binary} -> 
            try 
                Compiled = erlang:binary_to_term(Binary),
                true = erlang:is_record(Compiled, erlte_compiled),
                {ok, Compiled}
            catch _ -> {error, invalid_file_format}
            end;
        Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-spec render(Compiled) -> {ok, Result} | {error, Error} when
    Compiled :: erlte:compiled(),
    Result :: binary(),
    Error :: term().
%% @doc Render the compiled template without replacing any variables
%% There is no error return case now but some maybe added in the future so for compatibility
%% you should handle a generic error case too.
render(Compiled) -> render(Compiled, undefined).

-spec render(Compiled, Variables) -> {ok, Result} | {error, Error} when
    Compiled :: erlte:compiled(),
    Variables :: list(),
    Result :: binary(),
    Error :: term().
% @doc Render compiled template replacing the specified variables
%% There is no error return case now but some maybe added in the future so for compatibility
%% you should handle a generic error case too.
render(Compiled, Variables) -> erlte_renderer:render(Compiled, Variables).

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
render_to_compiled(Compiled, Variables) -> erlte_renderer:render_to_compiled(Compiled, Variables).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render_to_file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-spec render_to_file(Filename, Compiled) -> {ok, Result} | {error, Reason} when
    Filename :: file:name_all(),
    Compiled :: erlte:compiled(),
    Result :: binary(),
    Reason :: file:posix() | badarg | terminated | system_limit.
% @doc Render compiled template and save the result into the specified file
render_to_file(Filename, Compiled) -> render_to_file(Filename, Compiled, undefined).

-spec render_to_file(Filename, Compiled, Variables) -> {ok, Result} | {error, Reason} when
    Filename :: file:name_all(),
    Compiled :: erlte:compiled(),
    Variables :: undefined | list(),
    Result :: binary(),
    Reason :: file:posix() | badarg | terminated | system_limit.
% @doc Render compiled template replacing the specified variables and save the result into the specified file
render_to_file(Filename, Compiled, Variables) -> erlte_renderer:render_to_file(Filename, Compiled, Variables).

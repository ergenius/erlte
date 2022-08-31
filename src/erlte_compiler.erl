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
-module(erlte_compiler).
-author("Madalin Grigore-Enescu").

-include("../include/erlte.hrl").

-export([compile/3]).

%% Array containing known file formats specifications for the erlte compiler
-define(ERLTE_FORMAT_SPECS, [
    #erlte_format_specs{
        name = html,
        file_extension = ".html",
        nodes = [
            #erlte_node_specs{
                type = comment,
                tags = #erlte_tags{start_tag = "<!--", end_tag = "-->"},
                keep_tags = true
            },
            #erlte_node_specs{
                type = variable,
                tags = #erlte_tags{start_tag = "{{", end_tag = "}}"},
                keep_tags = false
            },
            #erlte_node_specs{
                type = import,
                tags = #erlte_tags{start_tag = "<erlte-import>", end_tag = "</erlte-import>"},
                keep_tags = false
            }
        ]
    },
    #erlte_format_specs{
        name = js,
        file_extension = ".js",
        nodes = [
            #erlte_node_specs{
                type = comment,
                tags = #erlte_tags{start_tag = "//", end_tag = "\n"},
                keep_tags = true
            },
            #erlte_node_specs{
                type = comment,
                tags = #erlte_tags{start_tag = "/*", end_tag = "*/"},
                keep_tags = true
            },
            #erlte_node_specs{
                type = variable,
                tags = #erlte_tags{start_tag = "{{", end_tag = "}}"},
                keep_tags = false
            },
            #erlte_node_specs{
                type = import,
                tags = #erlte_tags{start_tag = "import", end_tag = ";"},
                keep_tags = false,
                trim = #erlte_trim{
                    dir = both,
                    characters = [16#0020, 16#0009]
                },
                quotes = [16#0022, 16#0027]
            }
        ]
    },
    #erlte_format_specs{
        name = css,
        file_extension = ".css",
        nodes = [
            #erlte_node_specs{
                type = comment,
                tags = #erlte_tags{start_tag = "/*", end_tag = "*/"},
                keep_tags = true
            },
            #erlte_node_specs{
                type = variable,
                tags = #erlte_tags{start_tag = "{{", end_tag = "}}"},
                keep_tags = false
            },
            #erlte_node_specs{
                type = import,
                tags = #erlte_tags{start_tag = "@import", end_tag = ";"},
                keep_tags = false,
                trim = #erlte_trim{
                    dir = both,
                    characters = [16#0020, 16#0009]
                },
                quotes = [16#0022]
            }
        ]
    }
]).

%% Default format specifications (for unknown files)
-define(ERLTE_UNKNOWN_FORMAT_SPECS, #erlte_format_specs{
    name = unknown,
    file_extension = "",
    nodes = [
        #erlte_node_specs{
            type = variable,
            tags = #erlte_tags{start_tag = "{{", end_tag = "}}"},
            keep_tags = false
        }
    ]}).

%% Compiler accumulator
-record(erlte_compiler_acum, {
    fragments = [] :: list(),
    data = [] :: list()
}).

-type erlte_compiler_acum() :: #erlte_compiler_acum{}.

%% Compiler state
-record(erlte_compiler_state, {
    input = undefined :: undefined | list(),
    directory = undefined :: undefined | list(),
    specs = undefined :: undefined | erlte:format_specs(),
    acum = undefined :: undefined | erlte_compiler_acum()
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compile(Input, Variables, FormatSpecs) -> {ok, Compiled} | {error, Error} when
    Input :: binary() | list() | {file, file:name_all()},
    Variables :: list(),
    FormatSpecs :: undefined | erlte:format_specs(),
    Compiled :: erlte:compiled(),
    Error :: term().
%% @doc Compile the specified input according to the format specification
compile(Input, Variables, Options) -> 
    case compile_private(Input, Options) of 
        {ok, Compiled} -> erlte_renderer:render_to_compiled(Compiled, Variables);
        Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile_private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Compile the specified input according to the format specification
compile_private(Input, FormatSpecs = #erlte_format_specs{}) when erlang:is_binary(Input) -> 
    ListInput = unicode:characters_to_list(Input, utf8),
    case erlang:is_list(ListInput) of 
        true -> compile_private_state(#erlte_compiler_state{input = ListInput, specs = FormatSpecs});
        Error -> {error, Error}
    end;
compile_private(Input, FormatSpecs = #erlte_format_specs{}) when erlang:is_list(Input) -> 
    compile_private_state(#erlte_compiler_state{
        input = Input,
        directory = undefined,
        specs = FormatSpecs,
        acum = #erlte_compiler_acum{}});

compile_private({file, Filename}, FormatSpecs) -> 
    FileExt = filename:extension(Filename),
    FileFormatSpecs = case FormatSpecs of
        #erlte_format_specs{} -> FormatSpecs;
        _ -> get_default_format_specs(FileExt)
    end,
    case file:read_file(Filename) of
        {ok, BinaryInput} ->
            case FileFormatSpecs#erlte_format_specs.nodes of
                    [] -> 
                        {ok, #erlte_compiled{
                        format = FileFormatSpecs#erlte_format_specs.name,
                        fragments = [BinaryInput]}};
                    _ -> 
                        Input = unicode:characters_to_list(BinaryInput, utf8),                        
                        compile_private_state(#erlte_compiler_state{
                            input = Input,
                            directory = filename:dirname(Filename),
                            specs = FileFormatSpecs,
                            acum = #erlte_compiler_acum{}
                        })
                end;            
        Error -> Error
    end;

compile_private(Input, undefined) -> compile_private(Input, ?ERLTE_UNKNOWN_FORMAT_SPECS).

%% @doc Call parse and compile the result into a compiled template on success
compile_private_state(State) ->
    case parse(State) of 
        {ok, #erlte_compiler_state{
            specs = #erlte_format_specs{
                name = FormatName
            },
            acum = #erlte_compiler_acum{
                fragments = Fragments
            }
        }} -> {ok, #erlte_compiled{
                format = FormatName,
                fragments = lists:flatten(Fragments)}};
        Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

parse(State = #erlte_compiler_state{
    input = [], 
    acum = Acum = #erlte_compiler_acum{fragments = Fragments, data = Data}}) -> 
        FinalFragments = fragments_add_element(Data, Fragments),
        {ok, State#erlte_compiler_state{
            acum = Acum#erlte_compiler_acum{
                fragments = FinalFragments,
                data = []}}};

parse(State = #erlte_compiler_state{input = Input, specs = #erlte_format_specs{nodes = NodesSpecs}}) ->
    ScanResult = scan_nodes(NodesSpecs, Input),
    parse_scan_result(ScanResult, State).

% No match - advance next
parse_scan_result(no_match, State = #erlte_compiler_state{input = [H|T]}) -> 
    parse(acum_add_data(H, T, State));

% Variable node
parse_scan_result({variable, NodeData, RemainingInput}, State) -> 
    parse(acum_add_variable(NodeData, RemainingInput, State));

% Import
parse_scan_result({import, NodeData, NewInput}, State = #erlte_compiler_state{
    specs = FormatSpecs
}) -> 
    case import_locate_file(NodeData, State) of 
        {true, Filename} -> 
            case compile_private({file, Filename}, FormatSpecs) of
                {ok, Compiled} ->
                    parse(acum_add_compiled(Compiled, NewInput, State));
                Error -> Error
            end;
        {false, NotFoundFilename} -> {error, {import_locate_file, NotFoundFilename}}
    end;

% Other types of nodes (comments or custom nodes)
parse_scan_result({_, NodeData, NewInput}, State) -> 
    parse(acum_add_data(NodeData, NewInput, State)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% import_locate_file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

% Locate file
import_locate_file(NodeData, #erlte_compiler_state{
    directory = Directory,
    specs = #erlte_format_specs{
        file_extension = FileExtension
    }
}) ->
    case filename:extension(NodeData) of 
        FileExtension -> import_locate_file_directory(NodeData, Directory);
        _ -> import_locate_file_directory(NodeData++FileExtension, Directory)
    end.

import_locate_file_directory(NodeData = [$/|_], _Directory) -> 
    {filelib:is_file(NodeData), NodeData};
import_locate_file_directory(NodeData, Directory) -> 
    Filename = filename:join([Directory, NodeData]),
    {filelib:is_file(Filename), Filename}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% scan_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

%% @doc Iterate all known nodes types and scan for them next
scan_nodes([], _Input) -> no_match;
scan_nodes(undefined, _Input) -> no_match;
scan_nodes([NodeSpecs=#erlte_node_specs{}|T], Input) ->
    case scan_node(NodeSpecs, Input) of 
        no_match -> scan_nodes(T, Input);
        Found -> Found
    end.
        
% @doc Scan for a specific node type
scan_node(NodeSpecs = #erlte_node_specs{
    tags = #erlte_tags{start_tag = StartTag},
    keep_tags = false
}, Input) ->
    case scan_node_tag(StartTag, Input) of 
        {true, RemainingInput} -> scan_node_data(RemainingInput, NodeSpecs);
        _ -> no_match
    end;
scan_node(NodeSpecs = #erlte_node_specs{
    tags = #erlte_tags{start_tag = StartTag, end_tag = EndTag}
}, Input) ->
    case scan_node_tag(StartTag, Input) of 
        {true, RemainingInput} -> 
            case scan_node_data(RemainingInput, NodeSpecs) of
                {Type, Node, NewInput} -> {Type, lists:append([StartTag, Node, EndTag]), NewInput};
                _ -> no_match
            end;
        _ -> no_match
    end.

%% @doc Scan for node tag (start or end or others tags)
scan_node_tag([T|TT], [T|IT]) -> scan_node_tag(TT, IT);
scan_node_tag([], Input) -> {true, Input};
scan_node_tag(_Tag, _Input) -> false.

%% @doc Possible node detected, scan for node data (and end tag)
scan_node_data(Input, NodeSpecs) -> scan_node_data(Input, NodeSpecs, []).
scan_node_data([], _NodeSpecs, _Acum) -> no_match;
scan_node_data(Input = [H|T], NodeSpecs = #erlte_node_specs{
    type = Type,
    tags = #erlte_tags{end_tag = EndTag},
    trim = Trim,
    quotes = DataQuotes
}, Acum) ->
    case scan_node_tag(EndTag, Input) of 
        {true, NewInput} -> 
            ReversedAcum = lists:reverse(Acum),
            TrimedAcum = data_trim(Trim, ReversedAcum),
            UnquotedAcum = data_remove_quotes(DataQuotes, TrimedAcum),
            {Type, UnquotedAcum, NewInput};
        _ -> scan_node_data(T, NodeSpecs, [H|Acum])
    end.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% acum_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add raw data to accumulator
acum_add_data(Value, NewInput, State=#erlte_compiler_state{
    acum = Parsed = #erlte_compiler_acum{
        data = Data
    }
}) -> 
    State#erlte_compiler_state{
        input = NewInput,
        acum = Parsed#erlte_compiler_acum{
            data = [Data, Value]
        }
    }.

%% @doc Add variable to accumulator
acum_add_variable(Value, NewInput, State=#erlte_compiler_state{
    acum = #erlte_compiler_acum{
        fragments = Fragments,
        data = Data
}}) -> 
    Fragments1 = fragments_add_element(Data, Fragments),
    NewFragments = fragments_add_element({v, Value}, Fragments1),
    State#erlte_compiler_state{
        input = NewInput,
        acum = #erlte_compiler_acum{
            fragments = NewFragments,
            data = []
        }
    }.

%% @doc Add another compiled template accumulator
acum_add_compiled(#erlte_compiled{
        fragments = CompiledFragments
    }, NewInput, 
    State = #erlte_compiler_state{
        acum = #erlte_compiler_acum{
            fragments = StateFragments,
            data = []
        }
    }) -> 
    NewFragments = [StateFragments, CompiledFragments],
    State#erlte_compiler_state{
        input = NewInput,
        acum = #erlte_compiler_acum{
            fragments = NewFragments,
            data = []
        }
    };
acum_add_compiled(#erlte_compiled{
    fragments = CompiledFragments
}, NewInput, 
State = #erlte_compiler_state{
    acum = #erlte_compiler_acum{
        fragments = StateFragments,
        data = Data
    }
}) -> 
    StateFragments1 = fragments_add_element(Data, StateFragments),
    NewFragments = [StateFragments1, CompiledFragments],
    State#erlte_compiler_state{
        input = NewInput,
        acum = #erlte_compiler_acum{
            fragments = NewFragments,
            data = []
    }
}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Remove characters from the begining and/or end of data according to the parameters
data_trim(#erlte_trim{dir = TrimDir, characters = TrimCharacters}, Input) when erlang:is_list(TrimCharacters) -> 
    string:trim(Input, TrimDir, TrimCharacters);
data_trim(_, Input) -> Input.

%% @doc Remove quotes from data according to the parameters
data_remove_quotes(DataQuotes, Input) when erlang:is_list(DataQuotes) -> 
    string:trim(Input, both, DataQuotes);
data_remove_quotes(_DataQuotes, Input) -> Input.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fragments_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fragments_add_element({Type, Fragment}, Fragments) when 
    erlang:is_atom(Type); 
    erlang:is_list(Fragments) ->
    BinaryValue = unicode:characters_to_binary(Fragment, utf8, utf8),
    [Fragments, {Type, BinaryValue}];
fragments_add_element(Fragment, Fragments) when erlang:is_binary(Fragment); erlang:is_list(Fragments) ->
    Value = unicode:characters_to_binary(Fragment, utf8, utf8),
    [Fragments, Value].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_default_format_specs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns default format specifications for the specified file extension
get_default_format_specs(FileExtension) when erlang:is_list(FileExtension) -> get_default_format_specs(string:lowercase(FileExtension), ?ERLTE_FORMAT_SPECS).
get_default_format_specs(FileExtension, [H = #erlte_format_specs{file_extension = FileExtension}|_]) -> H;
get_default_format_specs(FileExtension, [_|T]) -> get_default_format_specs(FileExtension, T);
get_default_format_specs(_FileExtension, []) -> ?ERLTE_UNKNOWN_FORMAT_SPECS.
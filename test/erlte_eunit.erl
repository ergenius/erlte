-module(erlte_eunit).

-export([filename_input/1, filename_input/2]).
-export([filename_output/1, filename_output/2]).

-define(ERLTE_EUNIT_PATH, filename:join([filename:dirname(filename:dirname(code:which(?MODULE)))])).

% @doc Returns input template filename including full path
filename_input(RelativeFilename) -> 
    filename:join([?ERLTE_EUNIT_PATH, "examples/templates", RelativeFilename]).
filename_input(RelativeFilename, Extension) -> 
    filename_input(RelativeFilename++Extension).

% @doc Returns output template filename including full path
filename_output(RelativeFilename) -> 
    Path = filename:join([?ERLTE_EUNIT_PATH, "examples/output"]),
    Filename = filename:join([Path, RelativeFilename]),
    filelib:ensure_dir(Filename),
    Filename.
filename_output(RelativeFilename, Extension) -> 
    filename_output(RelativeFilename++Extension).

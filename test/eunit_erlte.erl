-module(eunit_erlte).

-export([custom_renderer/4]).

-include_lib("eunit/include/eunit.hrl").

-define(ERLTE_EUNIT_TEMPLATES_FILE_LIST, [
    {"html/index", ".html"},
    {"js/index", ".js"},
    {"css/index", ".css"},
    {"txt/index", ".txt"},
    {"txt/index", ".unknown"}
]).

-define(ERLTE_EUNIT_TEST_VARIABLES, [
    {atom, '[atom]'},
    {"list", "[list]"}, 
    {<<"binary">>, <<"[binary]">>},
    {"integer", 1},
    {"float", {2.34670, [{decimals, 1}, compact]}},
    {"deeplist", ["[", $d, "ee", "p", ["li", ["s", ["t", "]"]]]]},
    {"duplicate", "[ERROR]"},
    {"duplicate", "[duplicate]"},
    {"repeated", "[repeated]"},
    {"html_entities", "[<>\"&]"},
    {"missing_from_template", "[?]"},
    {"utf8test", "Kết nối với máy chủ"}
]).

custom_renderer(unknown, <<"customrenderer">>, undefined, ok) -> <<"OK">>.

compile_test_iterate([]) -> ok;
compile_test_iterate([{HN,HE}|T]) ->

    {ok, Compiled} = erlte:compile({file, erlte_eunit:filename_input(HN,HE)}),
    {ok, CompiledWithVariables} = erlte:compile({file, erlte_eunit:filename_input(HN,HE)}, ?ERLTE_EUNIT_TEST_VARIABLES),
    CompiledFilename = erlte_eunit:filename_output(HN, HE++".erlte"),
    ok = erlte:compiled_write_file(CompiledFilename, Compiled),
    {ok, Compiled} = erlte:compiled_read_file(CompiledFilename),
    {ok, _} = erlte:render_to_file(erlte_eunit:filename_output(HN, HE), Compiled),
    {ok, _} = erlte:render_to_file(erlte_eunit:filename_output(HN, "-compiled-var"++HE), CompiledWithVariables),
    {ok, _} = erlte:render_to_file(erlte_eunit:filename_output(HN, "-var"++HE), Compiled, ?ERLTE_EUNIT_TEST_VARIABLES),
    {ok, _} = erlte:render_to_file(erlte_eunit:filename_output(HN, "-var-empty"++HE), Compiled, []),
    {ok, _} = erlte:render_to_file(erlte_eunit:filename_output(HN, "-var-undefined"++HE), Compiled, undefined),
    compile_test_iterate(T).

compile_test_() ->

    {ok, C1} = erlte:compile("{{project}} was designed by {{author}}"),
    V1 = [{project, "erlte"}, {author, "Madalin"}],
    {ok, R1} = erlte:render(C1, V1),

    {ok, C2} = erlte:compile("{{molecule}} {{server is down}} {{1}}"),
    V2 = [{molecule, "atoms are OK"}, {<<"server is down">>, "binaries are OK"}, {1, "integers are OK"}],
    {ok, R2} = erlte:render(C2, V2),

    {ok, C3} = erlte:compile("Custom renderer function is {{customrenderer}}"),
    V3 = [{customrenderer, {undefined, {f, ?MODULE, custom_renderer, ok}}}],
    {ok, R3} = erlte:render(C3, V3),

    compile_test_iterate(?ERLTE_EUNIT_TEMPLATES_FILE_LIST),

    [
        ?_assert(R1 =:= <<"erlte was designed by Madalin">>),
        ?_assert(R2 =:= <<"atoms are OK binaries are OK integers are OK">>),
        ?_assert(R3 =:= <<"Custom renderer function is OK">>)
    ].

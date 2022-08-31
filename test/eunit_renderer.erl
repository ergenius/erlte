-module(eunit_renderer).

-include_lib("eunit/include/eunit.hrl").
-include("../include/erlte.hrl").

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
    {"missing_from_template", "[?]"}
]).

-define(ERLTE_EUNIT_TEMPLATE_FRAGMENTS_1, [
    <<"BEGIN">>,          
    <<"+">>,             
    {v, <<"atom">>},
    <<"+">>, 
    {v, <<"list">>},
    <<"+">>,  
    {v, <<"binary">>},
    <<"+">>,  
    {v, <<"integer">>},
    <<"+">>,  
    {v, <<"float">>},
    <<"+">>,  
    <<"">>,
    <<"+">>,            
    {v, <<"deeplist">>}, 
    <<"+">>,
    {v, <<"duplicate">>},
    <<"+">>, 
    {v, <<"repeated">>},             
    <<"+">>,
    {v, <<"repeated">>},
    <<"+">>,            
    {v, <<"repeated">>},                        
    <<"+">>, 
    {v, <<"html_entities">>},                        
    <<"+">>, 
    {v, <<"missing1">>},
    <<"+">>, 
    {v, <<"missing2">>},
    <<"+">>, 
    {v, <<"missing3">>},
    <<"+">>,                            
    <<"END">>
]).

-define(ERLTE_EUNIT_TEMPLATE_1, #erlte_compiled{
    format = txt,
    fragments = ?ERLTE_EUNIT_TEMPLATE_FRAGMENTS_1
}).

-define(ERLTE_EUNIT_TEMPLATE_2, #erlte_compiled{
    format = html,
    fragments = ?ERLTE_EUNIT_TEMPLATE_FRAGMENTS_1
}).

render_test_() ->

    {ok, R1} = erlte_renderer:render(?ERLTE_EUNIT_TEMPLATE_1, ?ERLTE_EUNIT_TEST_VARIABLES),
    {ok, R2} = erlte_renderer:render(?ERLTE_EUNIT_TEMPLATE_2, ?ERLTE_EUNIT_TEST_VARIABLES),
    {ok, Compiled = #erlte_compiled{fragments = Fragments}} = erlte_renderer:render_to_compiled(?ERLTE_EUNIT_TEMPLATE_2, ?ERLTE_EUNIT_TEST_VARIABLES),
    {ok, R2} = erlte_renderer:render(Compiled, ?ERLTE_EUNIT_TEST_VARIABLES),

    [
        ?_assert(R1 =:= <<"BEGIN+[atom]+[list]+[binary]+1+2.3++[deeplist]+[duplicate]+[repeated]+[repeated]+[repeated]+[<>\"&]+missing1+missing2+missing3+END">>),
        ?_assert(R2 =:= <<"BEGIN+[atom]+[list]+[binary]+1+2.3++[deeplist]+[duplicate]+[repeated]+[repeated]+[repeated]+[&lt;&gt;&quot;&amp;]+missing1+missing2+missing3+END">>),
        ?_assert(Fragments =:= [<<"BEGIN+[atom]+[list]+[binary]+1+2.3++[deeplist]+[duplicate]+[repeated]+[repeated]+[repeated]+[&lt;&gt;&quot;&amp;]+">>,
                              {v,<<"missing1">>},
                              <<"+">>,
                              {v,<<"missing2">>},
                              <<"+">>,
                              {v,<<"missing3">>},
                              <<"+END">>])
    ].

-module(eunit_utils).

-include_lib("eunit/include/eunit.hrl").
-include("../include/erlte.hrl").

html_entities_encode_test_() ->

    T1 = erlte_utils:html_entities_encode("<>&\"a"),
    T2 = erlte_utils:html_entities_encode(""),

    [?_assert(T1 =:= "&lt;&gt;&amp;&quot;a"),
    ?_assert(T2 =:= "")].

list_remove_start_list_test_() ->

    T1 = erlte_utils:list_remove_start_list("start", "startOK"),
    T2 = erlte_utils:list_remove_start_list("missing", "OK"),
    T3 = erlte_utils:list_remove_start_list("missing", ""),
    T4 = erlte_utils:list_remove_start_list("", "OK"),

    [
        ?_assert(T1 =:= {true,"OK"}),
        ?_assert(T2 =:= false),
        ?_assert(T3 =:= false),
        ?_assert(T4 =:= {true,"OK"})
    ].
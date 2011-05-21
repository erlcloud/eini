-module(eini_tests).

-author('shino@accense.com').

-include_lib("eunit/include/eunit.hrl").

-import(eini, [parse_string/1]).

setup() ->
  ok.

teardown(_) ->
  ok.

empty_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, []}, parse_string("")),
    ?_assertEqual({ok, []}, parse_string("\n"))
   ]}.

one_section_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, [
                        {{"title", default},
                         []}
                       ]},
                  parse_string(
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {{"title", default},
                         [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]\n"
                    "key1=value1\n"
                   )),
    ?_assertEqual({ok, [
                        {{"title", default},
                         [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=value1\n"
                   ))
   ]}.

two_section_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, [
                        {{"titleA", default},
                         []},
                        {{"titleB", default},
                         []}
                       ]},
                  parse_string(
                    "[titleA]\n"
                    "[titleB]\n"
                   )),
    ?_assertEqual({ok, [
                        {{"titleA", default},
                         [{"keyA1", "valueA1"}]},
                        {{"titleB", default},
                         [{"keyB1", "valueB1"}]}
                       ]},
                  parse_string(
                    "[titleA]\n"
                    "keyA1=valueA1\n"
                    "[titleB]  \n"
                    "keyB1=valueB1\n"
                   ))
   ]}.

syntax_error_title_test() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% セクションタイトルの前に空白
    ?_assertMatch({error, {_Line, _Module, _Reason}}, parse_string(" [title]"))
   ]}.
  

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

empty_one_title_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, ["title", []]}, parse_string("[title]\n"))
   ]}.

syntax_error_title_test() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% セクションタイトルの前に空白
    ?_assertMatch({error, {_Line, _Module, _Reason}}, parse_string(" [title]"))
   ]}.
  

#!/usr/bin/env escript
%% -*- erlang-acc -*-
%%! -env EvnKey EnvValue

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
  code:add_patha("./ebin/"),
  String = "\n[cat1]\n\nkey=va[lu]e\n",
  ?debugVal(eini_lexer:string(String)),
  case eini:parse_string(String) of
    {ok, Res} ->
      io:format("~p~n", [Res]);
    {error, {Line, Mod, Reason}} ->
      io:format("Error at line ~B: ~s~n", [Line, Mod:format_error(Reason)])
  end,
  ok.

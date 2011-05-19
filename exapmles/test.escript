#!/usr/bin/env escript
%% -*- erlang-acc -*-

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
  code:add_patha("../ebin/"),
  String = "\n[cat1]\n\nkey=va[lu]e\n",
  go(String),
  ok.

go(String) ->
  {ok, Tokens} = eini:lex(String),
  io:format("~p~n", [Tokens]),
  case eini:parse_tokens(Tokens) of
    {ok, Res} ->
      io:format("~p~n", [Res]);
    {error, {Line, Reason}} ->
      io:format("Error at line ~B: ~s~n", [Line, Reason])
  end,
  ok.

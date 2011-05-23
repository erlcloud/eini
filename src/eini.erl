%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(eini).

-author('shino@accense.com').

-export([parse_string/1, parse_file/1]).
%% for debug use
-export([lex/1, parse_tokens/1]).

%% Result form:
%% [
%%  {"title1", {"subtitle1", KVs}},
%%  {"title1", {"subbitle2", KVs}},
%%  {"title2", {default,     KVs}}
%% ].
%% KVs are proplists of keys and values
parse_string(String) when is_binary(String) ->
  parse_string(binary_to_list(String));
parse_string(String) when is_list(String) ->
  case lex(String) of
    {ok, Tokens} ->
      parse_tokens(Tokens);
    {error, Reason} ->
      {error, Reason}
  end.

parse_file(Filename) ->
  case file:read_file(Filename) of
    {ok, Binary} -> parse_string(Binary);
    Error -> Error
  end.

lex(String) when is_binary(String) ->
  lex(binary_to_list(String));
lex(String) when is_list(String) ->
  %% Add \n char at the end if does NOT end by \n
  %% TOD(shino): more simple logic?
  String2 = case String of
              "" ->
                "\n";
              _NotEmpty ->
                case lists:last(String) of
                  $\n ->
                    String;
                  _ ->
                    String ++ "\n"
                end
            end,
  case eini_lexer:string(String2) of
    {ok, [{break, _Line}|RestTokens], _EndLine} ->
      {ok, RestTokens};
    {ok, Tokens, _EndLine} ->
      {ok, Tokens};
    ErrorInfo ->
      {error, ErrorInfo}
  end.
  
parse_tokens(Tokens) ->
  case eini_parser:parse(Tokens) of
    {ok, Res} ->
      {ok, Res};
    {error, {Line, Mod, Reason}} ->
      {error, {Line, Mod:format_error(Reason)}}
  end.

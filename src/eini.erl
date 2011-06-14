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

-export([parse/1]).

%% for debug use
-export([lex/1, parse_tokens/1]).

-type sections() :: [section()].
-type section() :: {Title::atom(), [property()]}.
-type property() :: {Key::atom(), Value::binary()}.

-type reason() :: {illegal_character, Line::integer(), Reason::string()}
                | {syntax_error, Line::integer(), Reason::string()}
                | {duplicate_title, Title::binary()}
                | {duplicate_key, Title::binary(), Key::binary()}.

-spec parse(Content:: string() | binary()) -> {ok, sections()}
                                            | {error, reason()}.
parse(Content) when is_binary(Content) ->
  parse(binary_to_list(Content));
parse(Content) when is_list(Content) ->
  case lex(Content) of
    {ok, Tokens} ->
      parse_and_validate(Tokens);
    {error, Reason} ->
      {error, Reason}
  end.

parse_and_validate(Tokens) ->
  case parse_tokens(Tokens) of
    {ok, Parsed} ->
      validate(Parsed);
    {error, Reason} ->
      {error, Reason}
  end.

-spec lex(string()) -> {ok, list(Token::tuple())}
                     | {error, {illegal_character, Line::integer(), Reason::string()}}.
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
    {error, {ErrorLine, Mod, Reason}, _EndLine} ->
      {error, {illegal_character, ErrorLine, Mod:format_error(Reason)}}
  end.
  
-spec parse_tokens(Token::tuple()) ->
                      {ok, sections()}
                    | {error, {syntax_error, Line::integer(), Reason::string()}}.
parse_tokens(Tokens) ->
  case eini_parser:parse(Tokens) of
    {ok, Res} ->
      {ok, Res};
    {error, {Line, Mod, Reason}} ->
      {error, {syntax_error, Line, Mod:format_error(Reason)}}
  end.

-spec validate(sections()) ->
                      {ok, sections()}
                    | {error, {duplicate_title, Title::binary()}}
                    | {error, {duplicate_key, Title::binary(), Key::binary()}}.
validate(Sections) ->
  validate(Sections, [], []).

validate([], _AccTitles, AccSections) ->
  {ok, lists:reverse(AccSections)};
validate([{Title, Properties} = Section | Sections], AccTitles, AccSections) ->
  case lists:member(Title, AccTitles) of
    true ->
      {error, {duplicate_title, Title}};
    false ->
      validate(Sections, [Title|AccTitles], [Section|AccSections], Properties, [])
  end.

validate(Sections, AccTitles, AccSections, [], _AccKeys) ->
  validate(Sections, AccTitles, AccSections);
validate(Sections, AccTitles, AccSections, [{Key, _Value}|Properties], AccKeys) ->
  case lists:member(Key, AccKeys) of
    true ->
      {error, {duplicate_key, hd(AccTitles), Key}};
    false ->
      validate(Sections, AccTitles, AccSections, Properties, [Key|AccKeys])
  end.

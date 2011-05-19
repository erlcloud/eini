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

%% @author: shino@accense.com

Definitions.

A = [a-z][a-zA-Z0-9_\.]
D = [0-9]
S = [\s\t]
B = [\n\r]

Rules.

{B};.*{B}        : {skip_token, "\n"}.
{B}{S}*{B}       : {skip_token, "\n"}.
{B}              : {token, {break,   TokenLine, TokenChars}}.
=                : {token, {'=',     TokenLine}}.
\[               : {token, {'[',     TokenLine}}.
\]               : {token, {']',     TokenLine}}.
{S}+             : {token, {blank,   TokenLine, TokenChars}}.
"{A}*"           : {token, {quoted,  TokenLine, TokenChars}}.
{A}*             : {token, {word,    TokenLine, TokenChars}}.
;.*              : {token, {comment, TokenLine, TokenChars}}.
%% {S}*{A}+         : {token, {value, TokenLine, to_atom(TokenChars)}}.
%% {S}*{D}+         : {token, {value, TokenLine, to_integer(TokenChars)}}.
%% {S}*{D}+\.{D}+   : {token, {value, TokenLine, to_float(TokenChars)}}.
%% [\000-\s]+       : skip_token.

Erlang code.

%% -compile({inline, to_key/2}).
%% to_key(TokenChars, TokenLen) ->
%%     S = string:substr(TokenChars, 1, TokenLen-1),
%%     K = string:strip(S),
%%     list_to_atom(K).

%% -compile({inline, to_atom/1}).
%% to_atom(TokenChars) ->
%%     list_to_atom(string:strip(TokenChars, left)).

%% -compile({inline, to_integer/1}).
%% to_integer(TokenChars) ->
%%     list_to_integer(string:strip(TokenChars, left)).

%% -compile({inline, to_float/1}).
%% to_float(TokenChars) ->
%%     list_to_float(string:strip(TokenChars, left)).

%% -compile({inline, to_string/1}).
%% to_string(TokenChars) ->
%%     S = string:strip(TokenChars, left),
%%     string:substr(S, 2, length(S)-2).

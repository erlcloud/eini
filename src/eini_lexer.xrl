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

Erlang code.

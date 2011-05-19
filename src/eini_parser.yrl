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

Nonterminals sections section properties property single_value values.

Terminals key value '[' ']' '=' '$break'.

Rootsymbol sections.

sections -> section : ['$1'].
sections -> section sections : ['$1' | '$2'].

section -> '$break' '[' key ']' '$break' properties : {'$3', '$6'}.
section -> '[' key ']' '$break' properties : {'$2', '$5'}.

properties -> property : ['$1'].
properties -> property properties : ['$1' | '$2'].

property -> key '=' values '$break' : {'$1', '$3'}.

values -> single_value : ['$1'].
values -> single_value values : ['$1' | '$2'].

single_value ->  value : '$1'.
single_value -> '[' : '['.
single_value -> '=' : '='.
single_value -> ']' : ']'.



Erlang code.

%% -compile({inline, value_of/1}).
%% value_of(Token) ->
%%     element(3, Token).

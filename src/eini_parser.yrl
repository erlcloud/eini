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

Nonterminals
  sections section
  title_part
  title
  properties property
  values single_value.

Terminals
  '[' ']' '='
  end_of_file
  blank
  word quoted
  value
  comment
  break.

%% TODO(shino): extract string by value_of and flatten (if needed)

Rootsymbol sections.

sections -> end_of_file : [].
sections -> break end_of_file : [].
sections -> section : ['$1'].
sections -> section sections : ['$1' | '$2'].

section -> title_part properties : {'$1', '$2'}.

title_part -> title break : '$1'.
title_part -> break title break : '$2'.

%% TODO(shino): add option as "quited"
title -> '[' word ']'       : {value_of('$2'), default}.
title -> '[' word ']' blank : {value_of('$2'), default}.
title -> '[' word blank quoted ']'       : {value_of('$2'), value_of('$4')}.
title -> '[' word blank quoted ']' blank : {value_of('$2'), value_of('$4')}.

properties -> end_of_file : [].
properties -> property properties : ['$1' | '$2'].

property -> word '=' values break : {value_of('$1'), lists:flatten('$3')}.

values -> single_value : ['$1'].
values -> single_value values : ['$1' | '$2'].

single_value ->  word    : value_of('$1'). 
single_value ->  value   : value_of('$1').
single_value ->  blank   : value_of('$1').
single_value ->  comment : value_of('$1').
single_value -> '['      : "[".
single_value -> '='      : "=".
single_value -> ']'      : "]".


Erlang code.

-compile({inline, value_of/1}).
value_of(Token) ->
    element(3, Token).

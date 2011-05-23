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
  whole
  sections section
  title_part
  title
  property_with_skip_lines
  properties property
  key_part
  values single_value
  skip_lines
  comment_line.

Terminals
  '[' ']' '='
  blank
  word quoted
  value
  comment
  break.

Rootsymbol whole.

whole -> sections : '$1'.
whole -> skip_lines sections : '$2'.
%% whole -> sections skip_lines: '$1'.

sections -> '$empty' : [].
sections -> section sections : ['$1' | '$2'].

section -> title_part properties : {'$1', '$2'}.

title_part -> title break                  : '$1'.
title_part -> title blank break            : '$1'.
title_part -> title break skip_lines       : '$1'.
title_part -> title blank break skip_lines : '$1'.

title -> '[' word ']'              : {value_of('$2'), default}.
title -> '[' word blank quoted ']' : {value_of('$2'), value_of('$4')}.

properties -> '$empty' : [].
properties -> property_with_skip_lines properties : ['$1' | '$2'].

property_with_skip_lines -> property : '$1'.
property_with_skip_lines -> property skip_lines : '$1'.

property -> key_part '=' values break : {value_of('$1'), strip_values('$3')}.

key_part -> word : '$1'.
key_part -> word blank : '$1'.
key_part -> blank word : '$2'.
key_part -> blank word blank : '$2'.

values -> single_value : ['$1'].
values -> single_value values : ['$1' | '$2'].

%% At value position, any characters are accepted AS IS.
single_value ->  word    : value_of('$1'). 
single_value ->  value   : value_of('$1').
single_value ->  blank   : value_of('$1').
single_value ->  comment : value_of('$1').
single_value -> '['      : "[".
single_value -> '='      : "=".
single_value -> ']'      : "]".

%% ONLY a comment line at the beggining of file is NOT skipped by leex
skip_lines -> comment_line : ['$1'].
skip_lines -> comment_line skip_lines : ['$1', '$2'].

comment_line -> comment break : '$1'.


Erlang code.

-compile({inline, value_of/1}).
value_of(Token) ->
  element(3, Token).

-compile({inline, strip_values/1}).
strip_values(Values) ->
  string:strip(string:strip(lists:flatten(Values), both, $\s), both, $\t).

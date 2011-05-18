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

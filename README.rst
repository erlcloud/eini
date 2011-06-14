###########################
Eini - An Erlang INI parser
###########################

:Original: https://github.com/devinus/zucchini

Example
=======

Input file::

  [title1]
  key = value
  key2 = value2
  [title2]
  key = value

Result form::

  [
   {title1,
    [{key, <<"value">>},
     {key2, <<"value2">>}]},
   {title2,
    [{key, <<"value">>}]}
  ].


Copyright by Accense Technology, Inc.

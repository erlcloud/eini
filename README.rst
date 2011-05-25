###########################
Eini - An Erlang INI parser
###########################

:Original: https://github.com/devinus/zucchini

Example
=======

Input file::

  [title1 "subtitle1"]
  key = value
  key2 = value2
  [title1 "subtitle2"]
  key = value
  [title2]
  key = value

Result form::

  [
   {"title1", [{"subtitle1",
                [{"key", "value"},
                 {"key2", "value2"}]
               },
               {"subbitle2", [{"key", "value"}]}
              ]},
   {"title2", [{default, [{"key", "value"}]}]}
  ].


Copyright by Accense Technology, Inc.

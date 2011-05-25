###########################
Eini - An Erlang INI parser
###########################

:Original: https://github.com/devinus/zucchini

Example
=======

Input file::

  [title1 "subtitle1"]
  key = value
  [title1 "subtitle2"]
  key = value
  [title2]
  key = value

Result form::

  [
   {"title1", [{"subtitle1", KVs}},
               {"subbitle2", KVs}}],
   {"title2", {default,     KVs}}
  ].


Copyright by Accense Technology, Inc.

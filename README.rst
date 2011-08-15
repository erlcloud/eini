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

History
=======

1.0.0
-----

Initial release

Copyright
=========

Copyright 2011 by Accense Technology, Inc.

License
=======

Apache License v2.
See ``LICENSE`` file for detail.

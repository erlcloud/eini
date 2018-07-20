###########################
Eini - An Erlang INI parser
###########################

:Original: https://github.com/devinus/zucchini

.. image:: https://secure.travis-ci.org/erlcloud/eini.png?branch=master
    :target: https://travis-ci.org/erlcloud/eini
    :alt: Build Status

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

1.2.4
-------
- Unicode in values

1.2.3
-------
- Misc changes Hex and pre-R18

1.2.1
-----
- cobertura report with covertool

1.2.0
-----
- no change 1.1.0

1.1.0
-----

- To Store ini data.

1.0.1
-----

:release: 2011-08-17
:summary: Line number bug fix

- Line numbers in error messages were wrong.
  Thanks to @Hexa for reporting the bug.

1.0.0
-----

:release: 2011-08-15
:summary: Initial release

Copyright
=========

Copyright 2011 by Accense Technology, Inc.

License
=======

Apache License v2.
See ``LICENSE`` file for detail.

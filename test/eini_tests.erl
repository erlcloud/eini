-module(eini_tests).

-author('shino@accense.com').

-include_lib("eunit/include/eunit.hrl").

-import(eini, [parse_string/1]).

setup() ->
  ok.

teardown(_) ->
  ok.

empty_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, []}, parse_string("")),
    ?_assertEqual({ok, []}, parse_string("\n"))
   ]}.

one_section_title_only_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% Title only
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "[title]\n"
                   )),
    %% Title only, but trailing spaces
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "[title]  \n"
                   )),
    %% Title only, but comment lines
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "; comment line\n"
                    "  \n"
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "; comment line\n"
                    "; comment line 2\n"
                    "  \n"
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "; comment line\n"
                    "; comment line 2\n"
                    "  \n"
                    "[title]\n"
                    "; comment after section title"
                   )),
    %% Title only, but preceding blank lines
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "\n"
                    "  \n"
                    "[title]\n"
                   )),
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "  \n"
                    "\n"
                    "[title]\n"
                   )),
    %% Title only, but preceding blank lines and trailing spaces
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "\n"
                    "  \n"
                    "[title]\t\s\n"
                   )),
    %% Title only, but trailing blank lines
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "[title]\n"
                    "\n"
                    "  \n"
                    "\n"
                   )),
    %% Title only, but trailing spaces and trailing blank lines
    ?_assertEqual({ok, [
                        {"title", []}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "\n"
                    "\n"
                   ))
   ]}.

one_section_title_only_syntax_error_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% No ] char
    ?_assertMatch({error, {1, _Reason}},
                  parse_string(
                    "[title\n"
                   ))
   ]}.

one_section_title_and_one_prop_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% Simple case
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]\n"
                    "key1=value1\n"
                   )),
    %% title has trailing spaces
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=value1\n"
                   )),
    %% Single blank line between title and a prop
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "\n"
                    "key1=value1\n"
                   )),
    %% Single comment line between title and a prop
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "; comment\n"
                    "key1=value1\n"
                   )),
    %% Single comment line after a prop
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=value1\n"
                    "; comment\n"
                   )),
    %% Multi blank lines between title and a prop
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "\n"
                    "    \n"
                    "\n"
                    "key1=value1\n"
                   )),
    %% Multi blank lines after a prop
    ?_assertEqual({ok, [
                        {"title", 
                         [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=value1\n"
                    "\n"
                    "    \n"
                    "\n"
                   )),
    %% Multi blank lines between title and a prop and
    %% multi blank lines after a prop
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "\n"
                    "    \n"
                    "\n"
                    "key1=value1\n"
                    "\n"
                    "    \n"
                    "\n"
                   )),

    %% value has [ char
    ?_assertEqual({ok, [
                        {"title", [{"key1", "va[lue1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=va[lue1\n"
                   )),
    %% value has ] char
    ?_assertEqual({ok, [
                        {"title", [{"key1", "valu]e1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=valu]e1\n"
                   )),
    %% value has [ and ] chars
    ?_assertEqual({ok, [
                        {"title", [{"key1", "va[lu]e1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=va[lu]e1\n"
                   )),
    %% value has ; char
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1;continue"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=value1;continue\n"
                   )),
    %% key has trailing spaces
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1   =value1\n"
                   )),
    %% value has preceding and trailing spaces
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1"}]}
                       ]},
                  parse_string(
                    "[title]  \n"
                    "key1=  value1  \n"
                   )),
    %% value has characters which can not used in titles or keys
    ?_assertEqual({ok, [
                        {"title", [{"key1", "value1$% '""#!+*=@/:+"}]}
                       ]},
                  parse_string(
                    "[title]\n"
                    "key1=value1$% '""#!+*=@/:+\n"
                   ))
   ]}.

one_section_title_and_two_props_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% Simple case
    ?_assertEqual({ok, [
                        {"title",
                         [{"key1", "value1"},
                          {"key2", "value2"}]}
                       ]},
                  parse_string(
                    "[title]\n"
                    "key1=value1\n"
                    "key2=value2\n"
                   )),
    %% Blank lines
    ?_assertEqual({ok, [
                        {"title",
                         [{"key1", "value1"},
                          {"key2", "value2"}]}
                       ]},
                  parse_string(
                    "[title]\n"
                    "\n"
                    "key1=value1\n"
                    "  \n"
                    "\n"
                    "key2=value2\n"
                    "\n"
                    "\n"
                   ))
   ]}.

two_section_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    ?_assertEqual({ok, [
                        {"titleA", []},
                        {"titleB", []}
                       ]},
                  parse_string(
                    "[titleA]\n"
                    "[titleB]\n"
                   )),
    ?_assertEqual({ok, [
                        {"titleA",
                         [{"keyA1", "valueA1"}]},
                        {"titleB",
                         [{"keyB1", "valueB1"}]}
                       ]},
                  parse_string(
                    "[titleA]\n"
                    "keyA1=valueA1\n"
                    "[titleB]  \n"
                    "keyB1=valueB1\n"
                   ))
   ]}.

syntax_error_title_test() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
    %% セクションタイトルの前に空白
    ?_assertMatch({error, {_Line, _Module, _Reason}}, parse_string(" [title]"))
   ]}.
  

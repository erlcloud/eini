-module(eini).

-export([parse_string/1, parse_file/1]).

parse_string(String) when is_binary(String) ->
    parse_string(binary_to_list(String));
parse_string(String) when is_list(String) ->
    {ok, Tokens, _} = eini:string(String),
    eini_parser:parse(Tokens).

parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> parse_string(Binary);
        Error -> Error
    end.

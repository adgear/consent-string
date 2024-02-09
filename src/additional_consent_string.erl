-module(additional_consent_string).
-include("consent_string.hrl").

-compile([inline]).

-export([parse/1]).
-ifdef(TEST).
-export([parse_version/1, parse_atp_ids/1]).
-endif.

-define(KNOWN_SEPARATOR, "~").


-spec parse(binary()) ->
    {ok, addtl_consent()} | {error, invalid_ac_string}.

% @doc Parses an additional consent string, returning the version
% number and the list of numerical ATP ids.
%
% @returns `{ok, #addtl_consent{}}' if the string is valid,
% otherwise an appropriate error is returned.
%
parse(Bin) when is_binary(Bin) ->
    try
        {ok, Version, <<?KNOWN_SEPARATOR, Bin1/binary>>} = parse_version(Bin),
        case Version of
            1 ->
                {ok, AtpIds, <<"">>} = parse_atp_ids(Bin1),
                {ok, #addtl_consent {
                    version = Version,
                    atp_ids = AtpIds
                }};
            2 ->
                {ok, AtpIds, <<?KNOWN_SEPARATOR, "dv.", Bin2/binary>>} = parse_atp_ids(Bin1),
                {ok, DisclosedAtpIds, <<"">>} = parse_atp_ids(Bin2),
                {ok, #addtl_consent {
                    version = Version,
                    atp_ids = AtpIds,
                    disclosed_atp_ids = DisclosedAtpIds
                }}
        end
    catch
        _:_ ->
            {error, invalid_ac_string}
    end;
parse(_) ->
    {error, invalid_ac_string}.

parse_version(Bin) ->
    parse_version(Bin, <<"">>).

parse_version(<<Bin, Rest/binary>>, Acc) when Bin >= $0, Bin =< $9 ->
    parse_version(Rest, <<Acc/binary, Bin>>);
parse_version(Rest, Acc) ->
    case safe_binary_to_integer(Acc) of
        undefined ->
            {error, invalid_ac_string};
        eof ->
            {error, invalid_ac_string};
        Version ->
            {ok, Version, Rest}
    end.

parse_atp_ids(Bin) ->
    parse_atp_ids(Bin, <<"">>, []).

parse_atp_ids(<<Bin, Rest/binary>>, Acc, Ids) when Bin >= $0, Bin =< $9 ->
    parse_atp_ids(Rest, <<Acc/binary, Bin>>, Ids);
parse_atp_ids(<<".", Rest/binary>>, Acc, Ids) when Rest =/= <<"">> ->
    case safe_binary_to_integer(Acc) of
        undefined ->
            {error, invalid_ac_string};
        eof ->
            {error, invalid_ac_string};
        Id ->
            parse_atp_ids(Rest, <<"">>, [Id | Ids])
    end;
parse_atp_ids(<<Rest/binary>>, Acc, Ids) ->
    case safe_binary_to_integer(Acc) of
        undefined -> {error, invalid_ac_string};
        eof       -> {ok, lists:reverse(Ids), Rest};
        Id        -> {ok, lists:reverse([Id | Ids]), Rest}
    end.


-spec safe_binary_to_integer(binary()) -> eof | undefined | integer().
safe_binary_to_integer(<<"">>) ->
    eof;
safe_binary_to_integer(Bin) ->
    try
        binary_to_integer(Bin, 10)
    catch
        _:_ ->
            undefined
    end.


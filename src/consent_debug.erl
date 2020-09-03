-module(consent_debug).
-include("consent_string.hrl").

-export([
    print_vendors/1,
    vendors_to_array/1,
    bitstring_to_array/1
]).

bitstring_to_array(Bitstring) ->
    bitstring_to_array(1, Bitstring).

bitstring_to_array(_, <<>>) ->
    [];
bitstring_to_array(N, <<X:1, Rest/bitstring>>) ->
    case X of
        1 -> [N | bitstring_to_array(N + 1, Rest)];
        0 -> bitstring_to_array(N + 1, Rest)
    end.

print_vendors(Consent) ->
    Vendors = vendors_to_array(Consent),
    io:format(user, "~p~n", [Vendors]).

vendors_to_array(#consent{ max_vendor_id = Max } = Consent) ->
    lists:filter(fun(X) -> consent_string:vendor(X, Consent) end,
                 lists:seq(1, Max)).

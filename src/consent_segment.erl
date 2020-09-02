-module(consent_segment).
-include("consent_string.hrl").

-export([
    type/1
]).

-spec type(consent_segment()) -> pos_integer().

type(#consent_segment { type = T }) ->
    T.

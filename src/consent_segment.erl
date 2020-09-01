-module(consent_segment).
-include("consent_string.hrl").

-export([
    type/1
]).

type(#consent_segment { type = T }) ->
    T.

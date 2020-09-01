-module(consent_segment_test).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

consent_segment_type_test() ->
    CS = #consent_segment { type = 1 },
    Type = consent_segment:type(CS),
    ?assertEqual(Type, 1).

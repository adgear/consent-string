-module(v2_publisher_purposes_segment_test).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

publisher_purposes_test() ->
    TCF = <<"CO4xE1AO4xE1AAKALANLA0CsAP_AAH_AABpYGYtd_X9fb2vj-_5999t0eY1f9_63v-wzjgeNs-8NyZ_X_L4Xr2MyvB34pq4KmR4Eu3LBAQVlHGHcTQmQwIkVqTLsak2Mq7NKJ7JEilMbM2dYGG1Pn8XTuZCY70_sf__z_3-_-___67YGXkEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7K4CPUACABAYgIwIgQYgoxZBAAAAAElEQAkAwIBEARAIAAQArQEIACJAEFgBIGAQACoGhYARRBKBIQZHBUcogQFSLRQTzRgAA.f_gAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),
    #consent { segments = Segments } = Actual,

    ?assertEqual(1, length(Segments)),
    [FirstSeg | _] = Segments,

    #consent_segment {
       type = SegType,
       segment = #consent_segment_entry_publisher_purposes {
           pub_purposes_consent = PPC,
           pub_purposes_li_transparency = PPLIT,
           num_custom_purposes = Num,
           custom_purposes_consent = CustomPurposes,
           custom_purposes_li = CustomPurposesLI
       }
    } = FirstSeg,

    ?assertEqual(3, SegType),
    ?assertEqual(<<255, 192, 0>>, PPC),
    ?assertEqual(<<0, 0, 0>>, PPLIT),
    ?assertEqual(0, Num),
    ?assertEqual(<<>>, CustomPurposes),
    ?assertEqual(<<>>, CustomPurposesLI).

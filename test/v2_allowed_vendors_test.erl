-module(v2_allowed_vendors_test).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

allowed_segment_has_all_three_segs_test() ->
    TCF = <<"CO4qQtcO4qQtcCoACBENAzCMAP_AAEPAAAAAGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.IGVtd_X9fb2vj-_599_t0eY1f9_63t-wzjheNs-8NyZ_X_J4Xv2MyvB34pqYKmR4kunbBAQdtHGncTQgBwIlVqTLsYk2MjzNKJ7JEmlsbe2dYGH9Pn8XT_ZKZ70-v___7v3______7oA.QGVtd_X9fb2vj-_599_t0eY1f9_63t-wzjheNs-8NyZ_X_J4Xv2MyvB34pqYKmR4kunbBAQdtHGncTQgBwIlVqTLsYk2MjzNKJ7JEmlsbe2dYGH9Pn8XT_ZKZ70-v___7v3______7oA.YAAAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent { disclosed_vendors = DV,
               allowed_vendors = AV,
               publisher_tc = PTC
             } = Actual,

    Segments = [DV, AV, PTC],
    ?assert(lists:any(fun(X) -> X =:= undefined end, Segments) =:= false),

    Ret = lists:usort([DV, AV, PTC]),
    ?assertEqual(3, length(Ret)).

allowed_vendors_test() ->
    TCF = <<"CO4qQtcO4qQtcCoACBENAzCMAP_AAEPAAAAAGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.IGVtd_X9fb2vj-_599_t0eY1f9_63t-wzjheNs-8NyZ_X_J4Xv2MyvB34pqYKmR4kunbBAQdtHGncTQgBwIlVqTLsYk2MjzNKJ7JEmlsbe2dYGH9Pn8XT_ZKZ70-v___7v3______7oA.QGVtd_X9fb2vj-_599_t0eY1f9_63t-wzjheNs-8NyZ_X_J4Xv2MyvB34pqYKmR4kunbBAQdtHGncTQgBwIlVqTLsYk2MjzNKJ7JEmlsbe2dYGH9Pn8XT_ZKZ70-v___7v3______7oA.YAAAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),
    #consent { allowed_vendors = Segment } = Actual,

    #consent_segment {
       type    = SegType,
       segment = #consent_segment_entry_allowed_vendors {
           max_vendor_id = MaxVendorId,
           entries = #entry_bitfield {
               fields = Entries
           }
       }
    } = Segment,

    ?assertEqual(2, SegType),
    ?assertEqual(811, MaxVendorId),
    ?assertNotEqual(0, bit_size(Entries)).

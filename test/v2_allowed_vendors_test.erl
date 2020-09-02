-module(v2_allowed_vendors).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

allowed_vendors_test() ->
    TCF = <<"CO4qQtcO4qQtcCoACBENAzCMAP_AAEPAAAAAGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.IGVtd_X9fb2vj-_599_t0eY1f9_63t-wzjheNs-8NyZ_X_J4Xv2MyvB34pqYKmR4kunbBAQdtHGncTQgBwIlVqTLsYk2MjzNKJ7JEmlsbe2dYGH9Pn8XT_ZKZ70-v___7v3______7oA.QGVtd_X9fb2vj-_599_t0eY1f9_63t-wzjheNs-8NyZ_X_J4Xv2MyvB34pqYKmR4kunbBAQdtHGncTQgBwIlVqTLsYk2MjzNKJ7JEmlsbe2dYGH9Pn8XT_ZKZ70-v___7v3______7oA.YAAAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),
    #consent { segments = Segments } = Actual,

    ?assertEqual(3, length(Segments)),
    [_FirstSeg, SecondSeg | _] = Segments,

    #consent_segment {
       type    = SegType,
       segment = #consent_segment_entry_allowed_vendors {
           max_vendor_id = MaxVendorId,
           entries = #entry_bitfield {
               fields = Entries
           }
       }
    } = SecondSeg,

    ?assertEqual(2, SegType),
    ?assertEqual(811, MaxVendorId),
    ?assertNotEqual(0, bit_size(Entries)).

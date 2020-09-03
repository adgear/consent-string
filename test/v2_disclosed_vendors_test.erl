-module(v2_disclosed_vendors_test).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

disclosed_vendor_bitfield_test() ->
    TCF = <<"CO3rrVNO3vQn-B7ACBENAwCMAP_AAH_AAAAAGLtX_T9fb2vj-_Z99_tkeYwf95y3p-wzhheMs-8NyZeH_B4Wv2MyvBX4JiQKGRgksjLBAQdtHGlcTQgBwIlViTLMYk2MjzNKJrJEilsbe2dYGD9vn8HT3ZCY70-vv__7v3ff_3gYsQSYKl8BIkJYwEk2aUQpgAhXEBUg4BKCEYSDSw0JFATsDgI9QAIAEBgABAgBACCiEkEAAgAASURACADAgFQBEAgABACNAQgAIkAAWAEgYBAAKgaFgBFEEIAhBgcFRyiBAVItEAAA.IGLtX_T9fb2vj-_Z99_tkeYwf95y3p-wzhheMs-8NyZeH_B4Wv2MyvBX4JiQKGRgksjLBAQdtHGlcTQgBwIlViTLMYk2MjzNKJrJEilsbe2dYGD9vn8HT3ZCY70-vv__7v3ff_3g">>, %% bitfield
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent { disclosed_vendors = Segment,
               allowed_vendors = AV,
               publisher_tc = PTC } = Actual,

    ?assertEqual(undefined, AV),
    ?assertEqual(undefined, PTC),

    #consent_segment {
       type    = SegType,
       segment = #consent_segment_entry_disclosed_vendors {
           max_vendor_id = MaxVendorId,
           entries = #entry_bitfield {
               fields = Entries
           }
       }
    } = Segment,

    ?assertEqual(1, SegType),
    ?assertEqual(791, MaxVendorId),
    ?assertNotEqual(0, bit_size(Entries)).

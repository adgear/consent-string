-module(v2_vendor_legitimate_interests_test).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

consent_string_range_with_legitimate_interests_test() ->
    TCFv2Legitimate = <<"CO4Hcm2O4Hcm2AKAJAFRAzCsAP_AAH_AAAqIGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCFv2Legitimate),

    #consent {
       vendor_legitimate_interests = #vendor_legitimate_interests {
           max_vendor_id = MaxVendorId,
           interests = #vendor_legitimate_interests_entry {
               fields = Bitfield
           }
       }
    } = Actual,

    Expected = <<65,38,26,151,192,64,144,150,48,18,77,154,85,10,32,66,21,196,
                 133,64,56,0,162,132,97,104,210,195,66,71,5,59,35,128,143,80,
                 0,128,4,6,32,33,2,32,65,136,40,133,144,64,0,128,0,18,81,16,
                 2,0,48,32,17,0,68,2,0,1,0,35,64,66,0,8,144,4,22,0,72,24,4,0,
                 10,129,161,96,4,81,4,160,72,65,129,193,81,202,32,64,84,139,
                 69,4,243>>,

    ?assertEqual(808, MaxVendorId),
    ?assertEqual(Expected, Bitfield).

consent_string_with_vendor_legitimate_interests_range_test() ->
    TCF = <<"CO4xF7sO4xF8KAHABBENA0CsAP_AAH_AAAAAGSQKQABQAKAAyAB4AIAAVgAuADIAHAAQAAkgBSAFQALQAXgAyABoADwAIsARwBIACYAE-ALQAtgBtAD0AIQATYAnQBcgDSAHOAO6AfoB_AEIAJ0AVkAzQBnQDTgG_AUkAr4BeYDGQGSAZJAaAAEAAsAB4AFQALgAZAA4ACAAFQANAAeABMACeAF0ANoAegBCAC5AGkAOcAdwA_QCEAHkAXmAyQAA.f_gAD_gAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent {
       vendor_legitimate_interests = #vendor_legitimate_interests {
           max_vendor_id = MaxVendorId,
           interests = #vendor_legitimate_interests_range {
               num_entries = NumEntries,
               entries = Entries
           }
       }
    } = Actual,

    ExpectedLegitimateEntries =
        [804,755,484,264,253,238,231,210,185,132,
         122,109,93,79,76,60,52,42,32,28,25,23,21,
         15,11,2],

    ?assertEqual(804, MaxVendorId),
    ?assertEqual(Entries, ExpectedLegitimateEntries).

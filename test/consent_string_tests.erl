-module(consent_string_tests).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CONSENT_STRING_1, <<4,225,5,16,12,142,16,81,0,200,1,192,4,49,13,0,142,0,0,0,125,188,0,64,1,32>>).

consent_string_v1_test() ->
    {ok, CS1} = consent_string:parse(?CONSENT_STRING_1),

    #consent {
        version = 1,
        created = 15100821554,
        last_updated = 15100821554,
        cmp_id = 7,
        cmp_version = 1,
        consent_screen = 3,
        consent_language = <<"EN">>,
        vendor_list_version = 8,
        purposes_allowed = <<224,0,0>>, % 1,2,3
        max_vendor_id = 2011,
        encoding_type = 1,
        vendors = #vendor_range {
            default_consent = 1,
            num_entries = 1,
            entries = [9]
        }
    } = CS1,

    true = consent_string:purpose(1, CS1),
    true = consent_string:purpose([1, 2, 3], CS1),
    false = consent_string:purpose([1, 2, 3, 4], CS1),
    true = consent_string:vendor(1, CS1),
    false = consent_string:vendor(9, CS1),
    false = consent_string:vendor(2012, CS1).

consent_string_v2_test() ->
    TCFv2 = <<"CO4Hcm2O4Hcm2AKAJAFRAzCsAP_AAH_AAAqIGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAAAAAAAAA">>,
    Ret = consent_string:parse_b64(TCFv2),
    ok.

consent_string_wild_test() ->
    % ngrep -q port -d lo -W single port 8083 | grep '"consent"' | awk 'BEGIN {FS="\"consent\":\""} {print $2}' | cut -d '"' -f1
    {ok, File} = file:read_file("test/consent.data"),
    Data = binary:split(File, <<"\n">>, [global, trim]),
    lists:foreach(fun (Consent) ->
        {ok, CS} = consent_string:parse_b64(Consent),
        consent_string:purpose(1, CS),
        consent_string:vendor(100, CS)
    end, Data).

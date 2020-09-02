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
    {ok, Actual} = consent_string:parse_b64(TCFv2),

    ?assertEqual(Actual#consent.version, 2),
    ?assertEqual(Actual#consent.created, 15973861814),
    ?assertEqual(Actual#consent.last_updated, 15973861814),
    ?assertEqual(Actual#consent.cmp_id, 10),
    ?assertEqual(Actual#consent.cmp_version, 9),
    ?assertEqual(Actual#consent.consent_screen, 0),
    ?assertEqual(Actual#consent.consent_language, <<"FR">>),
    ?assertEqual(Actual#consent.vendor_list_version, 51),
    ?assertEqual(Actual#consent.tcf_policy_version, 2),
    ?assertEqual(Actual#consent.is_service_specific, 1),
    ?assertEqual(Actual#consent.use_non_standard_stacks, 0),
    ?assertEqual(Actual#consent.purposes_one_treatment, 0),
    ?assertEqual(Actual#consent.publisher_cc, <<"FR">>),
    ?assertEqual(Actual#consent.purposes_allowed, <<255, 192, 0>>),
    ?assertEqual(Actual#consent.max_vendor_id, 811),
    ?assertEqual(Actual#consent.encoding_type, 0).

consent_string_range_test() ->
    TCF = <<"CO4cLaDO4cMW-AHABBENA0CsAP_AAH_AAAAAGSQKAABQAKAAyAB4AIAAVgAuADIAHAAQAAkgBSAFQALQAXgAyABoADwAIsARwBIACYAE-ALQAtgBtAD0AIQATYAnQBcgDSAHOAO6AfoB_AEIAJ0AVkAzQBnQDTgG_AUkAr4BeYDJAMkgNAACAAWAA8ACoAFwAMgAcABAACoAGgAPAAmABPAC6AG0APQAhABcgDSAHOAO4AfoBCADyALzAZIAAAAA.f_gAD_gAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent {
            vendors = #vendor_range {
                num_entries = NumEntries,
                entries = Entries
            }
    } = Actual,

    ?assertEqual(Entries,
                 [804,755,702,658,447,423,413,410,345,314,264,
                  {253,254},238,231,210,185,157,155,132,122,109,
                  {90,91},79,76, {71,72},69,60,52,50,47,45,
                  {41,42},36,32,28,{23,25},21,{15,16},{10,12},2]).


consent_string_wild_test() ->
    % ngrep -q port -d lo -W single port 8083 | grep '"consent"' | awk 'BEGIN {FS="\"consent\":\""} {print $2}' | cut -d '"' -f1
    {ok, File} = file:read_file("test/consent.data"),
    Data = binary:split(File, <<"\n">>, [global, trim]),
    lists:foreach(fun (Consent) ->
        {ok, CS} = consent_string:parse_b64(Consent),
        consent_string:purpose(1, CS),
        consent_string:vendor(100, CS)
    end, Data).

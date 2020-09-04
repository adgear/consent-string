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

    #consent {
       version = Version,
       created = Created,
       last_updated = LastUpdated,
       cmp_id = CmpId,
       cmp_version = CmpVersion,
       consent_screen = ConsentScreen,
       consent_language = ConsentLanguage,
       vendor_list_version = VendorListVersion,
       tcf_policy_version = TcfPolicyVersion,
       is_service_specific = IsServiceSpecific,
       use_non_standard_stacks = UseNonStandardStacks,
       purposes_one_treatment = PurposesOneTreatment,
       publisher_cc = PublisherCC,
       purposes_allowed = PurposesAllowed,
       max_vendor_id = MaxVendorId,
       encoding_type = EncodingType
    } = Actual,

    ?assertEqual(2, Version),
    ?assertEqual(15973861814, Created),
    ?assertEqual(15973861814, LastUpdated),
    ?assertEqual(10, CmpId),
    ?assertEqual(9, CmpVersion),
    ?assertEqual(0, ConsentScreen),
    ?assertEqual(<<"FR">>, ConsentLanguage),
    ?assertEqual(51, VendorListVersion),
    ?assertEqual(2, TcfPolicyVersion),
    ?assertEqual(1, IsServiceSpecific),
    ?assertEqual(0, UseNonStandardStacks),
    ?assertEqual(0, PurposesOneTreatment),
    ?assertEqual(<<"FR">>, PublisherCC),
    ?assertEqual(<<255,192,0>>, PurposesAllowed),
    ?assertEqual(811, MaxVendorId),
    ?assertEqual(0, EncodingType).

consent_string_v2_lookup_vendors_test() ->
    TCF = <<"CO4Hcm2O4Hcm2AKAJAFRAzCsAP_AAH_AAAqIGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAAAAAAAAA">>,
    {ok, Consent} = consent_string:parse_b64(TCF),

    ?assertEqual(true, consent_string:vendor(1, Consent)),
    ?assertEqual(true, consent_string:vendor(2, Consent)),
    ?assertEqual(false, consent_string:vendor(3, Consent)),
    ?assertEqual(false, consent_string:vendor(43, Consent)),
    ?assertEqual(false, consent_string:vendor(46, Consent)),
    ?assertEqual(false, consent_string:vendor(48, Consent)),
    ?assertEqual(true, consent_string:vendor(70, Consent)),
    ?assertEqual(true, consent_string:vendor(71, Consent)),
    ?assertEqual(true, consent_string:vendor(72, Consent)),
    ?assertEqual(true, consent_string:vendor(811, Consent)).

consent_string_v2_lookup_purposes_test() ->
    TCF = <<"CO4Hcm2O4Hcm2AKAJAFRAzCsAP_AAH_AAAqIGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAAAAAAAAA">>,
    {ok, Consent} = consent_string:parse_b64(TCF),

    ?assert(consent_string:purpose(lists:seq(1, 10), Consent)),
    ?assertNot(consent_string:purpose(lists:seq(11, 24), Consent)),
    ?assertNot(consent_string:purpose([1,2,3,11], Consent)).

consent_string_v2_core_string_variables_test() ->
    TCF = <<"CO4Hcm2O4Hcm2AKAJAFRAzCsAP_AAH_AAAqIGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAAAAAAAAA">>,
    {ok, Consent} = consent_string:parse_b64(TCF),

    #consent {
       tcf_policy_version = TPV,
       is_service_specific = ISS,
       use_non_standard_stacks = UNSS,
       special_feature_optins = SFO,

       purposes_li_transparency = PLT,
       purposes_one_treatment = POT
    } = Consent,

    ?assertEqual(2, TPV),
    ?assertEqual(1, ISS),
    ?assertEqual(0, UNSS),
    ?assertEqual(3072, SFO),
    ?assertEqual(0, POT),
    ?assertEqual(24, bit_size(PLT)),
    ?assertEqual(0, POT).

consent_string_v2_performance_li_lookup_test() ->
    TCF = <<"CO4Hcm2O4Hcm2AKAJAFRAzCsAP_AAH_AAAqIGVtd_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EunLBAQdlHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1Pn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAIAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAAAAAAAAA">>,
    {ok, Consent} = consent_string:parse_b64(TCF),

    ?assertEqual(false, consent_string:purposes_li_transparency(1, Consent)),
    ?assertEqual(false, consent_string:purposes_li_transparency([1, 20], Consent)),
    ?assertEqual(false, consent_string:purposes_li_transparency([1, 24], Consent)),
    ?assertEqual(true, consent_string:purposes_li_transparency(lists:seq(2, 10), Consent)).

consent_string_range_test() ->
    TCF = <<"CO4cLaDO4cMW-AHABBENA0CsAP_AAH_AAAAAGSQKAABQAKAAyAB4AIAAVgAuADIAHAAQAAkgBSAFQALQAXgAyABoADwAIsARwBIACYAE-ALQAtgBtAD0AIQATYAnQBcgDSAHOAO6AfoB_AEIAJ0AVkAzQBnQDTgG_AUkAr4BeYDJAMkgNAACAAWAA8ACoAFwAMgAcABAACoAGgAPAAmABPAC6AG0APQAhABcgDSAHOAO4AfoBCADyALzAZIAAAAA.f_gAD_gAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent {
            vendors = #vendor_range {
                num_entries = NumEntries,
                entries = Entries
            }
    } = Actual,

    ?assertEqual(40, NumEntries),

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

consent_string_invalid_segment_test() ->
    TCF = <<"CO4kMJmO4kMKCDeACBFRAxCv_____3___wqIGPwAYAAgY-Bj8AGAAIGPgAA.f___7___4AA.gAAo">>,
    Ret = consent_string:parse_b64(TCF),
    ?assertEqual({error, invalid_consent_string}, Ret).

consent_string_invalid_segment_2_test() ->
    TCF = <<"CO4xKq5O4xKrHDeACBFRAxCv_____3___wqIGPwAYAAgY-Bj8AGAAIGPgAA.f___7___4AA.gAAo">>,
    Ret = consent_string:parse_b64(TCF),
    ?assertEqual({error, invalid_consent_string}, Ret).

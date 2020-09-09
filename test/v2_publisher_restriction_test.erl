-module(v2_publisher_restriction_test).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

publisher_restriction_test() ->
    TCF = <<"CO4tVeBO4tVeBAGABBENA0CgAP_AAH_AAAYgGVtf_X9fb2-j-9599dt0eY1fd_63t-wxjgeNs-8NyZvX-J4Xp0MyvB3wpqYKmR4EunLBAQdlHGHcTQgAwIkFqTLsYk2MizNKJ7JEikMbM0dYGG1vH8XT_ZCY7k-_f__7vn-_-___6oGUEEGCAKAQBCGAAIEihEIQAIQxIAIAACihEAgEkACRQALI4COQACABAYgIQIAQAgoAYBAAIAAElAQAgAAIBQARAIAAQADAEAACAAkFgBIgAAACIEhAARABKBAQUBAQcggQFQBBACBAMEQHQAVABWAC4AIYARIApABkADLAGoANkAcQA_ACMAFLAKuAawA-QCGwEXgJEATYAnYBQ4CkQFyAMCAYSAw8Bk4SCSAAgABcAFAAVAAyABwADwAIAARAAqAB4AD6AIYAiABMACfAFUAVgAsABcADeAHMAPQAhoBEAESAI4ASwAmgBSgDDAGQAMsAagA2QBxADvAHsAPiAfYB-gEYAI4AUsAq4BfgDCAGKANYAbQA3ABvAD0AHyAQ2AioBF4CRAExAJlATYAnYBQ4CkQFNgKwAWKAtgBcgC7wGBAMGAYSAw0Bh4DIgGSAMnDQIQAVABWAC4AIYARIApABkADLAGkANQAbIA4gB-AEYAKWAVeAtAC0gGsAN4AfIBDYCLwEiAJsATsAocBSIC5AGBAMJAYeAxgBk4YAIAbIA6gC-gGRjoJIAC4AKAAqABcADIAHAAQAAiABiADwAH0AQwBEACZAFUAVgAsABcADEAGYAN4AcwA9ACGAEQAJYATAAmoBRgFKAMgAZUA0QDSAGoANmAb4BwADiAHeAPaAfYB-AEYAI4AVcAsUBaAFpALmAXkAvwBhADFAG4gOmA6gB6AENgIiAReAkEBIgCbAE7AKHgU0BTYCrAFiwLYAtkBcAC5AF2gLvAYSAw0Bh4DEgGMAMeAZIAycZAYAAoACoAIYATAAuACOAFIAMsAagA4gB-AEYAI4AUsAq4BWwDeAJiATYApsBaIDAgGHgMiGAAQB4CoDAAFAAVABDACYAFwARwApABlgDUAHEAPwAjABHAClwFoAWkA3gCQQExAJsAU2AuQBgQDDwGREIFgACwAKAAZABEADEAIYATAAqgBcADMAG8APQAjgBSADKAGoAN8AcQA7wB-AEDAIwARwAocBaAFpAL8AYQAxQB1AD0AJBASIAmyBTQFNgLFAWjAtgC2QFwALkAXaAw8BiQDIgGTkoEwACAAFgAUAAuABkADgAMQAeABEACYAFUALgAYgAzABtAEIAIaARABEwCkAKUAZQA0gBsgDvAH4ARgAjkBaAFpAMUAbgA6gCLwEiAJsAWKAu0Bh4DIgGTlIG4ACwAKAAXAAyABwAEAAMQAeQBDAEQAJgAUgAqgBYAC4AGIAMwAcwBDACIgFGAUoAyoBogGkANkAcQA74B9gH4ARgAjgBQwCtgF5AMIAbQA3AB1AD0AIvASIAmwBOwCh4FNAU2ArYBYoC2AFwALkAXaAw0Bh4DEgGMAMiAZIAycqACAB8Aks.YAAAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent {
         publisher_restrictions = #publisher_restrictions {
             num_pub_restrictions = ActualNumPubRestrictions,
             entries = [FirstRestriction | _ ] = Restrictions
         }
    } = Actual,

    ?assertEqual(12, ActualNumPubRestrictions),

    #publisher_restrictions_entry {
       restriction_type = FirstRestrictType,
       purpose_id = FirstPurposeId,
       num_entries = FirstNumEntries,
       entries = FirstEntries
    } = FirstRestriction,
    ?assertEqual(2, FirstRestrictType),
    ?assertEqual(10, FirstPurposeId),
    ?assertEqual(2, FirstNumEntries),
    ?assertEqual([587, 62], FirstEntries),

    [LastRestriction | _] = lists:reverse(Restrictions),

    #publisher_restrictions_entry {
       restriction_type = LastRestrictType,
       purpose_id = LastPurposeId,
       num_entries = LastNumEntries,
       entries = LastEntries
    } = LastRestriction,
    ?assertEqual(1, LastRestrictType),
    ?assertEqual(4, LastPurposeId),
    ?assertEqual(29, LastNumEntries),
    ?assertEqual([807,783,777,770,740,657,647,
                  630,620,580,559,539,498,428,
                  343,331,280,252,226,217,212,
                  203,200,164,137,134,92,86,42],
                 LastEntries).

publisher_restriction_2_test() ->
    %% this seems to work just need to add more tests
    TCF = <<"CO4rUbLO4rUbLAGABCENA0CgAP_AAH_AAAwIGSFL_S9MQyFDeWp9Its0IYQXwFSRIeAjigEFoYAJSQAUNJwQhGACIAiggiAKCRYAIBIAAARAGAAQQAAAQAEBAACAIECEACAAICIACBAAQEAAAAkAgAEAEAAA4AQQAAAAgAAAEAMAQMkIDMlSYAkKEspTaRdIoQwgAgCEiQcBHFCILAgASEgAoSRgAKIAEQAABAEAUEgxAQCwAAAiAABAggAAAgAICAAEAAAAAAAAAAEQAICAAAIAAABABAAAAAAAAwAggAAgBAACIAAQAgFDQBgAYgAzACtAGUAS0Ar4BfgDOAHiAP2AkEBYgkBAAAsABwAEgALgA8AB8AEIAIgARwAkgBMgCeAKAAWwAxABzAFKAK0Ac4BLQCsgFfAL8AZwA2wB4gD1AH7ASDAsQCxQF0AMZAZIFANgACAAUABIAC4ALQAZABCACOAEkAKAAUgAtgBiAFaAQgArIBXwDMAGcANsAeIA9QB-wEggLEAYQAxkNAHABUAKQAZYA0gBxAEYATYHAKgAOAB4AEcAJIATAAngBbADmAKUAVoBLQCsgF-AM4AbYA8QCQQFigLoAYyAyQWAMADEAGYAPAArQBlAEtAK-AX4AzgB4gD9wLEAsUeAXAAUABIAFoAMgAjgBNACgAFIALYAkwBWgCyAJaAVkAr4BmADOAG2APEAeoA_YCxAGMkQBQATQBJgCtAFkAVkAr4BnADxAH7AWITAKgBCACOAE0AKAAUgA8ACTAFaALIAloBTQCsgFfAL8AZwA2wB4gD9gJBAWIAxkqATABkAD4AIQARwAmgBQADwAJMAVoAsgCWgFZAK-AZwA2wB4gD9gLEAYyAA.YAAAAAAAAAAA">>,
    {ok, Actual} = consent_string:parse_b64(TCF),

    #consent {
         publisher_restrictions = #publisher_restrictions {
             num_pub_restrictions = ActualNumPubRestrictions,
             entries = [FirstRestriction | _ ] = Restrictions
         }
    } = Actual,

    ?assertEqual(10, ActualNumPubRestrictions),

    #publisher_restrictions_entry {
       restriction_type = FirstRestrictType,
       purpose_id = FirstPurposeId,
       num_entries = FirstNumEntries,
       entries = FirstEntries
    } = FirstRestriction,
    ?assertEqual(2, FirstRestrictType),
    ?assertEqual(10, FirstPurposeId),
    ?assertEqual(19, FirstNumEntries),
    ?assertEqual([793,708,507,482,438,412,351,345,
                  301,178,173,147,120,80,77,71,66,
                  62,50], FirstEntries),

    [LastRestriction | _] = lists:reverse(Restrictions),

    #publisher_restrictions_entry {
       restriction_type = LastRestrictType,
       purpose_id = LastPurposeId,
       num_entries = LastNumEntries,
       entries = LastEntries
    } = LastRestriction,
    ?assertEqual(2, LastRestrictType),
    ?assertEqual(6, LastPurposeId),
    ?assertEqual(12, LastNumEntries),
    ?assertEqual([708,577,507,482,412,382,351,301,
                  202,173,102,98],
                 LastEntries).

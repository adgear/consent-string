-module(consent_string_cli).
-export([main/1]).
-include("consent_string.hrl").

-spec main(list()) -> no_return().
main([]) ->
    usage();
main([_Command]) ->
    usage();
main([Command | Args]) ->
    case Command of
        "parse" ->
            [Args1 | _] = Args,
            Input = list_to_binary(Args1),
            {ok, ConsentRecord} = consent_string:parse_b64(Input),
            pretty_print_consent(ConsentRecord);
        _ ->
            usage()
    end.

%% private

pretty_print_consent(#consent { version = 1 } = Consent) ->
    print_v1(Consent);
pretty_print_consent(#consent { version = 2 } = Consent) ->
    print_v2(Consent).

usage() ->
    io:format("ag-consent~n"),
    io:format("    parse <consent-string>~n"),
    io:format("~n").

print_v1(Consent) ->
    #consent {
       version = Version,
       created = Created,
       last_updated = LastUpdated,
       cmp_id = CmpId,
       cmp_version = CmpVersion,
       consent_screen = ConsentScreen,
       consent_language = ConsentLanguage,
       vendor_list_version = VendorListVersion,
       purposes_allowed = PurposesAllowed,
       max_vendor_id = MaxVendorId,
       encoding_type = EncodingType,
       vendors = Vendors
    } = Consent,

    io:format("  version             : ~p~n", [Version]),
    io:format("  created             : ~p~n", [Created]),
    io:format("  last_updated        : ~p~n", [LastUpdated]),
    io:format("  cmp_id              : ~p~n", [CmpId]),
    io:format("  cmp_version         : ~p~n", [CmpVersion]),
    io:format("  consent_screen      : ~p~n", [ConsentScreen]),
    io:format("  consent_language    : ~p~n", [ConsentLanguage]),
    io:format("  vendor_list_version : ~p~n", [VendorListVersion]),
    io:format("  purposes_allowed    : ~p~n", [PurposesAllowed]),
    io:format("  max_vendor_id       : ~p~n", [MaxVendorId]),
    io:format("  encoding_type       : ~p~n", [EncodingType]),

    [First| _] = tuple_to_list(Consent#consent.vendors),
    case First of
        vendor_bit_field ->
            io:format("  vendors: bitfield   : ~p~n",
                      [Vendors#vendor_bit_field.fields]);
        vendor_range ->
            #vendor_range{
               default_consent = DefaultConsent,
               num_entries = NumEntries,
               entries = Entries
            } = Consent#consent.vendors,
            io:format("  vendors: range: ~n"),
            io:format("    default_consent: ~p~n", [DefaultConsent]),
            io:format("    num_entries:     ~p~n", [NumEntries]),
            io:format("    entries:         ~p~n", [Entries])
    end,
    io:format("~n").

print_v2(Consent) ->
    #consent {
       version = Version,
       created = Created,
       last_updated = LastUpdated,
       cmp_id = CmpId,
       cmp_version = CmpVersion,
       consent_screen = ConsentScreen,
       consent_language = ConsentLanguage,
       vendor_list_version = VendorListVersion,
       purposes_allowed = PurposesAllowed,
       max_vendor_id = MaxVendorId,
       encoding_type = EncodingType,

       vendors = Vendors,
       vendor_legitimate_interests = VendorsLI,
       publisher_restrictions = PublisherRestrictions,
       segments = Segments
    } = Consent,

    io:format("  version             : ~p~n", [Version]),
    io:format("  created             : ~p~n", [Created]),
    io:format("  last_updated        : ~p~n", [LastUpdated]),
    io:format("  cmp_id              : ~p~n", [CmpId]),
    io:format("  cmp_version         : ~p~n", [CmpVersion]),
    io:format("  consent_screen      : ~p~n", [ConsentScreen]),
    io:format("  consent_language    : ~p~n", [ConsentLanguage]),
    io:format("  vendor_list_version : ~p~n", [VendorListVersion]),
    io:format("  purposes_allowed    : ~p~n", [PurposesAllowed]),
    io:format("  max_vendor_id       : ~p~n", [MaxVendorId]),
    io:format("  encoding_type       : ~p~n", [EncodingType]),
    io:format("  vendors             : ~p~n", [Vendors]),
    io:format("  vendor_li           : ~p~n", [VendorsLI]),
    io:format("  publisher restrict  : ~p~n", [PublisherRestrictions]),
    io:format("  segments            : ~p~n", [Segments]),
    io:format("~n").

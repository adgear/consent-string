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
            {ok, ConsentRecord} = consent_string:parse_b64(list_to_binary(Args1)),
            pretty_print_consent(ConsentRecord);
        _ -> usage()
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
    io:format("  version             : ~p~n", [Consent#consent.version]),
    io:format("  created             : ~p~n", [Consent#consent.created]),
    io:format("  last_updated        : ~p~n", [Consent#consent.last_updated]),
    io:format("  cmp_id              : ~p~n", [Consent#consent.cmp_id]),
    io:format("  cmp_version         : ~p~n", [Consent#consent.cmp_version]),
    io:format("  consent_screen      : ~p~n", [Consent#consent.consent_screen]),
    io:format("  consent_language    : ~p~n", [Consent#consent.consent_language]),
    io:format("  vendor_list_version : ~p~n", [Consent#consent.vendor_list_version]),
    io:format("  purposes_allowed    : ~p~n", [Consent#consent.purposes_allowed]),
    io:format("  max_vendor_id       : ~p~n", [Consent#consent.max_vendor_id]),
    io:format("  encoding_type       : ~p~n", [Consent#consent.encoding_type]),
    [First| _] = tuple_to_list(Consent#consent.vendors),
    case First of
        vendor_bit_field ->
            io:format("  vendors: bitfield   : ~p~n", [(Consent#consent.vendors)#vendor_bit_field.fields]);
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
    io:format("  version             : ~p~n", [Consent#consent.version]),
    io:format("  created             : ~p~n", [Consent#consent.created]),
    io:format("  last_updated        : ~p~n", [Consent#consent.last_updated]),
    io:format("  cmp_id              : ~p~n", [Consent#consent.cmp_id]),
    io:format("  cmp_version         : ~p~n", [Consent#consent.cmp_version]),
    io:format("  consent_screen      : ~p~n", [Consent#consent.consent_screen]),
    io:format("  consent_language    : ~p~n", [Consent#consent.consent_language]),
    io:format("  vendor_list_version : ~p~n", [Consent#consent.vendor_list_version]),
    io:format("  purposes_allowed    : ~p~n", [Consent#consent.purposes_allowed]),
    io:format("  max_vendor_id       : ~p~n", [Consent#consent.max_vendor_id]),
    io:format("  encoding_type       : ~p~n", [Consent#consent.encoding_type]),

    [VendorType| _] = tuple_to_list(Consent#consent.vendors),
    case VendorType of
        vendor_bit_field ->
            io:format("  vendors: bitfield   : ~p~n", [(Consent#consent.vendors)#vendor_bit_field.fields]);
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

    io:format("  vendor legitimate interests: ~p~n",
              [Consent#consent.vendor_legitimate_interests]),

    io:format("~n").

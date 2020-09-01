-module(consent_string_v2).
-include("consent_string.hrl").

-export([
    parse/1,
    parse_range_or_bitfield/1
]).

-spec parse(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

parse(<<Version:6, Created:36, LastUpdated:36, CmpId:12, CmpVersion:12,
        ConsentScreen:6, ConsentLanguage1:6, ConsentLanguage2:6,
        VendorListVersion:12,

        TcfPolicyVersion:6, IsServiceSpecific:1, UseNonStandardStacks:1,
        SpecialFeatureOptins:12,

        %% renamed from PurposesAllowed in specification
        PurposesConsent:24/bitstring,
        PurposesLITransparency:24/bitstring,
        PurposesOneTreatment:1,
        PublisherCC1:6, PublisherCC2:6,

        Blob/bitstring>>) ->
    PublisherConsentCountry = list_to_binary([65 + PublisherCC1, 65 + PublisherCC2]),
    ConsentLanguage = list_to_binary([65 + ConsentLanguage1, 65 + ConsentLanguage2]),

    case parse_vendors(Blob) of
        {error, invalid_vendors} -> {error, invalid_consent_string};
        {ok, MaxVendorId, Vendors, AfterVendorBlob} ->
            case parse_vendor_legitimate_interests(AfterVendorBlob) of
                {error, invalid_vendor_legitimate_interests} -> {error, invalid_consent_string};
                {ok, _MaxVendorLIId, LegitimateInterests, AfterPubLIBlob} ->
                    case parse_publisher_restrictions(AfterPubLIBlob) of
                        {error, invalid_publisher_restrictions} -> {error, invalid_publisher_restriction};
                        {ok, Restrictions, _Rest} ->
                            {ok, #consent {
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
                                special_feature_optins = SpecialFeatureOptins,
                                purposes_li_transparency = PurposesLITransparency,
                                purposes_one_treatment = PurposesOneTreatment,
                                publisher_cc = PublisherConsentCountry,
                                purposes_allowed = PurposesConsent,
                                max_vendor_id = MaxVendorId,
                                encoding_type = undefined, % IsRangeEncoding, % refactor messed this up will have to think about how to do this now
                                vendors = Vendors,
                                vendor_legitimate_interests = LegitimateInterests,
                                publisher_restrictions = Restrictions
                            }}
                    end
            end
    end;
parse(_) ->
    {error, invalid_consent_string}.

%% private

%% parse_vendors(<<MaxVendorId:16, IsRangeEncoding:1, Rest/bitstring>>)
-spec parse_vendors(binary()) -> {pos_integer(), #vendor_bit_field{}, bitstring()}
                               | {error, invalid_vendors}.
parse_vendors(<<MaxVendorId:16, 0:1, Bin:MaxVendorId/bitstring, Rest/bitstring>>) ->
    {ok, MaxVendorId, #vendor_bit_field { fields = Bin }, Rest};
parse_vendors(<<MaxVendorId:16, 1:1, NumEntries:12, Rest/bitstring>>) ->
    case parse_entries(Rest, NumEntries, []) of
        {ok, EntryRest, Entries} ->
            {ok, MaxVendorId,
                 #vendor_range {
                     default_consent = undefined,
                     num_entries = NumEntries,
                     entries = Entries
                 },
                 EntryRest};
        {error, invalid_entries} ->
            {error, invalid_vendors}
    end;
parse_vendors(_) ->
    {error, invalid_vendors}.

parse_publisher_restrictions(<<0:12, Rest/bitstring>>) ->
    {ok, #publisher_restrictions { num_pub_restrictions = 0 }, Rest};
parse_publisher_restrictions(<<NumPubRestrictions:12, RestrictionEntriesBlob/bitstring>>) ->
    {Rest, Entries} = parse_publisher_restriction_bundle(NumPubRestrictions, RestrictionEntriesBlob, []),
    {ok, #publisher_restrictions { num_pub_restrictions = NumPubRestrictions,
                                   entries = Entries }, Rest}.

%% need a better function name :)
parse_publisher_restriction_bundle(0, Rest, Acc) ->
    {Rest, Acc};
parse_publisher_restriction_bundle(N, Blob, Acc) ->
    {Rest, Entry} = parse_publisher_restriction_single_entry(Blob),
    parse_publisher_restriction_bundle(N - 1, Rest, [Entry | Acc]).

parse_publisher_restriction_single_entry(<<PurposeId:6, RestrictionType:2, NumEntries:12, Rest/bitstring>>) ->
    case parse_entries(Rest, NumEntries, []) of
        {ok, EntryRest, Entries} ->
            {EntryRest,
             #publisher_restrictions_entry {
                  purpose_id = PurposeId,
                  restriction_type = RestrictionType,
                  num_entries = NumEntries,
                  entries = Entries
             }
            };
        {error, invalid_entries} ->
            {error, invalid_publisher_restriction_invalid_entry}
    end.

parse_range_or_bitfield(<<MaxVendorId:16, 0:1, Bin:MaxVendorId/bitstring, Rest/bitstring>>) ->
    {ok, MaxVendorId, #entry_bitfield { fields = Bin }, Rest};
parse_range_or_bitfield(<<MaxVendorId:16, 1:1, NumEntries:12, Rest/bitstring>>) ->
    case parse_entries(Rest, NumEntries, []) of
        {ok, EntryRest, Entries} ->
            {ok,
             MaxVendorId,
             #entry_range {
                num_entries = NumEntries,
                entries = Entries
             },
             EntryRest};
        {error, invalid_entries} ->
            {error, invalid_entries}
    end.

%% TODO replace with 'parse_range_or_bitfield'
parse_vendor_legitimate_interests(<<MaxVendorId:16, 0:1, Bin:MaxVendorId/bitstring, Rest/bitstring>>) ->
    {ok, MaxVendorId, #vendor_legitimate_interests_entry { fields = Bin }, Rest};
parse_vendor_legitimate_interests(<<MaxVendorId:16, 1:1, NumEntries:12, Rest/bitstring>>) ->
    case parse_entries(Rest, NumEntries, []) of
        {ok, EntryRest, Entries} ->
            {ok,
             MaxVendorId,
             #vendor_legitimate_interests_range {
                 num_entries = NumEntries,
                 entries = Entries
             },
             EntryRest};
        {error, invalid_entries} ->
            {error, invalid_vendor_legitimate_interests}
    end;
parse_vendor_legitimate_interests(_) ->
    {error, invalid_vendor_legitimate_interests}.

parse_entries(<<>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:1>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:2>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:3>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:4>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:5>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:6>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(<<0:7>>, 0, Acc) ->
    {ok, <<>>, Acc};
parse_entries(Rest, 0, Acc) ->
    {ok, Rest, Acc};
parse_entries(<<0:1, Single:16, Rest/bitstring>>, N, Acc) ->
    parse_entries(Rest, N - 1, [Single | Acc]);
parse_entries(<<1:1, Start:16, End:16, Rest/bitstring>>, N, Acc) ->
    parse_entries(Rest, N - 1, [{Start, End} | Acc]);
parse_entries(_, _, _) ->
    {error, invalid_entries}.

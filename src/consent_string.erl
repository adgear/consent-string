% https://github.com/InteractiveAdvertisingBureau/Consent-String-SDK-JS/blob/master/src/utils/definitions.js

-module(consent_string).
-include("consent_string.hrl").

-export([
    parse/1,
    parse_b64/1,
    purpose/2,
    vendor/2
]).

-spec parse(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

% https://github.com/InteractiveAdvertisingBureau/GDPR-Transparency-and-Consent-Framework/blob/master/Consent%20string%20and%20vendor%20list%20formats%20v1.1%20Final.md#vendor-consent-string-format-
parse(<<1:6, Created:36, LastUpdated:36, CmpId:12, CmpVersion:12,
        ConsentScreen:6, ConsentLanguage1:6, ConsentLanguage2:6,
        VendorListVersion:12, PurposesAllowed:24/bitstring, MaxVendorId:16,
        EncodingType:1, Rest/bitstring>>) ->

    case parse_vendors(EncodingType, Rest) of
        {error, invalid_vendors} ->
            {error, invalid_consent_string};
        Vendors ->
            ConsentLanguage = list_to_binary([65 + ConsentLanguage1,
                65 + ConsentLanguage2]),

            {ok, #consent {
                version = 1,
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
            }}
    end;
% https://github.com/InteractiveAdvertisingBureau/GDPR-Transparency-and-Consent-Framework/blob/master/TCFv2/IAB%20Tech%20Lab%20-%20Consent%20string%20and%20vendor%20list%20formats%20v2.md#the-core-string
parse(<<2:6, Created:36, LastUpdated:36, CmpId:12, CmpVersion:12,
        ConsentScreen:6, ConsentLanguage1:6, ConsentLanguage2:6,
        VendorListVersion:12, _TcfPolicyVersion:6, _IsServiceSpecific:1,
        _UseNonStandardStacks:1, _SpecialFeatureOptIns:12, PurposesAllowed:24/bitstring,
        _PurposesLITransparency:24, _PurposeOneTreatment:1, _PublisherCC:12,
        MaxVendorId:16, EncodingType:1, Rest/bitstring>>) ->

    case parse_vendors(EncodingType, Rest) of
        {error, invalid_vendors} ->
            {error, invalid_consent_string};
        Vendors ->
            ConsentLanguage = list_to_binary([65 + ConsentLanguage1,
                65 + ConsentLanguage2]),

            {ok, #consent {
                version = 2,
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
            }}
    end;
parse(_) ->
    {error, invalid_consent_string}.

-spec parse_b64(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

parse_b64(Bin) ->
    io:format("~p~n", [web_base64_decode(Bin)]),
    parse(web_base64_decode(Bin)).

-spec purpose(pos_integer() | [pos_integer()], consent()) ->
    boolean().

purpose(PurposeId, #consent {} = Consent) when is_integer(PurposeId)->
    purpose([PurposeId], Consent);
purpose([], _Consent) ->
    true;
purpose([PurposeId | T], #consent {
        purposes_allowed = PurposesAllowed
    } = Consent) ->

    case check_bit(PurposeId, PurposesAllowed) of
        true ->
            purpose(T, Consent);
        false ->
            false
    end.

-spec vendor(pos_integer(), consent()) ->
    boolean().

vendor(VendorId, #consent {
        max_vendor_id = MaxVendorId,
        vendors = #vendor_bit_field {
            fields = Vendors
        }
    }) when VendorId =< MaxVendorId ->

    check_bit(VendorId, Vendors);
vendor(VendorId, #consent {
        max_vendor_id = MaxVendorId,
        vendors = #vendor_range {
            default_consent = 0,
            entries = Entries
        }
    }) when VendorId =< MaxVendorId ->

    search_entries(VendorId, Entries);
vendor(VendorId, #consent {
        max_vendor_id = MaxVendorId,
        vendors = #vendor_range {
            default_consent = 1,
            entries = Entries
        }
    }) when VendorId =< MaxVendorId ->

    negate(search_entries(VendorId, Entries));
vendor(_, _) ->
    false.

%% private
boolean(0) -> false;
boolean(1) -> true.

check_bit(Index, BitString) ->
    Index2 = Index - 1,
    case BitString of
        <<_:Index2/bitstring, Bit:1, _/bitstring>> ->
            boolean(Bit);
        _ ->
            false
    end.

negate(false) -> true;
negate(true) -> false.

padding(0) -> <<>>;
padding(1) -> <<"===">>;
padding(2) -> <<"==">>;
padding(3) -> <<"=">>.

parse_vendors(0, Bin) ->
    #vendor_bit_field {
        fields = Bin
    };
parse_vendors(1, <<DefaultConsent:1, NumEntries:12, Rest/bitstring>>) ->
    case parse_entries(Rest, NumEntries, []) of
        {ok, Entries} ->
            #vendor_range {
                default_consent = DefaultConsent,
                num_entries = NumEntries,
                entries = Entries
            };
        {error, invalid_entries} ->
            {error, invalid_vendors}
    end;
parse_vendors(_, _) ->
    {error, invalid_vendors}.

parse_entries(<<>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:1>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:2>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:3>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:4>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:5>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:6>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:7>>, 0, Acc) ->
    {ok, Acc};
parse_entries(<<0:1, Single:16, Rest/bitstring>>, N, Acc) ->
    parse_entries(Rest, N - 1, [Single | Acc]);
parse_entries(<<1:1, Start:16, End:16, Rest/bitstring>>, N, Acc) ->
    parse_entries(Rest, N - 1, [{Start, End} | Acc]);
parse_entries(_, _, _) ->
    {error, invalid_entries}.

search_entries(_Id, []) ->
    false;
search_entries(Id, [{Start, End} | _]) when Id > Start, Id < End->
    true;
search_entries(Id, [Value | _]) when Id =:= Value ->
    true;
search_entries(Id, [_ | T]) ->
    search_entries(Id, T).

web_base64_decode(Bin) ->
    Bin2 = binary:replace(Bin, <<"-">>, <<"+">>, [global]),
    Bin3 = binary:replace(Bin2, <<"_">>, <<"/">>, [global]),
    Padding = padding(size(Bin) rem 4),
    base64:decode(<<Bin3/binary, Padding/binary>>).

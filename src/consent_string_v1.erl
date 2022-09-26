-module(consent_string_v1).
-include("consent_string.hrl").

-export([
    parse/1,
    purpose/2,
    vendor/2,
    get_vendors/1,
    get_purposes_allowed/1
]).

-spec parse(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

parse(<<Version:6, Created:36, LastUpdated:36, CmpId:12, CmpVersion:12,
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
                version = Version,
                created = Created,
                last_updated = LastUpdated,
                cmp_id = CmpId,
                cmp_version = CmpVersion,
                consent_screen = ConsentScreen,
                consent_language = ConsentLanguage,
                vendor_list_version = VendorListVersion,
                tcf_policy_version = undefined,
                is_service_specific = undefined,
                use_non_standard_stacks = undefined,
                special_feature_optins = undefined,
                purposes_allowed = PurposesAllowed,
                purposes_li_transparency = undefined,
                purposes_one_treatment = undefined,
                publisher_cc = undefined,
                max_vendor_id = MaxVendorId,
                encoding_type = EncodingType,
                vendors = Vendors,
                vendor_legitimate_interests = undefined,
                publisher_restrictions = undefined,
                disclosed_vendors = undefined,
                allowed_vendors = undefined,
                publisher_tc = undefined
            }}
    end;
parse(_) ->
    {error, invalid_consent_string}.

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

-spec get_vendors(consent()) -> [pos_integer()] | {1, [pos_integer()]} | undefined.

get_vendors(#consent {
        vendors = #vendor_bit_field {
            fields = Vendors
        }
    }) ->
    get_bits(Vendors, 1, []);
get_vendors(#consent {
        vendors = #vendor_range {
            entries = []
        }
    }) ->
    undefined;
get_vendors(#consent {
        vendors = #vendor_range {
            default_consent = 0,
            entries = Entries
        }
    }) ->
    expand_range(Entries, []);
get_vendors(#consent {
        vendors = #vendor_range {
            default_consent = 1,
            entries = Entries
        }
    }) ->
    {1, expand_range(Entries, [])};
get_vendors(_) ->
    undefined.

-spec get_purposes_allowed(consent()) -> [pos_integer()].

get_purposes_allowed(#consent{purposes_allowed = PurposesAllowed}) ->
    get_bits(PurposesAllowed, 1, []).

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

get_bits(<<>>, _Cnt, SetBits) ->
    SetBits;
get_bits(<<1:1, BitString/bitstring>>, Cnt, SetBits) ->
    get_bits(BitString, Cnt + 1, [Cnt | SetBits]);
get_bits(<<0:1, BitString/bitstring>>, Cnt, SetBits) ->
    get_bits(BitString, Cnt + 1, SetBits).

expand_range([], Result) ->
    Result;
expand_range([{Start, End} | T], Result) ->
    expand_range(T, do_expand_range(Start, End, Result));
expand_range([Value | T], Result) ->
    expand_range(T, [Value | Result]).

do_expand_range(End, End, Result) ->
    [End | Result];
do_expand_range(Start, End, Result) ->
    do_expand_range(Start + 1, End, [Start | Result]).
-module(consent_string_v2).
-include("consent_string.hrl").

-export([parse/1]).

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
                purposes_allowed = PurposesAllowed,
                max_vendor_id = MaxVendorId,
                encoding_type = EncodingType,
                vendors = Vendors
            }}
    end;
parse(_) ->
    {error, invalid_consent_string}.

%% private

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

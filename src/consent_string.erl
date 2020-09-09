-module(consent_string).
-include("consent_string.hrl").

-export([
    parse/1,
    parse_b64/1,
    purpose/2,
    purposes_li_transparency/2,
    vendor/2,
    vendor_legitimate_interests/2,

    main/1
]).

-spec main(list()) -> no_return().
main(Args) ->
    consent_string_cli:main(Args).

-spec parse(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

%% fisrt 6 bits is the version, which decides which parser to use.
parse(<<1:6, _/bitstring>> = Bin) ->
    consent_string_v1:parse(Bin);
parse(<<2:6, _/bitstring>> = Bin) ->
    consent_string_v2:parse(Bin);
parse(_) ->
    {error, invalid_consent_string}.

%% tcf segments are a v2 thing. We omitt 0 because that's the core
%%  string
parse_segment(<<1:3, Blob/bitstring>>) ->
    case consent_string_v2:parse_range_or_bitfield(Blob) of
        {ok, MaxVendorId, Segment, _Rest} ->
            #consent_segment {
                 type = 1,
                 segment = #consent_segment_entry_disclosed_vendors {
                     max_vendor_id = MaxVendorId,
                     entries = Segment
                 }
            };
         {error, Reason} ->
            {error, Reason}
    end;
parse_segment(<<2:3, Blob/bitstring>>) ->
    case consent_string_v2:parse_range_or_bitfield(Blob) of
        {ok, MaxVendorId, Segment, _Rest} ->
            #consent_segment {
                 type = 2,
                 segment = #consent_segment_entry_allowed_vendors {
                     max_vendor_id = MaxVendorId,
                     entries = Segment
                 }
            };
         {error, Reason} ->
            {error, Reason}
    end;
parse_segment(<<3:3,
                PubPurposesConsent:24/bitstring,
                PubPurposesLITransparency:24/bitstring,
                NumCustomPurposes:6,
                CustomPurposesConsent:NumCustomPurposes/bitstring,
                CustomPurposesLI:NumCustomPurposes/bitstring,
                _Rest/bitstring>>) ->
    #consent_segment {
       type = 3,
       segment = #consent_segment_entry_publisher_purposes {
           pub_purposes_consent = PubPurposesConsent,
           pub_purposes_li_transparency = PubPurposesLITransparency,
           num_custom_purposes = NumCustomPurposes,
           custom_purposes_consent = CustomPurposesConsent,
           custom_purposes_li = CustomPurposesLI
       }
      };
parse_segment(_) ->
    {error, bad_segment}.

-spec parse_b64(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

parse_b64(Bin) ->
    Parts = binary:split(Bin, <<".">>, [global, trim]),
    [CoreString | Segments] = lists:map(fun(X) -> web_base64_decode(X) end,
                                        Parts),

    ParsedSegments = lists:map(fun(X) -> parse_segment(X) end,
                               Segments),

    BadSegment = lists:any(fun(X) -> X =:= {error, bad_segment} end,
                           ParsedSegments),

    case BadSegment of
        true ->
            {error, invalid_consent_string};
        _ ->
            DisclosedVendorSegment = find_segment(ParsedSegments, 1),
            AllowedVendorSegment = find_segment(ParsedSegments, 2),
            PublisherTCSegment = find_segment(ParsedSegments, 3),

            case parse(CoreString) of
                {ok, Consent} ->
                    NewConsent = Consent#consent {
                        disclosed_vendors = DisclosedVendorSegment,
                        allowed_vendors = AllowedVendorSegment,
                        publisher_tc = PublisherTCSegment
                    },

                    {ok, NewConsent};
                {error, _} ->
                    {error, invalid_consent_string}
            end
    end.

-spec purposes_li_transparency(pos_integer() | [pos_integer()], consent()) ->
          undefined | boolean().

purposes_li_transparency(_, #consent { version = 1 }) ->
    undefined;
purposes_li_transparency(Purposes, #consent { version = 2 } = Consent) ->
    consent_string_v2:purposes_li_transparency(Purposes, Consent).

-spec purpose(pos_integer() | [pos_integer()], consent()) ->
    boolean().

purpose(PurposeId, #consent { version = 1 } = Consent) ->
    consent_string_v1:purpose(PurposeId, Consent);
purpose(PurposeId, #consent { version = 2 } = Consent) ->
    consent_string_v2:purpose(PurposeId, Consent).

-spec vendor(pos_integer(), consent()) ->
    boolean().

vendor(VendorId, #consent { version = 1 } = Consent) ->
    consent_string_v1:vendor(VendorId, Consent);
vendor(VendorId, #consent { version = 2 } = Consent) ->
    consent_string_v2:vendor(VendorId, Consent);
vendor(_, _) ->
    false.

-spec vendor_legitimate_interests([pos_integer()], consent()) ->
          undefined | boolean().

vendor_legitimate_interests(_, #consent { version = 1 }) ->
    undefined;
vendor_legitimate_interests(Ids, #consent { version = 2 } = Consent) ->
    consent_string_v2:vendor_legitimate_interests(Ids, Consent).

%% private
find_segment([], _) ->
    undefined;
find_segment([#consent_segment { type = Type } = Segment | _], Type) ->
    Segment;
find_segment([_ | Rest], Type)->
    find_segment(Rest, Type).

padding(0) -> <<>>;
padding(1) -> <<"===">>;
padding(2) -> <<"==">>;
padding(3) -> <<"=">>.

web_base64_decode(Bin) ->
    Bin2 = binary:replace(Bin, <<"-">>, <<"+">>, [global]),
    Bin3 = binary:replace(Bin2, <<"_">>, <<"/">>, [global]),
    Padding = padding(size(Bin) rem 4),
    base64:decode(<<Bin3/binary, Padding/binary>>).

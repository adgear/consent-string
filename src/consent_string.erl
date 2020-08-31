-module(consent_string).
-include("consent_string.hrl").

-export([
    parse/1,
    parse_b64/1,
    purpose/2,
    vendor/2,

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

-spec parse_b64(binary()) ->
    {ok, consent()} | {error, invalid_consent_string}.

parse_b64(Bin) ->
    Parts = binary:split(Bin, <<".">>, [global]),
    [CoreString | Segments] = Parts,

    %% https://github.com/InteractiveAdvertisingBureau/GDPR-Transparency-and-Consent-Framework/blob/master/TCFv2/IAB%20Tech%20Lab%20-%20Consent%20string%20and%20vendor%20list%20formats%20v2.md#tc-string-format
    %% according to TCFv2 standard, after the core string, any of the
    %% the other strings can happen at least once:
    %%   [Core String].[Disclosed Vendors].[AllowedVendors].[Publisher TC]
    parse(web_base64_decode(CoreString)).

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

%% private
padding(0) -> <<>>;
padding(1) -> <<"===">>;
padding(2) -> <<"==">>;
padding(3) -> <<"=">>.

web_base64_decode(Bin) ->
    Bin2 = binary:replace(Bin, <<"-">>, <<"+">>, [global]),
    Bin3 = binary:replace(Bin2, <<"_">>, <<"/">>, [global]),
    Padding = padding(size(Bin) rem 4),
    base64:decode(<<Bin3/binary, Padding/binary>>).

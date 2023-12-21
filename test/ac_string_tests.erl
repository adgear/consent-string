-module(ac_string_tests).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(_assertMatchAtp(A, B),
        ?_assertMatch({ok, (A), _}, parse_atp_ids((B)))).
-define(_assertMatchVersion(A, B),
       ?_assertMatch({ok, (A), _}, parse_version((B)))).
-define(_assertErrorVersion(B),
        ?_assertMatch({error, invalid_ac_string}, parse_version((B)))).
-define(_assertErrorAtp(B),
        ?_assertMatch({error, invalid_ac_string}, parse_atp_ids((B)))).
-define(_assertErrorParse(A),
        ?_assertMatch({error, invalid_ac_string}, parse((A)))).
-define(_assertMatchParse(A, B),
        ?_assertMatch({ok, (A)}, parse((B)))).

parse_atps_only_test_() ->
    [
        % Make sure the order is preserved after parsing
        ?_assertMatchAtp([1, 10, 100, 20], atps([1, 10, 100, 20])),
        ?_assertMatchAtp([1], atps([1])),
        % Empty list of version should NOT cause an error
        ?_assertMatchAtp([], atps([]))
    ].

parse_test_() ->
    [
        % Version 1 tests
        ?_assertMatchParse(#addtl_consent{version = 1, atp_ids = []}, <<"1~">>),
        ?_assertMatchParse(#addtl_consent{version = 1, atp_ids = [1]}, <<"1~1">>),
        ?_assertMatchParse(#addtl_consent{version = 1, atp_ids = [1, 3, 2]}, <<"1~1.3.2">>),

        % Version 2 tests
        ?_assertMatchParse(#addtl_consent{version = 2, atp_ids = [], disclosed_atp_ids = [1, 3, 2]}, <<"2~~dv.1.3.2">>),
        ?_assertMatchParse(#addtl_consent{version = 2, atp_ids = [], disclosed_atp_ids = []}, <<"2~~dv.">>),

        % Test errors
        ?_assertErrorParse(<<"1~~2">>),
        ?_assertErrorParse(<<"1~~">>),
        ?_assertErrorParse(<<"1~a">>),
        ?_assertErrorParse(<<"1a1~">>),
        % Alphanumeric ATP ids are not allowed
        ?_assertErrorParse(<<"1~1.1.a">>),
        % Trailing ATP separator is not allowed
        ?_assertErrorParse(<<"1~1.">>),
        % Invalid separator
        ?_assertErrorParse(<<"1~1,2,3">>),
        % Version 1 doesn't support disclosed list of atps
        ?_assertErrorParse(<<"1~1~dv.">>),
        % Invalid version 2 separator
        ?_assertErrorParse(<<"2~~a">>),
        % Missing dot following version 2 separator
        ?_assertErrorParse(<<"2~~dv">>),
        % Missing dot following version 2 separator
        ?_assertErrorParse(<<"2~~dv1.1.1">>)
    ].

%% Helpers
parse(Bin) ->
    additional_consent_string:parse(Bin).

% parse_version(Bin) ->
%     additional_consent_string:parse_version(Bin).

parse_atp_ids(Bin) ->
    additional_consent_string:parse_atp_ids(Bin).

atps([]) ->
    <<"">>;
atps(List) ->
    list_to_binary(
        lists:join(".", [integer_to_list(X) || X <- List])
    ).

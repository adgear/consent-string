-module(ac_string_tests).
-include("consent_string.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(_assertMatchAtp(A, B),
        ?_assertMatch({ok, (A)}, parse_atp_ids((B)))).
-define(_assertMatchVersion(A, B),
       ?_assertMatch({ok, (A), _}, parse_version((B)))).
-define(_assertErrorVersion(B),
        ?_assertMatch({error, invalid_ac_string}, parse_version((B)))).
-define(_assertErrorAtp(B),
        ?_assertMatch({error, invalid_ac_string}, parse_atp_ids((B)))).

parse_version_only_test_() ->
    [
        ?_assertMatchVersion(20, <<"20~">>),
        ?_assertMatchVersion(0, <<"0~">>),

        % Test error clauses
        % Make sure that random separators are not recognized
        ?_assertErrorVersion(<<"1=">>),
        % Alphanumeric character in version
        ?_assertErrorVersion(<<"1a~">>),
        % Missing separator at the end
        ?_assertErrorVersion(<<"1">>),
        % Separator at the start
        ?_assertErrorVersion(<<"~1">>),
        % Separator at both eds
        ?_assertErrorVersion(<<"~1~">>)
    ].

parse_atps_only_test_() ->
    [
        % Make sure the order is preserved after parsing
        ?_assertMatchAtp([1, 10, 100, 20], atps([1, 10, 100, 20])),
        ?_assertMatchAtp([1], atps([1])),
        % Empty list of version should NOT cause an error
        ?_assertMatchAtp([], atps([])),

        % Test error clauses
        % Alphanumeric ids are not allowed
        ?_assertErrorAtp(<<"1.1.a">>),
        % Trailing separators are not allowed
        ?_assertErrorAtp(<<"1.">>),
        % Invalid separator
        ?_assertErrorAtp(<<"1,2,3">>)
    ].

parse_test_() ->
    [
        ?_assertMatch({ok, #addtl_consent{version = 1, atp_ids = []}}, parse(<<"1~">>)),
        ?_assertMatch({ok, #addtl_consent{version = 1, atp_ids = [1]}}, parse(<<"1~1">>)),
        ?_assertMatch({ok, #addtl_consent{version = 1, atp_ids = [1, 3, 2]}}, parse(<<"1~1.3.2">>)),

        % Test errors
        ?_assertMatch({error, invalid_ac_string}, parse(<<"1~a">>)),
        ?_assertMatch({error, invalid_ac_string}, parse(<<"1a1~">>))
    ].

%% Helpers
parse(Bin) ->
    additional_consent_string:parse(Bin).

parse_version(Bin) ->
    additional_consent_string:parse_version(Bin).

parse_atp_ids(Bin) ->
    additional_consent_string:parse_atp_ids(Bin).

atps([]) ->
    <<"">>;
atps(List) ->
    list_to_binary(
        lists:join(".", [integer_to_list(X) || X <- List])
    ).

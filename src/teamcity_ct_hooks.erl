-module(teamcity_ct_hooks).

%% teamcity_ct_hooks: teamcity_ct_hooks library's entry point.

-export([
    init/2,

    pre_init_per_suite/3,
    post_end_per_suite/4,

    pre_init_per_group/3,
    post_end_per_group/4,

    pre_init_per_testcase/3,
    post_end_per_testcase/4
]).

-record(state, {time_start, capture_standard_output}).

%% API

init(Id, Opts) ->
    CaptureStandardOutput = proplists:get_value(capture_standard_output, Opts, false),
    {ok, #state{capture_standard_output=CaptureStandardOutput}}.

%% suite

%% @doc Called before init_per_suite is called.
pre_init_per_suite(SuiteName, Config, CTHState) ->
    tc_testSuiteStarted(SuiteName),
    {Config, CTHState}.

%% @doc Called after end_per_suite.
post_end_per_suite(SuiteName, Config, Return, CTHState) ->
    tc_testSuiteFinished(SuiteName),
    {Return, CTHState}.

%% group

%% @doc Called before each init_per_group.
pre_init_per_group(Group, Config, CTHState) ->
    tc_blockOpened(Group),
    {Config, CTHState}.

%% @doc Called after each end_per_group.
post_end_per_group(Group, Config, Return, CTHState) ->
    tc_blockClosed(Group),
    {Return, CTHState}.

%% testcase

%% @doc Called before each test case.
pre_init_per_testcase(TestcaseName, Config, CTHState) ->
    tc_testStarted(TestcaseName, CTHState#state.capture_standard_output),
    {Config, CTHState#state{time_start = os:timestamp()}}.

%% @doc Called after each test case.
post_end_per_testcase(TestcaseName, Config, Return, CTHState) ->
    case Return of
        {error, Details} ->
            tc_testFailed(TestcaseName, Details);
        ok ->
            % CTHState is common for testcases running in parallel
            case CTHState#state.time_start of
                undefined  -> tc_testFinished(TestcaseName);
                TimeStart ->
                    Duration = timer:now_diff(os:timestamp(), TimeStart),
                    tc_testFinished(TestcaseName, Duration/1.0e3)
            end
    end,
    % FIXME: for tests running in parallel is needed to distinguish which
    % testcase is ended
    {Return, CTHState#state{time_start = undefined}}.

%% Internals

tc_blockOpened(BlockName) ->
    ct:print("##teamcity[blockOpened name='~s']", [BlockName]).

tc_blockClosed(BlockName) ->
    ct:print("##teamcity[blockClosed name='~s']", [BlockName]).

tc_testSuiteStarted(SuiteName) ->
    ct:print("##teamcity[testSuiteStarted name='~s']", [SuiteName]).

tc_testSuiteFinished(SuiteName) ->
    ct:print("##teamcity[testSuiteFinished name='~s']", [SuiteName]).

tc_testStarted(TestcaseName, CaptureStandardOutput) ->
    ct:print(
        "##teamcity[testStarted name='~s' captureStandardOutput='~s']",
        [TestcaseName, CaptureStandardOutput]).

tc_testFailed(TestcaseName, Details) ->
    FormatedDetails = lists:flatten(io_lib:format("~p", [Details])),

    EscapeDetails1 = re:replace(FormatedDetails, "\\|", "\\|\\|", [global, {return, list}]),
    EscapeDetails2 = re:replace(EscapeDetails1, "'", "\\|'", [global, {return, list}]),
    EscapeDetails3 = re:replace(EscapeDetails2, "\\[", "\\|\\[", [global, {return, list}]),
    EscapeDetails4 = re:replace(EscapeDetails3, "\\]", "\\|\\]", [global, {return, list}]),
    EscapeDetails5 = re:replace(EscapeDetails4, "\r", "\\|r", [global, {return, list}]),
    EscapeDetails6 = re:replace(EscapeDetails5, "\n", "\\|n", [global, {return, list}]),
    ct:print(
        "##teamcity[testFailed name='~s' details='~s']",
        [TestcaseName, EscapeDetails6]).

tc_testFinished(TestcaseName) ->
    ct:print(
        "##teamcity[testFinished name='~s']", [TestcaseName]).

tc_testFinished(TestcaseName, Duration) ->
    ct:print(
        "##teamcity[testFinished name='~s' duration='~w']",
        [TestcaseName, Duration]).

tc_buildStatisticValue(Key, Value) ->
    ct:print(
        "##teamcity[buildStatisticValue key='~s' value='~s']",
        [Key, Value]).

%% End of Module.

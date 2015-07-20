-module(teamcity_ct_hooks).

%% teamcity_ct_hooks: teamcity_ct_hooks library's entry point.

-export([
    init/2,

    pre_init_per_suite/3,
    post_end_per_suite/4,

    pre_init_per_testcase/3,
    post_end_per_testcase/4
]).

-record(state, {time_start}).

%% API

init(Id, Opts) ->
    {ok, #state{}}.

%% @doc Called before init_per_suite is called. 
pre_init_per_suite(SuiteName, Config, CTHState) ->
    tc_testSuiteStarted(SuiteName),
    {Config, CTHState}.

%% @doc Called after end_per_suite. 
post_end_per_suite(SuiteName, Config, Return, CTHState) ->
    tc_testSuiteFinished(SuiteName),
    {Return, CTHState}.

%% @doc Called before each test case.
pre_init_per_testcase(TestcaseName, Config, CTHState) ->
    tc_testStarted(TestcaseName),
    {Config, CTHState#state{time_start = os:timestamp()}}.

%% @doc Called after each test case.
post_end_per_testcase(TestcaseName, Config, Return, CTHState) ->
    case Return of
        {error, Details} ->
            tc_testFailed(TestcaseName, Details);
        ok ->
            Duration = timer:now_diff(os:timestamp(), CTHState#state.time_start),
            tc_testFinished(TestcaseName, Duration/1.0e3)
    end,
    {Return, CTHState#state{time_start = undefined}}.


%% Internals

tc_testSuiteStarted(SuiteName) ->
    ct:print("##teamcity[testSuiteStarted name='~s']", [SuiteName]).

tc_testSuiteFinished(SuiteName) ->
    ct:print("##teamcity[testSuiteFinished name='~s']", [SuiteName]).

tc_testStarted(TestcaseName) ->
    ct:print("##teamcity[testStarted name='~s']", [TestcaseName]).

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

tc_testFinished(TestcaseName, Duration) ->
    ct:print(
        "##teamcity[testFinished name='~s' duration='~w']",
        [TestcaseName, Duration]).

tc_buildStatisticValue(Key, Value) ->
    ct:print(
        "##teamcity[buildStatisticValue key='<valueTypeKey>' value='<value>']",
        [Key, Value]).

%% End of Module.

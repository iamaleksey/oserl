%%% Copyright (C) 2009 Enrique Marcote, Miguel Rodriguez
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% o Redistributions of source code must retain the above copyright notice,
%%%   this list of conditions and the following disclaimer.
%%%
%%% o Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% o Neither the name of ERLANG TRAINING AND CONSULTING nor the names of its
%%%   contributors may be used to endorse or promote products derived from this
%%%   software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
-module(gen_esme_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").
-include_lib("oserl/include/oserl.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(LOCALHOST, {127, 0, 0, 1}).
-define(HIGH_PRIORITY, 0).
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(ESME_TIMERS, ?TIMERS(15000, 15000, 30000, 20000)).
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [transmitter, queue].


sequences() ->
    [].


suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% Init Suite Exports
%%%-----------------------------------------------------------------------------
%% @spec init_per_suite(Conf) -> Conf
%%     Conf = [tuple()]
%%
%% @doc Initiation before the whole suite.
%%
%% <b>Note:</b> This function is free to add any key/value pairs to the
%% <u>Conf</u> variable, but should NOT alter/remove any existing entries.
%% @end
init_per_suite(Conf) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    application:start(common_lib),
    dbg:tracer(),
    dbg:p(all, [c, sos, sol]),
    MaxTime = ct:get_config(max_time, ?MAX_TIME),
    [{max_time, MaxTime} | Conf].

%%%-----------------------------------------------------------------------------
%%% End Suite Exports
%%%-----------------------------------------------------------------------------
%% @spec end_per_suite(Conf) -> Result
%%    Conf = [tuple()]
%%    Result = ok | {save_config, Conf1}
%%
%% @doc Cleanup after the suite.
%% @end
end_per_suite(_Conf) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Init Case Exports
%%%-----------------------------------------------------------------------------
%% @spec init_per_testcase(Case, Conf) -> Result
%%    Case = atom()
%%    Conf = [tuple()]
%%    Result = NewConf |
%%             {skip, Reason} |
%%             {skip_and_save, Reason, NewConf}
%%    NewConf = [tuple()]
%%    Reason = term()
%%
%% @doc Initialization before each test case.
%% @end
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    {ok, _Mc} = test_mc:start_link(false),
    file:delete(ct:get_config(file)),
    init_stubs(Case),
    init_traces(Case),
    Conf.

init_stubs(Case) ->
    NegCases = ct:get_config(neg_cases, []),
    Stubs = proplists:get_value(Case, NegCases, []),
    lists:foreach(fun(Stub) -> load_stub(Stub, true) end, Stubs).

init_traces(Case) ->
    TpCases = ct:get_config(tp_cases, []),
    Tps = proplists:get_value(Case, TpCases, []),
    lists:foreach(fun(Tp) -> add_trace(tp, Tp) end, Tps),
    TplCases = ct:get_config(tpl_cases, []),
    Tpls = proplists:get_value(Case, TplCases, []),
    lists:foreach(fun(Tpl) -> add_trace(tp, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% End Case Exports
%%%-----------------------------------------------------------------------------
%% @spec end_per_testcase(Case, Conf) -> Result
%%    Case = atom()
%%    Conf = [tuple()]
%%    Result = NewConf |
%%             {skip, Reason} |
%%             {skip_and_save, Reason, NewConf}
%%    NewConf = [tuple()]
%%    Reason = term()
%%
%% @doc Cleanup after each test case.
%% @end
end_per_testcase(Case, Conf) ->
    end_traces(Case),
    end_stubs(Case),
    test_mc:stop(),
    ct:print("Test case ~p completed", [Case]),
    Conf.

end_stubs(Case) ->
    NegCases = ct:get_config(neg_cases, []),
    Stubs = proplists:get_value(Case, NegCases, []),
    lists:foreach(fun purge_stub/1, Stubs).

end_traces(Case) ->
    TpCases = ct:get_config(tp_cases, []),
    Tps = proplists:get_value(Case, TpCases, []),
    lists:foreach(fun(Tp) -> del_trace(ctp, Tp) end, Tps),
    TplCases = ct:get_config(tpl_cases, []),
    Tpls = proplists:get_value(Case, TplCases, []),
    lists:foreach(fun(Tpl) -> del_trace(ctpl, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% Test Cases
%%%-----------------------------------------------------------------------------
transmitter() ->
    [{userdata, [{doc, "Tests the ESME as transmitter."}]}].

transmitter(_Conf) ->
    {ok, _Esme} = test_esme:start_link(tx, false),
    test_esme:reset(),
    ok = test_esme:add_log_handler(smpp_disk_log_hlr),
    wait_bound(),
    BcastArea = smpp_base:broadcast_area(0, "area_name"),
    BcastContentType = smpp_base:broadcast_content_type(0, 0),
    BcastFreqInterval = smpp_base:broadcast_frequency_interval(9, 10),
    BcastParams = [{message_id, "1"},
                   {message_payload, ct:get_config(long_sm)},
                   {broadcast_area_identifier, [BcastArea]},
                   {broadcast_content_type, BcastContentType},
                   {broadcast_rep_num, 1},
                   {broadcast_frequency_interval, BcastFreqInterval}],
    test_esme:broadcast_sm(BcastParams),
    test_esme:cancel_broadcast_sm([{message_id, "1"}]),
    test_esme:cancel_sm([{destination_addr, "0123456789"},
                           {message_id, "1"}]),
    DataSmParams = [{destination_addr, "0123456789"},
                    {message_payload, ct:get_config(long_sm)}],
    test_esme:data_sm(DataSmParams),
    QueryBcastParams = [{message_id, "1"},
                        {broadcast_area_identifier, [BcastArea]},
                        {broadcast_area_success, [50]}],
    test_esme:query_broadcast_sm(QueryBcastParams),
    test_esme:query_sm([{message_id, "1"}]),
    ReplaceSmParams = [{destination_addr, "0123456789"},
                      {short_message, ct:get_config(short_sm)}],
    test_esme:replace_sm([{message_id, "1"} | ReplaceSmParams]),
    % test enquire link
    timer:sleep(65000),
    ok = test_esme:swap_log_handler(smpp_disk_log_hlr, smpp_tty_log_hlr),
    DestAddrSme = fun(X) -> smpp_base:dest_address_sme(1, 1, 1, X) end,
    Nums = [DestAddrSme(X) || X <- ["0123456789", "0234567890", "0345678901"]],
    SubmitMutiParams = [{short_message, ct:get_config(short_sm)},
                        {dest_address, Nums}],
    test_esme:submit_multi(SubmitMutiParams),
    SubmitSmParams = [{destination_addr, "0123456789"},
                      {short_message, ct:get_config(long_sm)}],
    test_esme:submit_sm(SubmitSmParams),
    timer:sleep(30000),
    test_esme:submit_sm(SubmitSmParams),
    test_esme:submit_sm(SubmitSmParams),
    timer:sleep(3000),
    _Args = test_esme:delete_log_handler(smpp_tty_log_hlr),
    Failure = test_esme:failure(),
    Success = 14 - Failure,
    Success = test_esme:success(),
    test_esme:stop(),
    ok.


queue() ->
    [{userdata, [{doc, "Tests the queue ESME functions."}]}].

queue(_Conf) ->
    {ok, _Esme} = test_esme:start_link(tx, false),
    wait_bound(),
    ok = test_esme:pause(),
    ok = test_esme:rps_max(2),
    2 = test_esme:rps_max(),
    BcastArea = smpp_base:broadcast_area(0, "area_name"),
    BcastContentType = smpp_base:broadcast_content_type(0, 0),
    BcastFreqInterval = smpp_base:broadcast_frequency_interval(9, 10),
    BcastParams = [{message_id, "1"},
                   {message_payload, ct:get_config(long_sm)},
                   {broadcast_area_identifier, [BcastArea]},
                   {broadcast_content_type, BcastContentType},
                   {broadcast_rep_num, 1},
                   {broadcast_frequency_interval, BcastFreqInterval}],
    test_esme:queue_broadcast_sm(BcastParams),
    test_esme:queue_broadcast_sm(BcastParams, ?HIGH_PRIORITY),
    test_esme:queue_cancel_broadcast_sm([{message_id, "1"}]),
    test_esme:queue_cancel_broadcast_sm([{message_id, "1"}], ?HIGH_PRIORITY),
    0 = test_esme:rps(),
    4 = test_esme:queue_len(),
    test_esme:resume(),
    CancelParams = [{destination_addr, "0123456789"}, {message_id, "1"}],
    test_esme:queue_cancel_sm(CancelParams),
    test_esme:queue_cancel_sm(CancelParams, ?HIGH_PRIORITY),
    DataSmParams = [{destination_addr, "0123456789"},
                    {message_payload, ct:get_config(long_sm)}],
    timer:sleep(1100),
    2 = test_esme:rps(),
    timer:sleep(2000),
    0 = test_esme:queue_len(),
    [] = test_esme:queue_out(),
    test_esme:pause(),
    test_esme:queue_data_sm(DataSmParams),
    test_esme:queue_data_sm(DataSmParams, ?HIGH_PRIORITY),
    QueryBcastParams = [{message_id, "1"},
                        {broadcast_area_identifier, [BcastArea]},
                        {broadcast_area_success, [50]}],
    test_esme:queue_query_broadcast_sm(QueryBcastParams),
    [{{data_sm, DataSmParams}, _Args}] = test_esme:queue_out(),
    test_esme:queue_query_broadcast_sm(QueryBcastParams, ?HIGH_PRIORITY),
    test_esme:queue_query_sm([{message_id, "1"}]),
    test_esme:queue_query_sm([{message_id, "1"}], ?HIGH_PRIORITY),
    SubmitSmParams = [{destination_addr, "0123456789"},
                      {short_message, ct:get_config(short_sm)}],
    ReplaceParams = [{message_id, "1"} | SubmitSmParams],
    test_esme:queue_replace_sm(ReplaceParams),
    test_esme:queue_replace_sm(ReplaceParams, ?HIGH_PRIORITY),
    DestAddrSme = fun(X) -> smpp_base:dest_address_sme(1, 1, 1, X) end,
    Nums = [DestAddrSme(X) || X <- ["0123456789", "0234567890", "0345678901"]],
    SubmitMutiParams = [{short_message, ct:get_config(short_sm)},
                        {dest_address, Nums}],
    test_esme:queue_submit_multi(SubmitMutiParams),
    test_esme:queue_submit_multi(SubmitMutiParams, ?HIGH_PRIORITY),
    test_esme:queue_submit_sm(SubmitSmParams),
    test_esme:queue_submit_sm(SubmitSmParams, ?HIGH_PRIORITY),
    test_esme:queue_submit_sm(SubmitSmParams),
    test_esme:queue_submit_sm(SubmitSmParams, ?HIGH_PRIORITY),
    test_esme:queue_broadcast_sm(BcastParams),
    0 = test_esme:rps(),
    14 = test_esme:queue_len(),
    test_esme:resume(),
    test_esme:queue_broadcast_sm(BcastParams, ?HIGH_PRIORITY),
    test_esme:queue_cancel_broadcast_sm([{message_id, "1"}]),
    test_esme:queue_cancel_broadcast_sm([{message_id, "1"}], ?HIGH_PRIORITY),
    test_esme:rps_max(1000),
    timer:sleep(2000),
    test_esme:pause(),
    test_esme:queue_cancel_sm(CancelParams),
    test_esme:queue_cancel_sm(CancelParams, ?HIGH_PRIORITY),
    test_esme:queue_data_sm(DataSmParams),
    test_esme:queue_data_sm(DataSmParams, ?HIGH_PRIORITY),
    [{{cancel_sm, CancelParams}, _Args1}, {{data_sm, DataSmParams}, _Args2}] =
        test_esme:queue_out(2),
    2 = test_esme:queue_len(),
    test_esme:resume(),
    test_esme:queue_query_broadcast_sm(QueryBcastParams),
    test_esme:queue_query_sm([{message_id, "1"}]),
    test_esme:queue_replace_sm(ReplaceParams),
    test_esme:queue_replace_sm(ReplaceParams),
    test_esme:queue_submit_sm(SubmitSmParams, ?HIGH_PRIORITY),
    wait_empty(),
    0 = test_esme:queue_len(),
    test_esme:stop(),
    ok.


%%%-----------------------------------------------------------------------------
%%% Tracing Util Functions
%%%-----------------------------------------------------------------------------
add_trace(TpFun, {Mod, Fun, Spec}) ->
    dbg:TpFun(Mod, Fun, Spec);
add_trace(TpFun, {Mod, Fun}) ->
    dbg:TpFun(Mod, Fun, ?MATCH_SPEC);
add_trace(TpFun, Mod) ->
    dbg:TpFun(Mod, ?MATCH_SPEC).


del_trace(CtpFun, {Mod, Fun, _Spec}) ->
    dbg:CtpFun(Mod, Fun);
del_trace(CtpFun, {Mod, Fun}) ->
    dbg:CtpFun(Mod, Fun);
del_trace(CtpFun, Mod) ->
    dbg:CtpFun(Mod).

%%%-----------------------------------------------------------------------------
%%% Stub Util Functions
%%%-----------------------------------------------------------------------------
load_stub(Stub, NegTest) ->
    Opts = if NegTest -> [binary, {d, neg_case}]; true ->  [binary] end,
    Erl = atom_to_list(Stub) ++ ".erl",
    ct:print("Compiling ~s with options ~p", [Erl, Opts]),
    {ok, Mod, Bin} = compile:file(filename:join(?STUBS_DIR, Erl), Opts),
    ct:print("Purge default ~p stub", [Mod]),
    code:purge(Mod),
    code:delete(Mod),
    ct:print("Loading new ~p stub", [Mod]),
    Beam = atom_to_list(Mod) ++ code:objfile_extension(),
    {module, Mod} = code:load_binary(Mod, Beam, Bin).


purge_stub(Stub) ->
    ct:print("Purge ~p stub", [Stub]),
    code:purge(Stub),
    code:delete(Stub),
    ct:print("Reloading default ~p stub", [Stub]),
    {module, Stub} = code:load_file(Stub).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
wait_bound() ->
    case test_esme:bound() of
        true ->
            ok;
        false ->
            timer:sleep(ct:get_config(retry_time)),
            wait_bound()
    end.


wait_empty() ->
    case test_esme:queue_len() of
        0 ->
            ok;
        _ ->
            timer:sleep(1000),
            wait_empty()
    end.

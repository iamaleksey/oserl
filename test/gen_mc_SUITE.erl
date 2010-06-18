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
-module(gen_mc_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").
-include_lib("oserl/include/smpp_globals.hrl").
-include_lib("oserl/include/oserl.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(LISTEN_PORT, 9999).
-define(LOCALHOST, {127, 0, 0, 1}).
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(MC_TIMERS, ?TIMERS(15000, 15000, 30000, 20000)).
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [receiver, outbind, queue_receiver, errors].


sequences() ->
    [].


suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
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
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(_Conf) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
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
    lists:foreach(fun(Tpl) -> add_trace(tpl, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    end_traces(Case),
    end_stubs(Case),
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
%%% TEST CASES
%%%-----------------------------------------------------------------------------
receiver() ->
    [{userdata, [{doc, "Tests the ESME as receiver."}]}].

receiver(_Conf) ->
    {ok, _Mc} = test_mc:start_link(false),
    {ok, _Esme} = test_esme:start_link(rx, false),
    wait_bound(),
    alert_notification(),
    deliver_loop(1, 100),
    wait(180),
    test_mc:stop(),
    timer:sleep(5000),
    test_esme:stop(),
    ok.


outbind() ->
    [{userdata, [{doc, "Tests outbind."}]}].

outbind(_Conf) ->
    {ok, _Mc} = test_mc:start_link(false),
    {ok, _Esme} = test_esme:start_link(outbind, false),
    ok = test_esme:listen(?LISTEN_PORT),
    timer:sleep(3000),
    SystemId = ct:get_config(esme_id),
    Pwd = ct:get_config(esme_pwd),
    Params = [{system_id, SystemId}, {password, Pwd}],
    ok = test_mc:outbind(Params),
    timer:sleep(3000),
    alert_notification(),
    deliver_loop(1, 100),
    wait(180),
    test_mc:stop(),
    timer:sleep(1000),
    test_esme:stop(),
    ok.


queue_receiver() ->
    [{userdata, [{doc, "Tests queue MC operations."}]}].

queue_receiver(_Conf) ->
    {ok, _Mc} = test_mc:start_link(false),
    {ok, _Esme} = test_esme:start_link(trx, false),
    wait_bound(),
    test_mc:resume(),
    queue_deliver_loop(1, 100),
    wait(180),
    test_mc:stop(),
    timer:sleep(1000),
    test_esme:stop(),
    ok.


errors() ->
    [{userdata, [{doc, "Test some errors"}]}].

errors(_Conf) ->
    ListenOpts = [{ip, ?LOCALHOST}, {reuseaddr, true}],
    {ok, LSock} = gen_tcp:listen(?DEFAULT_SMPP_PORT, ListenOpts),
    {error, eaddrinuse} = test_mc:start(false),
    gen_tcp:close(LSock),
    {ok, Mc} = test_mc:start(false),
    timer:sleep(2000),
    false = is_process_alive(Mc),
    test_mc:stop(),
    ok.



%%%-----------------------------------------------------------------------------
%%% TRACING UTIL FUNCTIONS
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
%%% STUB UTIL FUNCTIONS
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
alert_notification() ->
    Params = [{source_addr_ton, ?TON_NATIONAL},
              {source_addr_npi, ?NPI_ISDN},
              {source_addr, "0123456789"},
              {esme_addr_ton, ?TON_NATIONAL},
              {esme_addr_npi, ?NPI_ISDN},
              {esme_addr, "*"},
              {ms_availability_status, ?MS_AVAILABILITY_STATUS_AVAILABLE}],
    test_mc:alert_notification(Params).


deliver_loop(Sleep, Times) ->
    deliver_loop(hd(ct:get_config(deliver_sm)), Sleep, Times).

deliver_loop({X, Y} = Msg, Sleep, Times) when Times > 0 ->
    Params = [{source_addr, X},
              {source_addr_ton, ?TON_NATIONAL},
              {source_addr_npi, ?NPI_ISDN},
              {dest_addr_ton, ?TON_NATIONAL},
              {dest_addr_npi, ?NPI_ISDN},
              {destination_addr, "9999"},
              {esm_class, ?ESM_CLASS_DEFAULT},
              {protocol_id, ?PROTOCOL_ID_GSM},
              {priority_flag, ?PRIORITY_FLAG_GSM_CBS_NORMAL}],
    test_mc:deliver_sm([{short_message, Y} | Params]),
    test_mc:data_sm([{message_payload, Y} | Params]),
    if Sleep > 0 -> timer:sleep(Sleep); true -> ok end,
    deliver_loop(Msg, Sleep, Times - 1);
deliver_loop(_Msg, _Sleep, _Time) ->
    ok.


queue_deliver_loop(Sleep, Times) ->
    queue_deliver_loop(hd(ct:get_config(deliver_sm)), Sleep, Times).

queue_deliver_loop({X, Y} = Msg, Sleep, Times) when Times > 0 ->
    Params = [{source_addr, X},
              {source_addr_ton, ?TON_NATIONAL},
              {source_addr_npi, ?NPI_ISDN},
              {dest_addr_ton, ?TON_NATIONAL},
              {dest_addr_npi, ?NPI_ISDN},
              {destination_addr, "9999"},
              {esm_class, ?ESM_CLASS_DEFAULT},
              {protocol_id, ?PROTOCOL_ID_GSM},
              {priority_flag, ?PRIORITY_FLAG_GSM_CBS_NORMAL}],
    test_mc:queue_deliver_sm([{short_message, Y} | Params]),
    test_mc:queue_data_sm([{message_payload, Y} | Params]),
    if Sleep > 0 -> timer:sleep(Sleep); true -> ok end,
    queue_deliver_loop(Msg, Sleep, Times - 1);
queue_deliver_loop(_Msg, _Sleep, _Time) ->
    ok.


wait(Count) ->
    case test_esme:recv() of
        C when C >= Count ->
            ok;
        _Other ->
            timer:sleep(200),
            wait(Count)
    end.


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
            timer:sleep(200),
            wait_empty()
    end.

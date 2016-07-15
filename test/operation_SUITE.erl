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
-module(operation_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_OPERATIONS, 1000).
-define(MAX_TIME, 1000).
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [performance].


sequences() ->
    [].


suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
    {A1, A2, A3} = erlang:timestamp(),
    random:seed(A1, A2, A3),
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
    lists:foreach(fun(Tpl) -> add_trace(tp, Tpl) end, Tpls).

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
performance() ->
    [{userdata, [{doc, "Test performace restrictions."}]}].

performance(Conf) ->
    MaxTime = ?config(max_time, Conf),
    MaxOperations = ?config(max_operations, Conf),
    {SrcAddr, ShortMsg} = hd(ct:get_config(deliver_sm)),
    Args = [SrcAddr, ShortMsg, MaxOperations],
    {T, _} = timer:tc(?MODULE, performance_test, Args),
    if trunc(T/1000) > MaxTime -> exit(too_slow); true -> ok end.

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


performance_test(_SrcAddr, _ShortMsg, 0) ->
    ok;
performance_test(SrcAddr, ShortMsg, Times) ->
    Params = [{service_type, ""},
              {source_addr_ton, ?TON_NATIONAL},
              {source_addr_npi, ?NPI_ISDN},
              {source_addr, SrcAddr},
              {dest_addr_ton, ?TON_NATIONAL},
              {dest_addr_npi, ?NPI_ISDN},
              {destination_addr, "9999"},
              {esm_class, ?ESM_CLASS_DEFAULT},
              {protocol_id, ?PROTOCOL_ID_GSM},
              {priority_flag, ?PRIORITY_FLAG_GSM_CBS_NORMAL},
              {short_message, ShortMsg}],
    {ok, Req} = operation:pack(operation:new(16#00000005, 2, Params)),
    {ok, _PduReq} = operation:unpack(list_to_binary(Req)),
    ParamsResp = [{message_id, "12345"}],
    {ok, Resp} = operation:pack(operation:new(16#80000005, 0, 3, ParamsResp)),
    {ok, _PduResp} = operation:unpack(list_to_binary(Resp)),
    performance_test(SrcAddr, ShortMsg, Times - 1).

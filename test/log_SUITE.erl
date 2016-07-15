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
-module(log_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [api, disk_log, tty_log, performance].


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
api() ->
    [{userdata, [{doc, "Tests the API of the loggin manager."}]}].

api(_Conf) ->
    {ok, Pid1} = smpp_log_mgr:start(),
    {ok, Pid2} = smpp_log_mgr:start_link(),
    Params = [{system_id, "oserl"}],
    UnbindRespError = smpp_operation:new(16#80000002, 2, 1, Params),
    {ok, DataError} = smpp_operation:pack(UnbindRespError),
    UnbindRespOk = smpp_operation:new(16#80000002, 3, Params),
    {ok, DataOk} = smpp_operation:pack(UnbindRespOk),
    ok = smpp_log_mgr:pdu(Pid1, DataError),
    ok = smpp_log_mgr:pdu(Pid1, DataOk),
    LogArgs = ct:get_config(log_args, []),
    ok = smpp_log_mgr:add_handler(Pid2, smpp_disk_log_hlr, LogArgs),
    ok = smpp_log_mgr:pdu(Pid2, DataError),
    ok = smpp_log_mgr:pdu(Pid2, DataOk),
    ok = smpp_log_mgr:add_handler(Pid2, smpp_tty_log_hlr, LogArgs),
    ok = smpp_log_mgr:pdu(Pid2, DataError),
    ok = smpp_log_mgr:pdu(Pid2, DataOk),
    TtyLogArgs = [{format, fun(Pdu) -> cl_binary:to_hexlist(Pdu) end},
                  {filter, fun(_Pdu) -> true end}|LogArgs],
    Handler1 = {smpp_tty_log_hlr, []},
    Handler2 = {smpp_tty_log_hlr, TtyLogArgs},
    ok = smpp_log_mgr:swap_handler(Pid2, Handler1, Handler2),
    ok = smpp_log_mgr:pdu(Pid2, DataError),
    ok = smpp_log_mgr:pdu(Pid2, DataOk),
    _TArgs = smpp_log_mgr:delete_handler(Pid2, smpp_tty_log_hlr, []),
    ok = smpp_log_mgr:pdu(Pid2, DataError),
    ok = smpp_log_mgr:pdu(Pid2, DataOk),
    _DArgs = smpp_log_mgr:delete_handler(Pid2, smpp_disk_log_hlr, []),
    ok = smpp_log_mgr:pdu(Pid2, DataError),
    ok = smpp_log_mgr:pdu(Pid2, DataOk),
    ok = smpp_log_mgr:stop(Pid1),
    ok = smpp_log_mgr:stop(Pid2).


disk_log() ->
    [{userdata, [{doc, "Tests the disk log handler."}]}].

disk_log(_Conf) ->
    {ok, Pid} = smpp_log_mgr:start_link(),
    Params = [{system_id, "oserl"}],
    UnbindRespError = smpp_operation:new(16#80000002, 2, 1, Params),
    {ok, DataError} = smpp_operation:pack(UnbindRespError),
    UnbindRespOk = smpp_operation:new(16#80000002, 3, Params),
    {ok, DataOk} = smpp_operation:pack(UnbindRespOk),
    Time1 = calendar:local_time(),
    timer:sleep(1500),
    LogArgs = ct:get_config(log_args, []),
    File = proplists:get_value(file, LogArgs),
    os:cmd("rm -f " ++ File ++ "*"),
    ok = smpp_log_mgr:add_handler(Pid, smpp_disk_log_hlr, LogArgs),
    ok = smpp_log_mgr:pdu(Pid, DataError),
    ok = smpp_log_mgr:pdu(Pid, DataOk),
    timer:sleep(3000),
    2 = smpp_disk_log_hlr:count(smpp_log, any, fun(_) -> true end),
    2 = smpp_disk_log_hlr:count(smpp_log, {from, Time1}, fun(_) -> true end),
    ok = smpp_log_mgr:pdu(Pid, DataError),
    ok = smpp_log_mgr:pdu(Pid, DataOk),
    timer:sleep(3000),
    Time2 = calendar:local_time(),
    0 = smpp_disk_log_hlr:count(smpp_log, {until, Time1}, fun(_) -> true end),
    4 = smpp_disk_log_hlr:count(smpp_log, {lapse, Time1, Time2},
                                fun(_) -> true end),
    4 = smpp_disk_log_hlr:count(smpp_log, any, fun(_) -> true end),
    4 = smpp_disk_log_hlr:count(smpp_log, {from, Time1}, fun(_) -> true end),
    0 = smpp_disk_log_hlr:count(smpp_log, {from, Time1}, fun(_) -> false end),
    DiskLogArgs = [{format, fun(Pdu) -> cl_binary:to_hexlist(Pdu) end},
                   {filter, fun(_Pdu) -> true end}|LogArgs],
    Handler1 = {smpp_disk_log_hlr, []},
    Handler2 = {smpp_disk_log_hlr, DiskLogArgs},
    ok = smpp_log_mgr:swap_handler(Pid, Handler1, Handler2),
    ok = smpp_log_mgr:pdu(Pid, DataError),
    ok = smpp_log_mgr:pdu(Pid, DataOk),
    Handler3 = {smpp_disk_log_hlr, [{file, "/wrong/file/name"}]},
    {error, {error, {file_error, _File, _Details}}} =
        smpp_log_mgr:swap_handler(Pid, Handler1, Handler3),
    ok = smpp_log_mgr:add_handler(Pid, smpp_disk_log_hlr, []),
    ok = gen_event:notify(Pid, other_event),
    {ok, state} = smpp_disk_log_hlr:handle_info(info, state),
    {ok, state} = smpp_disk_log_hlr:code_change(old_vsn, state, extra),
%    {error, {'EXIT',{function_clause, _}}} =
%        gen_event:call(Pid, smpp_disk_log_hlr, call),
    _TArgs = smpp_log_mgr:delete_handler(Pid, smpp_disk_log_hlr, []),
    ok = smpp_log_mgr:stop(Pid).



tty_log() ->
    [{userdata, [{doc, "Tests the tty log handler."}]}].

tty_log(_Conf) ->
    {ok, Pid} = smpp_log_mgr:start_link(),
    Params = [{system_id, "oserl"}],
    UnbindRespError = smpp_operation:new(16#80000002, 2, 1, Params),
    {ok, DataError} = smpp_operation:pack(UnbindRespError),
    UnbindRespOk = smpp_operation:new(16#80000002, 3, Params),
    {ok, DataOk} = smpp_operation:pack(UnbindRespOk),
    ok = smpp_log_mgr:add_handler(Pid, smpp_tty_log_hlr, []),
    ok = smpp_log_mgr:pdu(Pid, DataError),
    ok = smpp_log_mgr:pdu(Pid, DataOk),
    LogArgs = ct:get_config(log_args, []),
    TtyLogArgs = [{format, fun(Pdu) -> cl_binary:to_hexlist(Pdu) end},
                  {filter, fun(_Pdu) -> true end}|LogArgs],
    Handler1 = {smpp_tty_log_hlr, []},
    Handler2 = {smpp_tty_log_hlr, TtyLogArgs},
    ok = smpp_log_mgr:swap_handler(Pid, Handler1, Handler2),
    ok = smpp_log_mgr:pdu(Pid, DataError),
    ok = smpp_log_mgr:pdu(Pid, DataOk),
    Handler3 = {smpp_tty_log_hlr, [{file, "/wrong/file/name"}]},
    {error, {error, enoent}} = smpp_log_mgr:swap_handler(Pid, Handler1, Handler3),
    ok = smpp_log_mgr:add_handler(Pid, smpp_tty_log_hlr, []),
    ok = gen_event:notify(Pid, other_event),
    {ok, state} = smpp_tty_log_hlr:handle_info(info, state),
    {ok, state} = smpp_tty_log_hlr:code_change(old_vsn, state, extra),
%    {error, {'EXIT',{function_clause, _}}} =
%        gen_event:call(Pid, smpp_tty_log_hlr, call),
    _TArgs = smpp_log_mgr:delete_handler(Pid, smpp_tty_log_hlr, []),
    ok = smpp_log_mgr:stop(Pid).


performance() ->
    [{userdata, [{doc, "Test performace restrictions."}]}].

performance(Conf) ->
    MaxTime = ?config(max_time, Conf),
    {T, _} = timer:tc(?MODULE, performance_test, [Conf]),
    if trunc(T/1000) > MaxTime -> exit(too_slow); true -> ok end.

performance_test(_Conf) ->
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

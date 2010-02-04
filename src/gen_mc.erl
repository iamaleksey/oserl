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
-module(gen_mc).
-behaviour(gen_server).
-behaviour(gen_mc_session).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% BEHAVIOUR EXPORTS
-export([behaviour_info/1]).

%%% START/STOP EXPORTS
-export([start/3, start/4, start_link/3, start_link/4]).

%%% SERVER EXPORTS
-export([call/2, call/3, cast/2, reply/2]).

%%% CONNECT EXPORTS
-export([close/2]).

%%% SMPP EXPORTS
-export([alert_notification/3,
         data_sm/4,
         data_sm/5,
         deliver_sm/4,
         deliver_sm/5,
         outbind/4,
         outbind/5,
         unbind/3]).

%%% QUEUE EXPORTS
-export([queue_data_sm/4,
         queue_data_sm/5,
         queue_deliver_sm/4,
         queue_deliver_sm/5,
         queue_len/2,
         queue_out/2,
         queue_out/3]).

%%% LOG EXPORTS
-export([add_log_handler/3, delete_log_handler/3, swap_log_handler/3]).

%%% RPS EXPORTS
-export([pause/2, resume/2, rps/2, rps_avg/2, rps_max/2, rps_max/3]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE CHANGE EXPORTS
-export([code_change/3]).

%%% INTERNAL GEN_MC_SESSION EXPORTS
-export([handle_accept/2,
         handle_bind/2,
         handle_closed/2,
         handle_enquire_link/2,
         handle_operation/2,
         handle_resp/3,
         handle_unbind/2]).

%%% MACROS
-define(PRIORITY, 10).
-define(RPS, 1000).
-define(SECOND, 1000).

%%% RECORDS
-record(session, {pid, ref, consumer, rps}).

-record(st, {mod, mod_st, sessions = [], listener, log, timers, lsock}).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR EXPORTS
%%%-----------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init, 1},
     {terminate, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {code_change, 3},
     {handle_accept, 4},
     {handle_bind_receiver, 4},
     {handle_bind_transceiver, 4},
     {handle_bind_transmitter, 4},
     {handle_broadcast_sm, 4},
     {handle_cancel_broadcast_sm, 4},
     {handle_cancel_sm, 4},
     {handle_closed, 3},
     {handle_data_sm, 4},
     {handle_query_broadcast_sm, 4},
     {handle_query_sm, 4},
     {handle_replace_sm, 4},
     {handle_req, 5},
     {handle_resp, 4},
     {handle_submit_multi, 4},
     {handle_submit_sm, 4},
     {handle_unbind, 4}];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(Module, Args, Opts) ->
    {McOpts, SrvOpts} = split_options(Opts),
    gen_server:start(?MODULE, {Module, Args, McOpts}, SrvOpts).


start(SrvName, Module, Args, Opts) ->
    {McOpts, SrvOpts} = split_options(Opts),
    gen_server:start(SrvName, ?MODULE, {Module, Args, McOpts}, SrvOpts).


start_link(Module, Args, Opts) ->
    {McOpts, SrvOpts} = split_options(Opts),
    gen_server:start_link(?MODULE, {Module, Args, McOpts}, SrvOpts).


start_link(SrvName, Module, Args, Opts) ->
    {McOpts, SrvOpts} = split_options(Opts),
    gen_server:start_link(SrvName, ?MODULE, {Module, Args, McOpts}, SrvOpts).


%%%-----------------------------------------------------------------------------
%%% SERVER EXPORTS
%%%-----------------------------------------------------------------------------
call(SrvRef, Req) ->
    gen_server:call(SrvRef, {call, Req}).


call(SrvRef, Req, Timeout) ->
    gen_server:call(SrvRef, {call, Req}, Timeout).


cast(SrvRef, Req) ->
    gen_server:cast(SrvRef, {cast, Req}).


reply(Client, Reply) ->
    gen_server:reply(Client, Reply).

%%%-----------------------------------------------------------------------------
%%% CONNECT EXPORTS
%%%-----------------------------------------------------------------------------
close(SrvRef, Session) ->
    gen_server:cast(SrvRef, {close, Session}).

%%%-----------------------------------------------------------------------------
%%% SMPP EXPORTS
%%%-----------------------------------------------------------------------------
alert_notification(SrvRef, Session, Params) ->
    gen_server:cast(SrvRef, {{alert_notification, Params}, Session}).


data_sm(SrvRef, Session, Params, Args) ->
    data_sm(SrvRef, Session, Params, Args, ?ASSERT_TIME).

data_sm(SrvRef, Session, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{{data_sm, Params}, Args}, Session},  Timeout).


deliver_sm(SrvRef, Session, Params, Args) ->
    deliver_sm(SrvRef, Session, Params, Args, ?ASSERT_TIME).

deliver_sm(SrvRef, Session, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{{deliver_sm, Params}, Args}, Session}, Timeout).


outbind(SrvRef, Addr, Opts, Params) ->
    outbind(SrvRef, Addr, Opts, Params, ?ASSERT_TIME).

outbind(SrvRef, Addr, Opts, Params, Timeout) ->
    Pid = ref_to_pid(SrvRef),
    gen_server:call(Pid, {outbind, [{addr, Addr} | Opts], Params}, Timeout).


unbind(SrvRef, Session, Args) ->
    gen_server:cast(SrvRef, {{{unbind, []}, Args}, Session}).

%%%-----------------------------------------------------------------------------
%%% QUEUE EXPORTS
%%%-----------------------------------------------------------------------------
queue_data_sm(SrvRef, Session, Params, Args) ->
    queue_data_sm(SrvRef, Session, Params, Args, ?PRIORITY).

queue_data_sm(SrvRef, Session, Params, Args, Priority) ->
    queue(SrvRef, Session, {data_sm, Params}, Args, Priority).


queue_deliver_sm(SrvRef, Session, Params, Args) ->
    queue_deliver_sm(SrvRef, Session, Params, Args, ?PRIORITY).

queue_deliver_sm(SrvRef, Session, Params, Args, Priority) ->
    queue(SrvRef, Session, {deliver_sm, Params}, Args, Priority).


queue_len(_SrvRef, Session) ->
    QueueSrv = cl_queue_tab:lookup(Session),
    cl_queue_srv:len(QueueSrv).


queue_out(SrvRef, Session) ->
    queue_out(SrvRef, Session, 1).

queue_out(_SrvRef, Session, Num) ->
    QueueSrv = cl_queue_tab:lookup(Session),
    cl_queue_srv:out(QueueSrv, Num).

%%%-----------------------------------------------------------------------------
%%% LOG EXPORTS
%%%-----------------------------------------------------------------------------
add_log_handler(SrvRef, Handler, Args) ->
    gen_server:call(SrvRef, {add_log_handler, Handler, Args}).


delete_log_handler(SrvRef, Handler, Args) ->
    gen_server:call(SrvRef, {delete_log_handler, Handler, Args}).


swap_log_handler(SrvRef, Handler1, Handler2) ->
    gen_server:call(SrvRef, {swap_log_handler, Handler1, Handler2}).

%%%-----------------------------------------------------------------------------
%%% RPS EXPORTS
%%%-----------------------------------------------------------------------------
pause(SrvRef, Session) ->
    gen_server:call(SrvRef, {pause, Session}, ?ASSERT_TIME).


resume(SrvRef, Session) ->
    gen_server:cast(SrvRef, {resume, Session}).


rps(_SrvRef, Session) ->
    QueueSrv = cl_queue_tab:lookup(Session),
    cl_queue_srv:rps(QueueSrv).


rps_avg(_SrvRef, Session) ->
    QueueSrv = cl_queue_tab:lookup(Session),
    cl_queue_srv:rps_avg(QueueSrv).


rps_max(SrvRef, Session) ->
    gen_server:call(SrvRef, {rps_max, Session}, ?ASSERT_TIME).


rps_max(SrvRef, Session, Rps) ->
    gen_server:cast(SrvRef, {{rps_max, Rps}, Session}).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init({Mod, Args, Opts}) ->
    case smpp_session:listen(Opts) of
        {ok, LSock} ->
            {ok, Log} = smpp_log_mgr:start_link(),
            Timers = proplists:get_value(timers, Opts, ?DEFAULT_TIMERS_SMPP),
            SessionOpts = [{log, Log}, {lsock, LSock}, {timers, Timers}],
            % Start a listening session
            {ok, Pid} = gen_mc_session:start_link(?MODULE, SessionOpts),
            St = #st{mod = Mod,
                     listener = Pid,
                     log = Log,
                     timers = Timers,
                     lsock = LSock},
            pack((St#st.mod):init(Args), St);
        {error, Reason} ->
            {stop, Reason}
    end.


terminate(Reason, St) ->
    (St#st.mod):terminate(Reason, St#st.mod_st),
    gen_mc_session:stop(St#st.listener, Reason),
    lists:foreach(fun(X) -> session_stop(Reason, X) end, St#st.sessions),
    gen_tcp:close(St#st.lsock),
    smpp_log_mgr:stop(St#st.log).

%%%-----------------------------------------------------------------------------
%%% HANDLE EXPORTS
%%%-----------------------------------------------------------------------------
handle_call({call, Req}, From, St) ->
    pack((St#st.mod):handle_call(Req, From, St#st.mod_st), St);
handle_call({{accepted, Opts}, Pid}, _From, St) ->
    {reply, ok, St#st{sessions = [session_new(Pid, Opts) | St#st.sessions]}};
handle_call({{rejected, _Error}, Pid}, _From, St) ->
    unlink(Pid),
    {reply, ok, St};
handle_call({{{CmdName, Params} = Req, Args}, Pid}, _From, St) ->
    Ref = req_send(Pid, CmdName, Params),
    case pack((St#st.mod):handle_req(Pid, Req, Args, Ref, St#st.mod_st), St) of
        {noreply, NewSt} ->
            {reply, ok, NewSt};
        {noreply, NewSt, Timeout} ->
            {reply, ok, NewSt, Timeout};
        {stop, Reason, NewSt} ->
            {stop, ok, Reason, NewSt}
    end;
handle_call({outbind, Opts, Params}, _From, St) ->
    case gen_mc_session:start_link(?MODULE, [{log, St#st.log} | Opts]) of
        {ok, Pid} ->
            _Ref = erlang:monitor(process, Pid),
            unlink(Pid),
            ok = req_send(Pid, outbind, Params),
            Session = session_new(Pid, Opts),
            {reply, {ok, Pid}, St#st{sessions = [Session | St#st.sessions]}};
        Error ->
            {reply, Error, St}
    end;
handle_call({pause, Pid}, _From, St) ->
    Sss = session(Pid, St#st.sessions),
    ok = cl_consumer:pause(Sss#session.consumer),
    {reply, ok, St};
handle_call({add_log_handler, Handler, Args}, _From, St) ->
    {reply, smpp_log_mgr:add_handler(St#st.log, Handler, Args), St};
handle_call({delete_log_handler, Handler, Args}, _From, St) ->
    {reply, smpp_log_mgr:delete_handler(St#st.log, Handler, Args), St};
handle_call({swap_log_handler, Handler1, Handler2}, _From, St) ->
    {reply, smpp_log_mgr:swap_handler(St#st.log, Handler1, Handler2), St};
handle_call({rps, Pid}, _From, St) ->
    Sss = session(Pid, St#st.sessions),
    {reply, Sss#session.rps, St};
handle_call({rps_max, Pid}, _From, St) ->
    Sss = session(Pid, St#st.sessions),
    {reply, Sss#session.rps, St};
handle_call({{handle_unbind, Pdu}, Pid}, From, St) ->
    pack((St#st.mod):handle_unbind(Pid, Pdu, From, St#st.mod_st), St);
handle_call({{handle_accept, Addr}, Pid}, From, #st{listener = Pid} = St) ->
    Opts = [{lsock, St#st.lsock}, {log, St#st.log}, {timers, St#st.timers}],
    {ok, Listener} = gen_mc_session:start_link(?MODULE, Opts),
    NewSt = St#st{listener = Listener},
    pack((NewSt#st.mod):handle_accept(Pid, Addr, From, NewSt#st.mod_st), NewSt);
handle_call({{Fun, Pdu}, Pid}, From, St) ->
    pack((St#st.mod):Fun(Pid, Pdu, From, St#st.mod_st), St);
handle_call({handle_enquire_link, _Pdu}, _From, St) ->
    {reply, ok, St}.


handle_cast({cast, Req}, St) ->
    pack((St#st.mod):handle_cast(Req, St#st.mod_st), St);
handle_cast({close, Pid}, St) ->
    try
        Sss = session(Pid, St#st.sessions),
        ok = cl_consumer:pause(Sss#session.consumer),
        ok = gen_mc_session:stop(Pid)
    catch
        _Class:_NotRunning ->
            ok
    end,
    {noreply, St};
handle_cast({{alert_notification, Params}, Pid}, St) ->
    ok = req_send(Pid, alert_notification, Params),
    {noreply, St};
handle_cast({{{unbind, Params}, Args}, Pid}, St) ->
    Ref = req_send(Pid, unbind, Params),
    pack((St#st.mod):handle_req(Pid, unbind, Args, Ref, St#st.mod_st), St);
handle_cast({{rps, Rps}, Pid}, St) ->
    Sss = session(Pid, St#st.sessions),
    L = session_update(Sss#session{rps = Rps}, St#st.sessions),
    {noreply, St#st{sessions = L}};
handle_cast({resume, Pid}, St) ->
    Sss = session(Pid, St#st.sessions),
    try
        true = is_process_alive(Sss#session.consumer),
        ok = cl_consumer:resume(Sss#session.consumer),
        {noreply, St}
    catch
        _Class:_NotRunning ->
            QueueSrv = cl_queue_tab:lookup(Pid),
            Mc = self(),
            ReqFun = fun(X) -> gen_server:call(Mc, {X, Pid}, ?ASSERT_TIME) end,
            {ok, C} = cl_consumer:start_link(QueueSrv, ReqFun, Sss#session.rps),
            L = session_update(Sss#session{consumer = C}, St#st.sessions),
            {noreply, St#st{sessions = L}}
    end;
handle_cast({{rps_max, Rps}, Pid}, St) ->
    Sss = session(Pid, St#st.sessions),
    ok = cl_consumer:rps(Sss#session.consumer, Rps),
    L = session_update(Sss#session{rps = Rps}, St#st.sessions),
    {noreply, St#st{sessions = L}};
handle_cast({{handle_closed, Reason}, Pid}, St) ->
    NewSt = session_closed(Pid, St),
    pack((NewSt#st.mod):handle_closed(Pid, Reason, NewSt#st.mod_st), NewSt);
handle_cast({{handle_resp, Resp, Ref}, Pid}, St) ->
    pack((St#st.mod):handle_resp(Pid, Resp, Ref, St#st.mod_st), St).


handle_info({'DOWN', _Ref, _Type, Pid, Reason}, St) ->
    NewSt = session_closed(Pid, St),
    pack((NewSt#st.mod):handle_closed(Pid, Reason, NewSt#st.mod_st), NewSt);
handle_info(Info, St) ->
    pack((St#st.mod):handle_info(Info, St#st.mod_st), St).

%%%-----------------------------------------------------------------------------
%%% CODE CHANGE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(OldVsn, St, Extra) ->
    pack((St#st.mod):code_change(OldVsn, St#st.mod_st, Extra), St).

%%%-----------------------------------------------------------------------------
%%% INTERNAL GEN_MC_SESSION EXPORTS
%%%-----------------------------------------------------------------------------
handle_accept(SrvRef, Addr) ->
    Self = self(),
    case gen_server:call(SrvRef, {{handle_accept, Addr}, Self}, ?ASSERT_TIME) of
        {ok, Opts} ->
            Reply = {accepted, Opts},
            ok = gen_server:call(SrvRef, {Reply, Self}, ?ASSERT_TIME);
        Error ->
            Reply = {rejected, Error},
            ok = gen_server:call(SrvRef, {Reply, Self}, ?ASSERT_TIME),
            Error
    end.


handle_bind(SrvRef, {bind_receiver, Pdu}) ->
    handle_bind(SrvRef, {handle_bind_receiver, Pdu});
handle_bind(SrvRef, {bind_transceiver, Pdu}) ->
    handle_bind(SrvRef, {handle_bind_transceiver, Pdu});
handle_bind(SrvRef, {bind_transmitter, Pdu}) ->
    handle_bind(SrvRef, {handle_bind_transmitter, Pdu});
handle_bind(SrvRef, {HandleFun, Pdu}) ->
    gen_server:call(SrvRef, {{HandleFun, Pdu}, self()}, ?ASSERT_TIME).


handle_closed(SrvRef, Reason) ->
    gen_server:cast(SrvRef, {{handle_closed, Reason}, self()}).


handle_enquire_link(SrvRef, Pdu) ->
    gen_server:call(SrvRef, {handle_enquire_link, Pdu}, ?ASSERT_TIME).


handle_operation(SrvRef, {broadcast_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_broadcast_sm, Pdu});
handle_operation(SrvRef, {cancel_broadcast_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_cancel_broadcast_sm, Pdu});
handle_operation(SrvRef, {cancel_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_cancel_sm, Pdu});
handle_operation(SrvRef, {data_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_data_sm, Pdu});
handle_operation(SrvRef, {query_broadcast_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_query_broadcast_sm, Pdu});
handle_operation(SrvRef, {query_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_query_sm, Pdu});
handle_operation(SrvRef, {replace_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_replace_sm, Pdu});
handle_operation(SrvRef, {submit_multi, Pdu}) ->
    handle_operation(SrvRef, {handle_submit_multi, Pdu});
handle_operation(SrvRef, {submit_sm, Pdu}) ->
    handle_operation(SrvRef, {handle_submit_sm, Pdu});
handle_operation(SrvRef, {HandleFun, Pdu}) ->
    gen_server:call(SrvRef, {{HandleFun, Pdu}, self()}, ?ASSERT_TIME).


handle_resp(SrvRef, Resp, Ref) ->
    gen_server:cast(SrvRef, {{handle_resp, Resp, Ref}, self()}).


handle_unbind(SrvRef, Pdu) ->
    gen_server:call(SrvRef, {{handle_unbind, Pdu}, self()}, ?ASSERT_TIME).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
pack({reply, Reply, ModSt}, St) ->
    {reply, Reply, St#st{mod_st = ModSt}};
pack({reply, Reply, ModSt, Timeout}, St) ->
    {reply, Reply, St#st{mod_st = ModSt}, Timeout};
pack({noreply, ModSt}, St) ->
    {noreply, St#st{mod_st = ModSt}};
pack({noreply, ModSt, Timeout}, St) ->
    {noreply, St#st{mod_st = ModSt}, Timeout};
pack({stop, Reason, Reply, ModSt}, St) ->
    {stop, Reason, Reply, St#st{mod_st = ModSt}};
pack({stop, Reason, ModSt}, St) ->
    {stop, Reason,  St#st{mod_st = ModSt}};
pack({ok, ModSt}, St) ->
    {ok, St#st{mod_st = ModSt}};
pack({ok, ModSt, Timeout}, St) ->
    {ok, St#st{mod_st = ModSt}, Timeout};
pack(Other, _St) ->
    Other.


queue(_SrvRef, Session, Req, Args, Priority) ->
    QueueSrv = cl_queue_tab:lookup(Session),
    ok = cl_queue_srv:in(QueueSrv, {Req, Args}, Priority).


ref_to_pid(Ref) when is_pid(Ref) ->
    Ref;
ref_to_pid(Ref) when is_atom(Ref) ->
    whereis(Ref);
ref_to_pid({global, Name}) ->
    global:whereis_name(Name);
ref_to_pid({Name, Node}) ->
    rpc:call(Node, erlang, whereis, [Name]).


req_send(Pid, CmdName, Params) ->
    try % Need to protect ourselves to make sure that handle_closed is called
        if
            CmdName == alert_notification ->
                gen_mc_session:alert_notification(Pid, Params);
            CmdName == data_sm ->
                gen_mc_session:data_sm(Pid, Params);
            CmdName == deliver_sm ->
                gen_mc_session:deliver_sm(Pid, Params);
            CmdName == outbind ->
                gen_mc_session:outbind(Pid, Params);
            CmdName == unbind ->
                gen_mc_session:unbind(Pid)
        end
    catch
        % Session not alive or request malformed
        _Any:{_Reason, _Stack} when CmdName == alert_notification ->
            ok;
        _Any:{_Reason, _Stack} when CmdName == outbind ->
            ok;
        _Any:{Reason, _Stack} ->
            Ref = make_ref(),
            gen_server:cast(self(), {handle_resp, Pid, Reason, Ref}),
            Ref
    end.


split_options(L) ->
    split_options(L, [], []).

split_options([], Mc, Srv) ->
    {Mc, Srv};
split_options([{addr, _} = H | T], Mc, Srv) ->
    split_options(T, [H | Mc], Srv);
split_options([{port, _} = H | T], Mc, Srv) ->
    split_options(T, [H | Mc], Srv);
split_options([{timers, _} = H | T], Mc, Srv) ->
    split_options(T, [H | Mc], Srv);
split_options([H | T], Mc, Srv) ->
    split_options(T, Mc, [H | Srv]).


%%%-----------------------------------------------------------------------------
%%% SESSION FUNCTIONS
%%%-----------------------------------------------------------------------------
session(Pid, List) ->
    {value, Session} = lists:keysearch(Pid, #session.pid, List),
    Session.


session_closed(Pid, St) ->
    try
        Sss = session(Pid, St#st.sessions),
        erlang:demonitor(Sss#session.ref, [flush]),
        ok = cl_consumer:stop(Sss#session.consumer),
        QueueSrv = cl_queue_tab:lookup(Pid),
        ok = cl_queue_srv:stop(QueueSrv)
    catch
        _Class:_NotSession ->
            ok
    end,
    St#st{sessions = session_delete(Pid, St#st.sessions)}.



session_delete(Pid, List) ->
    lists:keydelete(Pid, #session.pid, List).


session_new(Pid, Opts) ->
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    {ok, QueueSrv} = case proplists:get_value(file_queue, Opts) of
                         undefined ->
                             cl_queue_srv:start_link();
                         File ->
                             cl_queue_srv:start_link(File)
                     end,
    true = cl_queue_tab:insert(Pid, QueueSrv),
    #session{pid = Pid, ref = Ref, rps = proplists:get_value(rps, Opts, ?RPS)}.


session_stop(Sss, Reason) ->
    try
        true = is_process_alive(Sss#session.pid),
        gen_mc_session:stop(Sss#session.pid, Reason)
    catch
        _Class:_NotRunning ->
            ok
    end.


session_update(Sss, List) ->
    lists:keyreplace(Sss#session.pid, #session.pid, List, Sss).

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
-module(gen_mc_session).
-behaviour(gen_fsm).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% BEHAVIOUR EXPORTS
-export([behaviour_info/1]).

%%% START/STOP EXPORTS
-export([start_link/2, stop/1, stop/2]).

%%% SMPP EXPORTS
-export([reply/2,
         alert_notification/2,
         outbind/2,
         deliver_sm/2,
         data_sm/2,
         unbind/1]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/3]).

%%% GEN_FSM STATE EXPORTS
-export([bound_rx/2,
         bound_tx/2,
         bound_trx/2,
         listen/3,
         open/2,
         outbound/2,
         unbound/2]).

%%% HANDLE EXPORTS
-export([handle_event/3, handle_sync_event/4, handle_info/3]).

%%% CODE UPDATE EXPORTS
-export([code_change/4]).

%%% MACROS
-define(BOUND(B),
        if
            B == ?COMMAND_ID_BIND_RECEIVER ->
                bound_rx;
            B == ?COMMAND_ID_BIND_TRANSMITTER ->
                bound_tx;
            B == ?COMMAND_ID_BIND_TRANSCEIVER ->
                bound_trx
        end).

%%% RECORDS
-record(st,
        {mc,
         mod,
         log,
         sequence_number = 0,
         sock,
         sock_ctrl,
         req_tab,
         op_tab,
         congestion_state = 0,
         timers,
         session_init_timer,
         enquire_link_timer,
         inactivity_timer,
         enquire_link_resp_timer}).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR EXPORTS
%%%-----------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{handle_accept, 2},
     {handle_bind, 2},
     {handle_closed, 2},
     {handle_enquire_link, 2},
     {handle_operation, 2},
     {handle_unbind, 2}];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(Mod, Opts) ->
    Mc = proplists:get_value(mc, Opts, self()),
    case proplists:get_value(lsock, Opts) of
        undefined -> start_connect(Mod, Mc, Opts);
        _LSock    -> start_listen(Mod, Mc, Opts)
    end.

stop(FsmRef) ->
    stop(FsmRef, normal).

stop(FsmRef, Reason) ->
    gen_fsm:sync_send_all_state_event(FsmRef, {stop, Reason}, ?ASSERT_TIME).


%%%-----------------------------------------------------------------------------
%%% SMPP EXPORTS
%%%-----------------------------------------------------------------------------
reply(FsmRef, {SeqNum, Reply}) ->
    Event = {reply, {SeqNum, Reply}},
    gen_fsm:sync_send_all_state_event(FsmRef, Event, ?ASSERT_TIME).

alert_notification(FsmRef, Params) ->
    Event = {?COMMAND_ID_ALERT_NOTIFICATION, Params},
    gen_fsm:sync_send_all_state_event(FsmRef, Event, ?ASSERT_TIME).


data_sm(FsmRef, Params) ->
    Event = {?COMMAND_ID_DATA_SM, Params},
    gen_fsm:sync_send_all_state_event(FsmRef, Event, ?ASSERT_TIME).


deliver_sm(FsmRef, Params) ->
    Event = {?COMMAND_ID_DELIVER_SM, Params},
    gen_fsm:sync_send_all_state_event(FsmRef, Event, ?ASSERT_TIME).


outbind(FsmRef, Params) ->
    Event = {?COMMAND_ID_OUTBIND, Params},
    gen_fsm:sync_send_all_state_event(FsmRef, Event, ?ASSERT_TIME).


unbind(FsmRef) ->
    Event = {?COMMAND_ID_UNBIND, []},
    gen_fsm:sync_send_all_state_event(FsmRef, Event, ?ASSERT_TIME).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Mod, Mc, Opts]) ->
    _Ref = erlang:monitor(process, Mc),
    Timers = proplists:get_value(timers, Opts, ?DEFAULT_TIMERS_SMPP),
    Log = proplists:get_value(log, Opts),
    case proplists:get_value(lsock, Opts) of
        undefined ->
            init_open(Mod, Mc, proplists:get_value(sock, Opts), Timers, Log);
        LSock ->
            init_listen(Mod, Mc, LSock, Timers, Log)
    end.


init_open(Mod, Mc, Sock, Tmr, Log) ->
    Self = self(),
    Pid = spawn_link(smpp_session, wait_recv, [Self, Sock, Log]),
    {ok, open, #st{mc = Mc,
                   mod = Mod,
                   log = Log,
                   sock = Sock,
                   sock_ctrl = Pid,
                   req_tab = smpp_req_tab:new(),
                   op_tab = smpp_req_tab:new(),
                   timers = Tmr,
                   session_init_timer =
                       smpp_session:start_timer(Tmr, session_init_timer),
                   enquire_link_timer =
                       smpp_session:start_timer(Tmr, enquire_link_timer)}}.


init_listen(Mod, Mc, LSock, Tmr, Log) ->
    Self = self(),
    Pid = spawn_link(smpp_session, wait_accept, [Self, LSock, Log]),
    {ok, listen, #st{mc = Mc,
                     mod = Mod,
                     log = Log,
                     sock_ctrl = Pid,
                     req_tab = smpp_req_tab:new(),
                     op_tab = smpp_req_tab:new(),
                     timers = Tmr}}.


terminate(_Reason, _Stn, Std) ->
    exit(Std#st.sock_ctrl, kill),
    if Std#st.sock == undefined -> ok; true -> gen_tcp:close(Std#st.sock) end.

%%%-----------------------------------------------------------------------------
%%% ASYNC REQUEST EXPORTS
%%%-----------------------------------------------------------------------------
bound_rx({?COMMAND_ID_UNBIND, _Pdu} = R, St) ->
    case handle_peer_unbind(R, St) of
        true ->
            smpp_session:cancel_timer(St#st.inactivity_timer),
            {next_state, unbound, St};
        false ->
            {next_state, bound_rx, St}
    end;
bound_rx({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_BIND_RECEIVER;
       CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
       CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    esme_ralybnd_resp(R, St#st.sock, St#st.log),
    {next_state, bound_rx, St};
bound_rx({timeout, _Ref, Timer}, St) ->
    case handle_timeout(Timer, St) of
        ok ->
            {next_state, bound_rx, St};
        {error, Reason} ->
            {stop, Reason, St}
    end;
bound_rx(R, St) ->
    esme_rinvbndsts_resp(R, St#st.sock, St#st.log),
    {next_state, bound_rx, St}.


bound_tx({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_DATA_SM;
       CmdId == ?COMMAND_ID_SUBMIT_SM;
       CmdId == ?COMMAND_ID_SUBMIT_MULTI;
       CmdId == ?COMMAND_ID_REPLACE_SM;
       CmdId == ?COMMAND_ID_BROADCAST_SM;
       CmdId == ?COMMAND_ID_QUERY_SM;
       CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_SM ->
    handle_peer_operation(R, St),
    {next_state, bound_tx, St};
bound_tx({?COMMAND_ID_UNBIND, _Pdu} = R, St) ->
    case handle_peer_unbind(R, St) of
        true ->
            smpp_session:cancel_timer(St#st.inactivity_timer),
            {next_state, unbound, St};
        false ->
            {next_state, bound_tx, St}
    end;
bound_tx({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_BIND_RECEIVER;
       CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
       CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    esme_ralybnd_resp(R, St#st.sock, St#st.log),
    {next_state, bound_tx, St};
bound_tx({timeout, _Ref, Timer}, St) ->
    case handle_timeout(Timer, St) of
        ok ->
            {next_state, bound_tx, St};
        {error, Reason} ->
            {stop, Reason, St}
    end;
bound_tx(R, St) ->
    esme_rinvbndsts_resp(R, St#st.sock, St#st.log),
    {next_state, bound_tx, St}.


bound_trx({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_DATA_SM;
       CmdId == ?COMMAND_ID_SUBMIT_SM;
       CmdId == ?COMMAND_ID_SUBMIT_MULTI;
       CmdId == ?COMMAND_ID_REPLACE_SM;
       CmdId == ?COMMAND_ID_BROADCAST_SM;
       CmdId == ?COMMAND_ID_QUERY_SM;
       CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_SM ->
    handle_peer_operation(R, St),
    {next_state, bound_trx, St};
bound_trx({?COMMAND_ID_UNBIND, _Pdu} = R, St) ->
    case handle_peer_unbind(R, St) of
        true ->
            smpp_session:cancel_timer(St#st.inactivity_timer),
            {next_state, unbound, St};
        false ->
            {next_state, bound_trx, St}
    end;
bound_trx({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_BIND_RECEIVER;
       CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
       CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    esme_ralybnd_resp(R, St#st.sock, St#st.log),
    {next_state, bound_trx, St};
bound_trx({timeout, _Ref, Timer}, St) ->
    case handle_timeout(Timer, St) of
        ok ->
            {next_state, bound_trx, St};
        {error, Reason} ->
            {stop, Reason, St}
    end;
bound_trx(R, St) ->
    esme_rinvbndsts_resp(R, St#st.sock, St#st.log),
    {next_state, bound_trx, St}.


listen({accept, Sock, Addr}, _From, St) ->
    case (St#st.mod):handle_accept(St#st.mc, Addr) of
        ok ->
            TI = smpp_session:start_timer(St#st.timers, session_init_timer),
            TE = smpp_session:start_timer(St#st.timers, enquire_link_timer),
            {reply, true, open, St#st{sock = Sock,
                                      session_init_timer = TI,
                                      enquire_link_timer = TE}};
        {error, _Reason} ->
            {reply, false, listen, St}
    end.


open(activate, St) ->
    ok = gen_tcp:controlling_process(St#st.sock, St#st.sock_ctrl),
    St#st.sock_ctrl ! activate,
    {next_state, open, St};
open({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_BIND_RECEIVER;
       CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
       CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    case handle_peer_bind(R, St) of
        true ->
            smpp_session:cancel_timer(St#st.session_init_timer),
            smpp_session:cancel_timer(St#st.inactivity_timer),
            Timer = smpp_session:start_timer(St#st.timers, inactivity_timer),
            {next_state, ?BOUND(CmdId), St#st{inactivity_timer = Timer}};
        false ->
            {next_state, open, St}
    end;
open({timeout, _Ref, Timer}, St) ->
    case handle_timeout(Timer, St) of
        ok ->
            {next_state, open, St};
        {error, Reason} ->
            {stop, Reason, St}
    end;
open(R, St) ->
    esme_rinvbndsts_resp(R, St#st.sock, St#st.log),
    {next_state, open, St}.


outbound({CmdId, _Pdu} = R, St)
  when CmdId == ?COMMAND_ID_BIND_RECEIVER;
       CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
       CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    case handle_peer_bind(R, St) of
        true ->
            smpp_session:cancel_timer(St#st.session_init_timer),
            smpp_session:cancel_timer(St#st.inactivity_timer),
            Timer = smpp_session:start_timer(St#st.timers, inactivity_timer),
            {next_state, ?BOUND(CmdId), St#st{inactivity_timer = Timer}};
        false ->
            {next_state, open, St}
    end;
outbound({timeout, _Ref, Timer}, St) ->
    case handle_timeout(Timer, St) of
        ok ->
            {next_state, outbound, St};
        {error, Reason} ->
            {stop, Reason, St}
    end;
outbound(R, St) ->
    esme_rinvbndsts_resp(R, St#st.sock, St#st.log),
    {next_state, outbound, St}.


unbound({timeout, _Ref, Timer}, St) ->
    case handle_timeout(Timer, St) of
        ok ->
            {next_state, unbound, St};
        {error, Reason} ->
            {stop, Reason, St}
    end;
unbound(R, St) ->
    esme_rinvbndsts_resp(R, St#st.sock, St#st.log),
    {next_state, unbound, St}.

%% Auxiliary function for Event/2 functions.
%%
%% Sends the corresponding response with a ``?ESME_RALYBND`` status.
esme_ralybnd_resp({CmdId, Pdu}, Sock, Log) ->
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    send_response(?RESPONSE(CmdId), ?ESME_RALYBND, SeqNum, [], Sock, Log).

%% Auxiliary function for Event/2 functions.
%%
%% Sends the corresponding response with a ``?ESME_RINVBNDSTS`` status.
esme_rinvbndsts_resp({CmdId, Pdu}, Sock, Log) ->
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    case ?VALID_COMMAND_ID(CmdId) of
        true ->
            RespId = ?RESPONSE(CmdId),
            send_response(RespId, ?ESME_RINVBNDSTS, SeqNum, [], Sock, Log);
        false ->
            RespId = ?COMMAND_ID_GENERIC_NACK,
            send_response(RespId, ?ESME_RINVCMDID, SeqNum, [], Sock, Log)
    end.

%%%-----------------------------------------------------------------------------
%%% HANDLE EXPORTS
%%%-----------------------------------------------------------------------------
handle_event({input, CmdId, _Pdu, _Lapse, _Timestamp}, Stn, Std)
  when CmdId == ?COMMAND_ID_ENQUIRE_LINK_RESP ->
    smpp_session:cancel_timer(Std#st.enquire_link_resp_timer),
    {next_state, Stn, Std};
handle_event({input, CmdId, Pdu, _Lapse, _Timestamp}, Stn, Std)
  when CmdId == ?COMMAND_ID_GENERIC_NACK ->
    smpp_session:cancel_timer(Std#st.enquire_link_resp_timer),  % In case it was set
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    case smpp_req_tab:read(Std#st.req_tab, SeqNum) of
        {ok, {SeqNum, _ReqId, RTimer, Ref}} ->
            smpp_session:cancel_timer(RTimer),
            case smpp_operation:get_value(command_status, Pdu) of
                ?ESME_ROK ->    % Some MCs return ESME_ROK in generic_nack
                    handle_peer_resp({error, ?ESME_RINVCMDID}, Ref, Std);
                Status ->
                    handle_peer_resp({error, Status}, Ref, Std)
            end;
        {error, not_found} ->
            % Do not send anything, might enter a request/response loop
            true
    end,
    {next_state, Stn, Std};
handle_event({input, CmdId, Pdu, _Lapse, _Timestamp}, Stn, Std)
  when ?IS_RESPONSE(CmdId) ->
    smpp_session:cancel_timer(Std#st.enquire_link_resp_timer),  % In case it was set
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    ReqId = ?REQUEST(CmdId),
    case smpp_req_tab:read(Std#st.req_tab, SeqNum) of
        {ok, {SeqNum, ReqId, RTimer, Ref}} ->
            smpp_session:cancel_timer(RTimer),
            case smpp_operation:get_value(command_status, Pdu) of
                ?ESME_ROK when CmdId == ?COMMAND_ID_UNBIND_RESP ->
                    smpp_session:cancel_timer(Std#st.inactivity_timer),
                    handle_peer_resp({ok, Pdu}, Ref, Std),
                    {next_state, unbound, Std};
                ?ESME_ROK ->
                    handle_peer_resp({ok, Pdu}, Ref, Std),
                    {next_state, Stn, Std};
                Status ->
                    handle_peer_resp({error, Status}, Ref, Std),
                    {next_state, Stn, Std}
            end;
        {error, not_found} ->
            Sock = Std#st.sock,
            Log = Std#st.log,
            Nack = ?COMMAND_ID_GENERIC_NACK,
            send_response(Nack, ?ESME_RINVCMDID, SeqNum, [], Sock, Log),
            {next_state, Stn, Std}
    end;
handle_event({input, CmdId, Pdu, _Lapse, _Timestamp}, Stn, Std)
  when CmdId == ?COMMAND_ID_ENQUIRE_LINK ->
    smpp_session:cancel_timer(Std#st.enquire_link_resp_timer),  % In case it was set
    smpp_session:cancel_timer(Std#st.enquire_link_timer),
    ok = (Std#st.mod):handle_enquire_link(Std#st.mc, Pdu),
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    RespId = ?COMMAND_ID_ENQUIRE_LINK_RESP,
    send_response(RespId, ?ESME_ROK, SeqNum, [], Std#st.sock, Std#st.log),
    T = smpp_session:start_timer(Std#st.timers, enquire_link_timer),
    {next_state, Stn, Std#st{enquire_link_timer = T}};
handle_event({input, CmdId, Pdu, Lapse, Timestamp}, Stn, Std) ->
    smpp_session:cancel_timer(Std#st.enquire_link_resp_timer),  % In case it was set
    smpp_session:cancel_timer(Std#st.inactivity_timer),
    smpp_session:cancel_timer(Std#st.enquire_link_timer),
    gen_fsm:send_event(self(), {CmdId, Pdu}),
    TE = smpp_session:start_timer(Std#st.timers, enquire_link_timer),
    TI = smpp_session:start_timer(Std#st.timers, inactivity_timer),
    C = smpp_session:congestion(Std#st.congestion_state, Lapse, Timestamp),
    {next_state, Stn, Std#st{congestion_state = C,
                             enquire_link_timer = TE,
                             inactivity_timer = TI}};
handle_event({error, CmdId, Status, _SeqNum}, _Stn, Std)
  when ?IS_RESPONSE(CmdId) ->
    {stop, {error, Status}, Std};
handle_event({error, CmdId, Status, SeqNum}, Stn, Std) ->
    RespId = case ?VALID_COMMAND_ID(CmdId) of
                 true when CmdId /= ?COMMAND_ID_GENERIC_NACK ->
                     ?RESPONSE(CmdId);
                 _Otherwise ->
                     ?COMMAND_ID_GENERIC_NACK
             end,
    send_response(RespId, Status, SeqNum,[], Std#st.sock, Std#st.log),
    {next_state, Stn, Std};
handle_event(?COMMAND_ID_ENQUIRE_LINK, Stn, Std) ->
    NewStd = send_enquire_link(Std),
    {next_state, Stn, NewStd};
handle_event({sock_error, _Reason}, unbound, Std) ->
    gen_tcp:close(Std#st.sock),
    {stop, normal, Std#st{sock = undefined}};
handle_event({sock_error, Reason}, _Stn, Std) ->
    gen_tcp:close(Std#st.sock),
    (Std#st.mod):handle_closed(Std#st.mc, Reason),
    {stop, normal, Std#st{sock = undefined}};
handle_event({listen_error, Reason}, _Stn, Std) ->
    {stop, Reason, Std}.


handle_info({'DOWN', _Ref, _Type, _Mc, Reason}, _Stn, Std) ->
    {stop, Reason, Std};
handle_info({inet_reply, _, ok}, Stn, Std) ->
    {next_state, Stn, Std};
handle_info({inet_reply, _, Reason}, Stn, Std) ->
    gen_fsm:send_all_state_event(self(), {sock_error, Reason}),
    {next_state, Stn, Std};
handle_info(_Info, Stn, Std) ->
    {next_state, Stn, Std}.


handle_sync_event({stop, Reason}, _From, _Stn, Std) ->
    {stop, Reason, ok, Std};
handle_sync_event({reply, {SeqNum, Reply}}, _From, Stn, Std) ->
    {ok, {SeqNum, CmdId}} = smpp_req_tab:read(Std#st.op_tab, SeqNum),
    RespId = ?RESPONSE(CmdId),
    Sock = Std#st.sock,
    Log = Std#st.log,
    case Reply of
        {ok, PList1} ->
            PList2  = [{congestion_state, Std#st.congestion_state}],
            Params = smpp_operation:merge(PList1, PList2),
            send_response(RespId, ?ESME_ROK, SeqNum, Params, Sock, Log);
        {error, Error} ->
            send_response(RespId, Error, SeqNum, [], Sock, Log)
    end,
    {reply, ok, Stn, Std};
handle_sync_event({?COMMAND_ID_OUTBIND, Params}, From, open, Std) ->
    NewStd = send_request(?COMMAND_ID_OUTBIND, Params, From, Std),
    {next_state, outbound, NewStd};
handle_sync_event({CmdId, Params}, From, Stn, Std) ->
    NewStd = send_request(CmdId, Params, From, Std),
    {next_state, Stn, NewStd}.

%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, Stn, Std, _Extra) ->
    {ok, Stn, Std}.

%%%-----------------------------------------------------------------------------
%%% START FUNCTIONS
%%%-----------------------------------------------------------------------------
start_connect(Mod, Mc, Opts) ->
    case smpp_session:connect(Opts) of
        {ok, Sock} ->
            Args = [Mod, Mc, [{sock, Sock} | Opts]],
            case gen_fsm:start_link(?MODULE, Args, []) of
                {ok, Pid} ->
                    case gen_tcp:controlling_process(Sock, Pid) of
                        ok ->
                            gen_fsm:send_event(Pid, activate),
                            {ok, Pid};
                        CtrlError ->
                            gen_tcp:close(Sock),
                            CtrlError
                        end;
                SessionError ->
                    gen_tcp:close(Sock),
                    SessionError
                end;
            ConnError ->
                ConnError
    end.

start_listen(Mod, Mc, Opts) ->
    gen_fsm:start_link(?MODULE, [Mod, Mc, Opts], []).

%%%-----------------------------------------------------------------------------
%%% HANDLE PEER FUNCTIONS
%%%-----------------------------------------------------------------------------
handle_peer_bind({CmdId, Pdu}, St) ->
    CmdName = ?COMMAND_NAME(CmdId),
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    RespId = ?RESPONSE(CmdId),
    Sock = St#st.sock,
    Log = St#st.log,
    case (St#st.mod):handle_bind(St#st.mc, {CmdName, Pdu}) of
        {ok, Params} ->
            send_response(RespId, ?ESME_ROK, SeqNum, Params, Sock, Log),
            true;
        {error, Error} ->
            send_response(RespId, Error, SeqNum, [], Sock, Log),
            false
    end.


handle_peer_operation({CmdId, Pdu}, St) ->
    CmdName = ?COMMAND_NAME(CmdId),
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    RespId = ?RESPONSE(CmdId),
    Sock = St#st.sock,
    Log = St#st.log,
    case (St#st.mod):handle_operation(St#st.mc, {CmdName, Pdu}) of
        noreply ->
            ok = smpp_req_tab:write(St#st.op_tab, {SeqNum, CmdId}),
            true;
        {ok, PList1} ->
            PList2  = [{congestion_state, St#st.congestion_state}],
            Params = smpp_operation:merge(PList1, PList2),
            send_response(RespId, ?ESME_ROK, SeqNum, Params, Sock, Log),
            true;
        {error, Error} ->
            send_response(RespId, Error, SeqNum, [], Sock, Log),
            false
    end.


handle_peer_resp(Reply, Ref, St) ->
    (St#st.mod):handle_resp(St#st.mc, Reply, Ref).


handle_peer_unbind({?COMMAND_ID_UNBIND, Pdu}, St) ->
    SeqNum = smpp_operation:get_value(sequence_number, Pdu),
    RespId = ?COMMAND_ID_UNBIND_RESP,
    case (St#st.mod):handle_unbind(St#st.mc, Pdu) of
        ok ->
            send_response(RespId, ?ESME_ROK, SeqNum, [], St#st.sock, St#st.log),
            true;
        {error, Error} ->
            send_response(RespId, Error, SeqNum, [],  St#st.sock, St#st.log),
            false
    end.

%%%-----------------------------------------------------------------------------
%%% HANDLE TIMEOUT
%%%-----------------------------------------------------------------------------
handle_timeout({response_timer, SeqNum}, St) ->
    {ok, {SeqNum, CmdId, _, Ref}} = smpp_req_tab:read(St#st.req_tab, SeqNum),
    Status = smpp_operation:request_failure_code(CmdId),
    handle_peer_resp({error, Status}, Ref, St),
    ok;
handle_timeout(enquire_link_timer, _St) ->
    ok = gen_fsm:send_all_state_event(self(), ?COMMAND_ID_ENQUIRE_LINK);
handle_timeout(enquire_link_failure, _St) ->
    {error, {timeout, enquire_link}};
handle_timeout(session_init_timer, _St) ->
    {error, {timeout, session_init_timer}};
handle_timeout(inactivity_timer, _St) ->
    {error, {timeout, inactivity_timer}}.

%%%-----------------------------------------------------------------------------
%%% SEND PDU FUNCTIONS
%%%-----------------------------------------------------------------------------
send_enquire_link(St) ->
    SeqNum = ?INCR_SEQUENCE_NUMBER(St#st.sequence_number),
    Pdu = smpp_operation:new(?COMMAND_ID_ENQUIRE_LINK, SeqNum, []),
    ok = smpp_session:send_pdu(St#st.sock, Pdu, St#st.log),
    ETimer = smpp_session:start_timer(St#st.timers, enquire_link_timer),
    RTimer = smpp_session:start_timer(St#st.timers, enquire_link_failure),
    St#st{sequence_number = SeqNum,
          enquire_link_timer = ETimer,
          enquire_link_resp_timer = RTimer,
          congestion_state = 0}.


send_request(CmdId, Params, From, St)
  when CmdId == ?COMMAND_ID_ALERT_NOTIFICATION;
       CmdId == ?COMMAND_ID_OUTBIND ->
    smpp_session:cancel_timer(St#st.inactivity_timer),
    smpp_session:cancel_timer(St#st.enquire_link_timer),
    SeqNum = ?INCR_SEQUENCE_NUMBER(St#st.sequence_number),
    Pdu = smpp_operation:new(CmdId, SeqNum, Params),
    ok = smpp_session:send_pdu(St#st.sock, Pdu, St#st.log),
    gen_fsm:reply(From, ok),
    St#st{sequence_number = SeqNum,
          enquire_link_timer = smpp_session:start_timer(St#st.timers, enquire_link_timer),
          inactivity_timer = smpp_session:start_timer(St#st.timers, inactivity_timer)};


send_request(CmdId, Params, From, St) ->
    Ref = make_ref(),
    gen_fsm:reply(From, Ref),
    SeqNum = ?INCR_SEQUENCE_NUMBER(St#st.sequence_number),
    Pdu = smpp_operation:new(CmdId, SeqNum, Params),
    case smpp_operation:pack(Pdu) of
        {ok, BinPdu} ->
            smpp_session:cancel_timer(St#st.inactivity_timer),
            smpp_session:cancel_timer(St#st.enquire_link_timer),
            ok = smpp_session:send_pdu(St#st.sock, BinPdu, St#st.log),
            RTimer = smpp_session:start_timer(St#st.timers, {response_timer, SeqNum}),
            ok = smpp_req_tab:write(St#st.req_tab, {SeqNum, CmdId, RTimer, Ref}),
            St#st{sequence_number = SeqNum,
                  enquire_link_timer = smpp_session:start_timer(St#st.timers, enquire_link_timer),
                  inactivity_timer = smpp_session:start_timer(St#st.timers, inactivity_timer)};
        {error, _CmdId, Status, _SeqNum} ->
            handle_peer_resp({error, {command_status, Status}}, Ref, St),
            St
    end.


send_response(CmdId, Status, SeqNum, Params, Sock, Log) ->
    Pdu = smpp_operation:new(CmdId, Status, SeqNum, Params),
    smpp_session:send_pdu(Sock, Pdu, Log).

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
-module(gen_esme).
-behaviour(gen_server).
-behaviour(gen_esme_session).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% BEHAVIOUR EXPORTS
-export([behaviour_info/1]).

%%% START/STOP EXPORTS
-export([start/3, start/4, start_link/3, start_link/4]).

%%% SERVER EXPORTS
-export([call/2, call/3, cast/2, reply/2]).

%%% CONNECT EXPORTS
-export([listen/2, open/3, close/1]).

%%% SMPP EXPORTS
-export([bind_receiver/3,
         bind_transceiver/3,
         bind_transmitter/3,
         broadcast_sm/3,
         broadcast_sm/4,
         cancel_broadcast_sm/3,
         cancel_broadcast_sm/4,
         cancel_sm/3,
         cancel_sm/4,
         data_sm/3,
         data_sm/4,
         query_broadcast_sm/3,
         query_broadcast_sm/4,
         query_sm/3,
         query_sm/4,
         replace_sm/3,
         replace_sm/4,
         submit_multi/3,
         submit_multi/4,
         submit_sm/3,
         submit_sm/4,
         unbind/2]).

%%% QUEUE EXPORTS
-export([queue_broadcast_sm/3,
         queue_broadcast_sm/4,
         queue_cancel_broadcast_sm/3,
         queue_cancel_broadcast_sm/4,
         queue_cancel_sm/3,
         queue_cancel_sm/4,
         queue_data_sm/3,
         queue_data_sm/4,
         queue_len/1,
         queue_out/1,
         queue_out/2,
         queue_out_r/1,
         queue_out_r/2,
         queue_query_broadcast_sm/3,
         queue_query_broadcast_sm/4,
         queue_query_sm/3,
         queue_query_sm/4,
         queue_replace_sm/3,
         queue_replace_sm/4,
         queue_submit_multi/3,
         queue_submit_multi/4,
         queue_submit_sm/3,
         queue_submit_sm/4]).

%%% LOG EXPORTS
-export([add_log_handler/3, delete_log_handler/3, swap_log_handler/3]).

%%% RPS EXPORTS
-export([pause/1, resume/1, rps/1, rps_avg/1, rps_max/1, rps_max/2]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE CHANGE EXPORTS
-export([code_change/3]).

%%% INTERNAL GEN_ESME_SESSION EXPORTS
-export([handle_accept/2,
         handle_alert_notification/2,
         handle_closed/2,
         handle_enquire_link/2,
         handle_operation/2,
         handle_outbind/2,
         handle_resp/3,
         handle_unbind/2]).

%%% MACROS
-define(PRIORITY, 10).
-define(RPS, 1000).
-define(SECOND, 1000).

%%% RECORDS
-record(st, {mod, mod_st, ref, session, consumer, rps, log}).

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
     {handle_accept, 3},
     {handle_alert_notification, 2},
     {handle_closed, 2},
     {handle_data_sm, 3},
     {handle_deliver_sm, 3},
     {handle_outbind, 2},
     {handle_req, 4},
     {handle_resp, 3},
     {handle_unbind, 3}];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(Module, Args, Opts) ->
    {EsmeOpts, SrvOpts} = split_options(Opts),
    gen_server:start(?MODULE, {Module, Args, EsmeOpts}, SrvOpts).


start(SrvName, Module, Args, Opts) ->
    {EsmeOpts, SrvOpts} = split_options(Opts),
    gen_server:start(SrvName, ?MODULE, {Module, Args, EsmeOpts}, SrvOpts).


start_link(Module, Args, Opts) ->
    {EsmeOpts, SrvOpts} = split_options(Opts),
    gen_server:start_link(?MODULE, {Module, Args, EsmeOpts}, SrvOpts).


start_link(SrvName, Module, Args, Opts) ->
    {EsmeOpts, SrvOpts} = split_options(Opts),
    gen_server:start_link(SrvName, ?MODULE, {Module, Args, EsmeOpts}, SrvOpts).

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
listen(SrvRef, Opts) ->
    Pid = ref_to_pid(SrvRef),
    case smpp_session:listen(Opts) of
        {ok, LSock} ->
            ok = gen_tcp:controlling_process(LSock, Pid),
            Timers = proplists:get_value(timers, Opts, ?DEFAULT_TIMERS_SMPP),
            ListenOpts = [{lsock, LSock}, {timers, Timers}],
            gen_server:call(Pid, {start_session, ListenOpts}, ?ASSERT_TIME);
        Error ->
            Error
    end.


open(SrvRef, Addr, Opts) ->
    Pid = ref_to_pid(SrvRef),
    gen_server:call(Pid, {start_session, [{addr, Addr} | Opts]}, ?ASSERT_TIME).


close(SrvRef) ->
    gen_server:cast(SrvRef, close).


%%%-----------------------------------------------------------------------------
%%% SMPP EXPORTS
%%%-----------------------------------------------------------------------------
bind_receiver(SrvRef, Params, Args) ->
    gen_server:cast(SrvRef, {{bind_receiver, Params}, Args}).


bind_transceiver(SrvRef, Params, Args) ->
    gen_server:cast(SrvRef, {{bind_transceiver, Params}, Args}).


bind_transmitter(SrvRef, Params, Args) ->
    gen_server:cast(SrvRef, {{bind_transmitter, Params}, Args}).


broadcast_sm(SrvRef, Params, Args) ->
    broadcast_sm(SrvRef, Params, Args, ?ASSERT_TIME).

broadcast_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{broadcast_sm, Params}, Args}, Timeout).


cancel_broadcast_sm(SrvRef, Params, Args) ->
    cancel_broadcast_sm(SrvRef, Params, Args, ?ASSERT_TIME).

cancel_broadcast_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{cancel_broadcast_sm, Params}, Args}, Timeout).


cancel_sm(SrvRef, Params, Args) ->
    cancel_sm(SrvRef, Params, Args, ?ASSERT_TIME).

cancel_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{cancel_sm, Params}, Args}, Timeout).


data_sm(SrvRef, Params, Args) ->
    data_sm(SrvRef, Params, Args, ?ASSERT_TIME).

data_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{data_sm, Params}, Args}, Timeout).


query_broadcast_sm(SrvRef, Params, Args) ->
    query_broadcast_sm(SrvRef, Params, Args, ?ASSERT_TIME).

query_broadcast_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{query_broadcast_sm, Params}, Args}, Timeout).


query_sm(SrvRef, Params, Args) ->
    query_sm(SrvRef, Params, Args, ?ASSERT_TIME).

query_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{query_sm, Params}, Args}, Timeout).


replace_sm(SrvRef, Params, Args) ->
    replace_sm(SrvRef, Params, Args, ?ASSERT_TIME).

replace_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{replace_sm, Params}, Args}, Timeout).


submit_multi(SrvRef, Params, Args) ->
    submit_multi(SrvRef, Params, Args, ?ASSERT_TIME).

submit_multi(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{submit_multi, Params}, Args}, Timeout).


submit_sm(SrvRef, Params, Args) ->
    submit_sm(SrvRef, Params, Args, ?ASSERT_TIME).

submit_sm(SrvRef, Params, Args, Timeout) ->
    gen_server:call(SrvRef, {{submit_sm, Params}, Args}, Timeout).


unbind(SrvRef, Args) ->
    gen_server:cast(SrvRef, {{unbind, []}, Args}).

%%%-----------------------------------------------------------------------------
%%% QUEUE EXPORTS
%%%-----------------------------------------------------------------------------
queue_broadcast_sm(SrvRef, Params, Args) ->
    queue_broadcast_sm(SrvRef, Params, Args, ?PRIORITY).

queue_broadcast_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {broadcast_sm, Params}, Args, Priority).


queue_cancel_broadcast_sm(SrvRef, Params, Args) ->
    queue_cancel_broadcast_sm(SrvRef, Params, Args, ?PRIORITY).

queue_cancel_broadcast_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {cancel_broadcast_sm, Params}, Args, Priority).


queue_cancel_sm(SrvRef, Params, Args) ->
    queue_cancel_sm(SrvRef, Params, Args, ?PRIORITY).

queue_cancel_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {cancel_sm, Params}, Args, Priority).


queue_data_sm(SrvRef, Params, Args) ->
    queue_data_sm(SrvRef, Params, Args, ?PRIORITY).

queue_data_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {data_sm, Params}, Args, Priority).


queue_len(SrvRef) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:len(QueueSrv).


queue_out(SrvRef) ->
    queue_out(SrvRef, 1).

queue_out(SrvRef, Num) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:out(QueueSrv, Num).


queue_out_r(SrvRef) ->
    queue_out_r(SrvRef, 1).

queue_out_r(SrvRef, Num) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:out_r(QueueSrv, Num).


queue_query_broadcast_sm(SrvRef, Params, Args) ->
    queue_query_broadcast_sm(SrvRef, Params, Args, ?PRIORITY).

queue_query_broadcast_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {query_broadcast_sm, Params}, Args, Priority).


queue_query_sm(SrvRef, Params, Args) ->
    queue_query_sm(SrvRef, Params, Args, ?PRIORITY).

queue_query_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {query_sm, Params}, Args, Priority).


queue_replace_sm(SrvRef, Params, Args) ->
    queue_replace_sm(SrvRef, Params, Args, ?PRIORITY).

queue_replace_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {replace_sm, Params}, Args, Priority).


queue_submit_multi(SrvRef, Params, Args) ->
    queue_submit_multi(SrvRef, Params, Args, ?PRIORITY).

queue_submit_multi(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {submit_multi, Params}, Args, Priority).


queue_submit_sm(SrvRef, Params, Args) ->
    queue_submit_sm(SrvRef, Params, Args, ?PRIORITY).

queue_submit_sm(SrvRef, Params, Args, Priority) ->
    queue(SrvRef, {submit_sm, Params}, Args, Priority).

%%%-----------------------------------------------------------------------------
%%% LOG EXPORTS
%%%-----------------------------------------------------------------------------
add_log_handler(SrvRef, Handler, Args) ->
    gen_server:call(SrvRef, {add_log_handler, Handler, Args}, infinity).


delete_log_handler(SrvRef, Handler, Args) ->
    gen_server:call(SrvRef, {delete_log_handler, Handler, Args}, infinity).


swap_log_handler(SrvRef, Handler1, Handler2) ->
    gen_server:call(SrvRef, {swap_log_handler, Handler1, Handler2}, infinity).

%%%-----------------------------------------------------------------------------
%%% RPS EXPORTS
%%%-----------------------------------------------------------------------------
pause(SrvRef) ->
    gen_server:call(SrvRef, pause, infinity).


resume(SrvRef) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:count_reset(QueueSrv),
    gen_server:cast(SrvRef, resume).


rps(SrvRef) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:rps(QueueSrv).


rps_avg(SrvRef) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:rps_avg(QueueSrv).


rps_max(SrvRef) ->
    gen_server:call(SrvRef, rps_max, infinity).


rps_max(SrvRef, Rps) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
    cl_queue_srv:count_reset(QueueSrv),
    gen_server:cast(SrvRef, {rps_max, Rps}).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init({Mod, Args, Opts}) ->
    {ok, Log} = smpp_log_mgr:start_link(),
    {ok, QueueSrv} = case proplists:get_value(file_queue, Opts) of
                         undefined ->
                             cl_queue_srv:start_link();
                         File ->
                             cl_queue_srv:start_link(File)
                     end,
    true = cl_queue_tab:insert(QueueSrv),
    St = #st{mod = Mod, rps = proplists:get_value(rps, Opts, ?RPS), log = Log},
    pack((St#st.mod):init(Args), St).


terminate(Reason, St) ->
    (St#st.mod):terminate(Reason, St#st.mod_st).

%%%-----------------------------------------------------------------------------
%%% HANDLE EXPORTS
%%%-----------------------------------------------------------------------------
handle_call({call, Req}, From, St) ->
    pack((St#st.mod):handle_call(Req, From, St#st.mod_st), St);
handle_call({start_session, Opts}, _From, St) ->
    case gen_esme_session:start_link(?MODULE,  [{log, St#st.log} | Opts]) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            unlink(Pid),
            {reply, ok, St#st{ref = Ref, session = Pid}};
        Error ->
            {reply, Error, St}
    end;
handle_call({{CmdName, Params} = Req, Args}, _From, St) ->
    Ref = req_send(St#st.session, CmdName, Params),
    case pack((St#st.mod):handle_req(Req, Args, Ref, St#st.mod_st), St) of
        {noreply, NewSt} ->
            {reply, ok, NewSt};
        {noreply, NewSt, Timeout} ->
            {reply, ok, NewSt, Timeout};
        {stop, Reason, NewSt} ->
            {stop, ok, Reason, NewSt}
    end;
handle_call(pause, _From, St) ->
    try
        true = is_process_alive(St#st.consumer),
        ok = cl_consumer:pause(St#st.consumer)
    catch
        error:_NotAlive ->
            ok
    end,
    {reply, ok, St};
handle_call({add_log_handler, Handler, Args}, _From, St) ->
    {reply, smpp_log_mgr:add_handler(St#st.log, Handler, Args), St};
handle_call({delete_log_handler, Handler, Args}, _From, St) ->
    {reply, smpp_log_mgr:delete_handler(St#st.log, Handler, Args), St};
handle_call({swap_log_handler, Handler1, Handler2}, _From, St) ->
    {reply, smpp_log_mgr:swap_handler(St#st.log, Handler1, Handler2), St};
handle_call(rps_max, _From, St) ->
    {reply, St#st.rps, St};
handle_call({handle_accept, Addr}, From, St) ->
    pack((St#st.mod):handle_accept(Addr, From, St#st.mod_st), St);
handle_call({handle_data_sm, Pdu}, From, St) ->
    pack((St#st.mod):handle_data_sm(Pdu, From, St#st.mod_st), St);
handle_call({handle_deliver_sm, Pdu}, From, St) ->
    pack((St#st.mod):handle_deliver_sm(Pdu, From, St#st.mod_st), St);
handle_call({handle_unbind, Pdu}, From, St) ->
    pack((St#st.mod):handle_unbind(Pdu, From, St#st.mod_st), St);
handle_call({handle_enquire_link, _Pdu}, _From, St) ->
    {reply, ok, St}.


handle_cast({cast, Req}, St) ->
    pack((St#st.mod):handle_cast(Req, St#st.mod_st), St);
handle_cast(close, St) ->
    try
        true = is_process_alive(St#st.consumer),
        ok = cl_consumer:pause(St#st.consumer)
    catch
        error:_ConsumerNotAlive ->
            ok
    end,
    try
        true = is_process_alive(St#st.session),
        ok = gen_esme_session:stop(St#st.session)
    catch
        error:_SessionNotAlive ->
            ok
    end,
    {noreply, St};
handle_cast({{CmdName, Params} = Req, Args}, St) ->
    Ref = req_send(St#st.session, CmdName, Params),
    pack((St#st.mod):handle_req(Req, Args, Ref, St#st.mod_st), St);
handle_cast({handle_closed, Reason}, St) ->
    NewSt = session_closed(St),
    pack((NewSt#st.mod):handle_closed(Reason, NewSt#st.mod_st), NewSt);
handle_cast({handle_outbind, Pdu}, St) ->
    pack((St#st.mod):handle_outbind(Pdu, St#st.mod_st), St);
handle_cast(resume, St) ->
    try
        true = is_process_alive(St#st.consumer),
        ok = cl_consumer:resume(St#st.consumer),
        {noreply, St}
    catch
        error:_NotAlive ->
            QueueSrv = cl_queue_tab:lookup(),
            Self = self(),
            ReqFun = fun(X) -> gen_server:call(Self, X, ?ASSERT_TIME) end,
            {ok, Pid} = cl_consumer:start_link(QueueSrv, ReqFun, St#st.rps),
            {noreply, St#st{consumer = Pid}}
    end;
handle_cast({rps_max, Rps}, St) ->
    ok = cl_consumer:rps(St#st.consumer, Rps),
    {noreply, St#st{rps = Rps}};
handle_cast({handle_resp, Resp, Ref}, St) ->
    pack((St#st.mod):handle_resp(Resp, Ref, St#st.mod_st), St);
handle_cast({handle_alert_notification, Pdu}, St) ->
    pack((St#st.mod):handle_alert_notification(Pdu, St#st.mod_st), St).


handle_info({'DOWN', _Ref, _Type, Pid, Reason}, #st{session = Pid} = St) ->
    NewSt = session_closed(St),
    pack((NewSt#st.mod):handle_closed(Reason, NewSt#st.mod_st), NewSt);
handle_info(Info, St) ->
    pack((St#st.mod):handle_info(Info, St#st.mod_st), St).

%%%-----------------------------------------------------------------------------
%%% CODE CHANGE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(OldVsn, St, Extra) ->
    pack((St#st.mod):code_change(OldVsn, St#st.mod_st, Extra), St).

%%%-----------------------------------------------------------------------------
%%% INTERNAL GEN_ESME_SESSION EXPORTS
%%%-----------------------------------------------------------------------------
handle_accept(SrvRef, Addr) ->
    gen_server:call(SrvRef, {handle_accept, Addr}, ?ASSERT_TIME).


handle_alert_notification(SrvRef, Pdu) ->
    gen_server:cast(SrvRef, {handle_alert_notification, Pdu}).


handle_closed(SrvRef, Reason) ->
    gen_server:cast(SrvRef, {handle_closed, Reason}).


handle_enquire_link(SrvRef, Pdu) ->
    gen_server:call(SrvRef, {handle_enquire_link, Pdu}, ?ASSERT_TIME).


handle_operation(SrvRef, {data_sm, Pdu}) ->
    gen_server:call(SrvRef, {handle_data_sm, Pdu}, ?ASSERT_TIME);
handle_operation(SrvRef, {deliver_sm, Pdu}) ->
    gen_server:call(SrvRef, {handle_deliver_sm, Pdu}, ?ASSERT_TIME).


handle_outbind(SrvRef, Pdu) ->
    gen_server:cast(SrvRef, {handle_outbind, Pdu}).


handle_resp(SrvRef, Resp, Ref) ->
    gen_server:cast(SrvRef, {handle_resp, Resp, Ref}).


handle_unbind(SrvRef, Pdu) ->
    gen_server:call(SrvRef, {handle_unbind, Pdu}, ?ASSERT_TIME).

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


queue(SrvRef, Req, Args, Priority) ->
    QueueSrv = cl_queue_tab:lookup(ref_to_pid(SrvRef)),
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
    try   % Need to protect ourselves to make sure that handle_closed is called
        if
            CmdName == bind_receiver ->
                gen_esme_session:bind_receiver(Pid, Params);
            CmdName == bind_transceiver ->
                gen_esme_session:bind_transceiver(Pid, Params);
            CmdName == bind_transmitter ->
                gen_esme_session:bind_transmitter(Pid, Params);
            CmdName == broadcast_sm ->
                gen_esme_session:broadcast_sm(Pid, Params);
            CmdName == cancel_broadcast_sm ->
                gen_esme_session:cancel_broadcast_sm(Pid, Params);
            CmdName == cancel_sm ->
                gen_esme_session:cancel_sm(Pid, Params);
            CmdName == data_sm ->
                gen_esme_session:data_sm(Pid, Params);
            CmdName == query_broadcast_sm ->
                gen_esme_session:query_broadcast_sm(Pid, Params);
            CmdName == query_sm ->
                gen_esme_session:query_sm(Pid, Params);
            CmdName == replace_sm ->
                gen_esme_session:replace_sm(Pid, Params);
            CmdName == submit_multi ->
                gen_esme_session:submit_multi(Pid, Params);
            CmdName == submit_sm ->
                gen_esme_session:submit_sm(Pid, Params);
            CmdName == unbind ->
                gen_esme_session:unbind(Pid)
        end
    catch
        _Any:{Reason, _Stack} -> % Session not alive or request malformed
            Ref = make_ref(),
            handle_resp(self(), {error, Reason}, Ref),
            Ref
    end.


session_closed(St) ->
    try
        erlang:demonitor(St#st.ref, [flush]),
        true = is_process_alive(St#st.consumer),
        ok = cl_consumer:stop(St#st.consumer)
    catch
        error:_NotAlive ->
            ok
    end,
    St#st{ref = undefined, session = undefined, consumer = undefined}.


split_options(L) ->
    split_options(L, [], []).

split_options([], Esme, Srv) ->
    {Esme, Srv};
split_options([{rps, _} = H | T], Esme, Srv) ->
    split_options(T, [H | Esme], Srv);
split_options([{file_queue, _} = H | T], Esme, Srv) ->
    split_options(T, [H | Esme], Srv);
split_options([H | T], Esme, Srv) ->
    split_options(T, Esme, [H | Srv]).

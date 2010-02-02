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

%%% IMPORTANT NOTE:
%%%
%%% This module was implemented for test coverage purporses only, please
%%% refer to doc/examples for a much cleaner MC example to start with.
-module(test_mc).
-behaviour(gen_mc).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

%%% START/STOP EXPORTS
-export([start_link/0, start_link/1, stop/0]).

%%% SMPP EXPORTS
-export([alert_notification/1, data_sm/1, deliver_sm/1, outbind/3, unbind/0]).

%%% QUEUE EXPORTS
-export([queue_data_sm/1, queue_deliver_sm/1]).

%%% RPS EXPORTS
-export([resume/0, pause/0, rps/0, rps_max/0, rps_max/1]).

%%% STATUS EXPORTS
-export([failure/0, reset/0, silent/1, success/0]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE MESSAGES EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% MC EXPORTS
-export([handle_accept/4,
         handle_bind_receiver/4,
         handle_bind_transceiver/4,
         handle_bind_transmitter/4,
         handle_broadcast_sm/4,
         handle_cancel_broadcast_sm/4,
         handle_cancel_sm/4,
         handle_closed/3,
         handle_data_sm/4,
         handle_query_broadcast_sm/4,
         handle_query_sm/4,
         handle_replace_sm/4,
         handle_req/5,
         handle_resp/4,
         handle_submit_multi/4,
         handle_submit_sm/4,
         handle_unbind/4]).

%%% MACROS
-define(ADDR, {127, 0, 0, 1}).
-define(PORT, ?DEFAULT_SMPP_PORT).

%%% RECORDS
-record(st,
        {silent,
         msg_id = 0,
         rxs = [],
         txs = [],
         failure = 0,
         success = 0}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    start_link(true).

start_link(Silent) ->
    Opts = [{addr, ?ADDR}, {port, ?PORT}],
    gen_mc:start_link({local, ?MODULE}, ?MODULE, [Silent], Opts).


stop() ->
    F = fun(Pid) -> gen_mc:unbind(?MODULE, Pid, []) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)),
    lists:foreach(F, gen_mc:call(?MODULE, txs)),
    timer:sleep(1000),
    gen_mc:call(?MODULE, stop).

%%%-----------------------------------------------------------------------------
%%% SMPP EXPORTS
%%%-----------------------------------------------------------------------------
alert_notification(Params) ->
    F = fun(X) -> gen_mc:alert_notification(?MODULE, X, Params) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).


data_sm(Params) ->
    F = fun(X) -> gen_mc:data_sm(?MODULE, X, Params, []) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).


deliver_sm(Params) ->
    F = fun(X) -> gen_mc:deliver_sm(?MODULE, X, Params, []) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).


outbind(Addr, Port, Params) ->
    case gen_mc:outbind(?MODULE, Addr, [{port, Port}], Params) of
        {ok, _Pid} ->
            ok;
        Error ->
            Error
    end.


unbind() ->
    gen_mc:unbind(?MODULE).

%%%-----------------------------------------------------------------------------
%%% QUEUE EXPORTS
%%%-----------------------------------------------------------------------------
queue_data_sm(Params) ->
    F = fun(X) -> gen_mc:queue_data_sm(?MODULE, X, Params, []) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).


queue_deliver_sm(Params) ->
    F = fun(X) -> gen_mc:queue_deliver_sm(?MODULE, X, Params, []) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).

%%%-----------------------------------------------------------------------------
%%% RPS EXPORTS
%%%-----------------------------------------------------------------------------
resume() ->
    F = fun(X) -> gen_mc:resume(?MODULE, X) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).


pause() ->
    F = fun(X) -> gen_mc:pause(?MODULE, X) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).


rps() ->
    [gen_mc:rps(?MODULE, X) || X <- gen_mc:call(?MODULE, rxs)].


rps_max() ->
    [gen_mc:rps_max(?MODULE, X) || X <- gen_mc:call(?MODULE, rxs)].


rps_max(RpsMax) ->
    F = fun(X) -> gen_mc:rps_max(?MODULE, X, RpsMax) end,
    lists:foreach(F, gen_mc:call(?MODULE, rxs)).

%%%-----------------------------------------------------------------------------
%%% STATUS EXPORTS
%%%-----------------------------------------------------------------------------
failure() ->
    gen_mc:call(?MODULE, failure, infinity).


reset() ->
    gen_mc:cast(?MODULE, reset).


silent(Silent) ->
    gen_mc:cast(?MODULE, {silent, Silent}).


success() ->
    gen_mc:call(?MODULE, success, infinity).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Silent]) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    {ok, #st{silent = Silent}}.


terminate(_Reason, _St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(rxs, _From, St) ->
    {reply, St#st.rxs, St};
handle_call(txs, _From, St) ->
    {reply, St#st.txs, St};
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(failure, _From, St) ->
    {reply, St#st.failure, St};
handle_call(success, _From, St) ->
    {reply, St#st.success, St}.


handle_cast(reset, St) ->
    {noreply, St#st{failure = 0, success = 0}};
handle_cast({silent, Silent}, St) ->
    {noreply, St#st{silent = Silent}}.


handle_info(_Info, St) ->
    {noreply, St}.

%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%-----------------------------------------------------------------------------
%%% MC EXPORTS
%%%-----------------------------------------------------------------------------
handle_accept(_Pid, Addr, _From, St) ->
    Ip = format_ip(Addr),
    case random:uniform(100) of
        N when N < 25 ->
            format(St#st.silent, "~nConnection from ~s refused~n", [Ip]),
            {reply, {error, refused}, St};
        _N ->
            format(St#st.silent, "~nAccepted connection from ~s~n", [Ip]),
            {reply, {ok, []}, St}
    end.


handle_bind_receiver(Pid, Pdu, _From, St) ->
    case bind_resp(Pid, Pdu, St) of
        {ok, _Params} = Reply ->
            {reply, Reply, St#st{rxs = [Pid | St#st.rxs]}};
        Error ->
            {reply, Error, St}
    end.


handle_bind_transceiver(Pid, Pdu, _From, St) ->
    case bind_resp(Pid, Pdu, St) of
        {ok, _Params} = Reply ->
            {reply, Reply, St#st{rxs = [Pid | St#st.rxs],
                                 txs = [Pid | St#st.txs]}};
        Error ->
            {reply, Error, St}
    end.


handle_bind_transmitter(Pid, Pdu, _From, St) ->
    {reply, bind_resp(Pid, Pdu, St), St}.


handle_broadcast_sm(_Pid, _Pdu, _From, St) ->
    Params = [{message_id, integer_to_list(St#st.msg_id)}],
    {reply, {ok, Params}, St#st{msg_id = St#st.msg_id + 1}}.


handle_cancel_broadcast_sm(_Pid, _Pdu, _From, St) ->
    {reply, {ok, []}, St}.


handle_cancel_sm(_Pid, _Pdu, _From, St) ->
    {reply, {ok, []}, St}.


handle_closed(Pid, Reason, St) ->
    format(St#st.silent, "Session ~p closed with reason ~p~n", [Pid, Reason]),
    {noreply, St#st{rxs = lists:delete(Pid, St#st.rxs),
                    txs = lists:delete(Pid, St#st.txs)}}.


handle_data_sm(_Pid, _Pdu, _From, St) when (St#st.msg_id rem 10) == 0 ->
    {reply, {error, ?ESME_RINVNUMMSGS}, St#st{msg_id = St#st.msg_id + 1}};
handle_data_sm(_Pid, _Pdu, _From, St) ->
    Params = [{message_id, integer_to_list(St#st.msg_id)}],
    {reply, {ok, Params}, St#st{msg_id = St#st.msg_id + 1}}.


handle_query_broadcast_sm(_Pid, Pdu, _From, St) ->
    BcastArea = smpp_base:broadcast_area(0, "area_name"),
    Params = [{message_id, smpp_operation:get_value(message_id, Pdu)},
              {message_state, ?MESSAGE_STATE_DELIVERED},
              {broadcast_area_identifier, [BcastArea]},
              {broadcast_area_success, [100]}],
    {reply, {ok, Params}, St}.


handle_query_sm(_Pid, Pdu, _From, St) ->
    Params = [{message_id, smpp_operation:get_value(message_id, Pdu)},
              {final_date, "090423195134104+"},
              {message_state, ?MESSAGE_STATE_DELIVERED},
              {error_code, 0}],
    {reply, {ok, Params}, St}.


handle_replace_sm(_Pid, _Pdu, _From, St) ->
    {reply, {ok, []}, St}.


handle_req(_Pid, _Req, _Args, _Ref, St) ->
    {noreply, St}.


handle_resp(_Pid, _Resp, _Ref, St) ->
    {noreply, St}.


handle_submit_multi(Pid, Pdu, _From, St) ->
    format(St#st.silent, "Session: ~p~nReceived: ~p~n", [Pid, Pdu]),
    case random_submit_status() of
        ?ESME_ROK ->
            MsgId = integer_to_list(St#st.msg_id),
            format(St#st.silent, "Message ID: ~s~n", [MsgId]),
            Params = [{message_id, MsgId}, {unsuccess_sme, []}],
            {reply, {ok, Params}, St#st{msg_id = St#st.msg_id + 1,
                                        success = St#st.success + 1}};
        Status ->
            Reason = smpp_error:format(Status),
            format(St#st.silent, "Error: ~s~n", [Reason]),
            {reply, {error, Status}, St#st{failure = St#st.failure + 1}}
    end.


handle_submit_sm(Pid, Pdu, _From, St) ->
    format(St#st.silent, "Session: ~p~nReceived: ~p~n", [Pid, Pdu]),
    case random_submit_status() of
        ?ESME_ROK ->
            MsgId = integer_to_list(St#st.msg_id),
            format(St#st.silent, "Message ID: ~s~n", [MsgId]),
            Params = [{message_id, MsgId}],
            {reply, {ok, Params}, St#st{msg_id = St#st.msg_id + 1,
                                        success = St#st.success + 1}};
        Status ->
            Reason = smpp_error:format(Status),
            format(St#st.silent, "Error: ~s~n", [Reason]),
            {reply, {error, Status}, St#st{failure = St#st.failure + 1}}
    end.


handle_unbind(Pid, _Pdu, _From, St) ->
    {reply, ok, St#st{rxs = lists:delete(Pid, St#st.rxs),
                      txs = lists:delete(Pid, St#st.txs)}}.

%%%-----------------------------------------------------------------------------
%%% FORMAT FUNCTIONS
%%%-----------------------------------------------------------------------------
format(false, Msg, Args) ->
    ct:print(Msg, Args);
format(true, _Msg, _Args) ->
    ok.


format_ip({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
bind_resp(Pid, Pdu, St) ->
    SystemId = smpp_operation:get_value(system_id, Pdu),
    Password = smpp_operation:get_value(password, Pdu),
    format(St#st.silent, "Bind request from: ~s/~s~n", [SystemId, Password]),
    case random_bind_status() of
        ?ESME_ROK ->
            format(St#st.silent, "System Id: ~s~n", [?MODULE_STRING]),
            Params = [{system_id, ?MODULE_STRING},
                      {sc_interface_version, ?SMPP_VERSION_5_0}],
            {ok, Params};
        Status ->
            Reason = smpp_error:format(Status),
            format(St#st.silent, "Rejected: ~s~n", [Reason]),
            gen_mc:close(?MODULE, Pid),
            {error, Status}
    end.


random_bind_status() ->
    case random:uniform(100) of
        N when N =< 25 ->
            ?ESME_RBINDFAIL;
        _N ->
            ?ESME_ROK
    end.


random_submit_status() ->
    case random:uniform(100) of
        N when N =< 10 ->
            ?ESME_RSYSERR;
        N when N =< 20 ->
            ?ESME_RMSGQFUL;
        N when N =< 30 ->
            ?ESME_RSUBMITFAIL;
        N when N =< 40 ->
            ?ESME_RTHROTTLED;
        _N ->
            ?ESME_ROK
    end.

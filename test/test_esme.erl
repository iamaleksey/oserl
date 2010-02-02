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
%%% refer to doc/examples for a much cleaner ESME example to start with.
-module(test_esme).
-behaviour(gen_esme).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

%%% START/STOP EXPORTS
-export([start_link/1, start_link/2, stop/0]).

%%% SEND EXPORTS
-export([send/4]).

%%% RPS EXPORTS
-export([pause/0, resume/0, rps_avg/0, rps/0, rps_max/0, rps_max/1]).

%%% STATUS EXPORTS
-export([bound/0, failure/0, recv/0, reset/0, silent/1, success/0]).

%%% SMPP EXPORTS
-export([broadcast_sm/1,
         cancel_broadcast_sm/1,
         cancel_sm/1,
         data_sm/1,
         query_broadcast_sm/1,
         query_sm/1,
         replace_sm/1,
         submit_multi/1,
         submit_sm/1,
         submit_sm/2]).

%%% QUEUE EXPORTS
-export([queue_broadcast_sm/1,
         queue_broadcast_sm/2,
         queue_cancel_broadcast_sm/1,
         queue_cancel_broadcast_sm/2,
         queue_cancel_sm/1,
         queue_cancel_sm/2,
         queue_data_sm/1,
         queue_data_sm/2,
         queue_len/0,
         queue_out/0,
         queue_out/1,
         queue_query_broadcast_sm/1,
         queue_query_broadcast_sm/2,
         queue_query_sm/1,
         queue_query_sm/2,
         queue_replace_sm/1,
         queue_replace_sm/2,
         queue_submit_multi/1,
         queue_submit_multi/2,
         queue_submit_sm/1,
         queue_submit_sm/2]).

%%% LOG EXPORTS
-export([add_log_handler/1, delete_log_handler/1, swap_log_handler/2]).

%%% DEBUG EXPORTS
-export([show_warnings/0]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE MESSAGES EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% HANDLE ESME EXPORTS
-export([handle_accept/3,
         handle_alert_notification/2,
         handle_closed/2,
         handle_data_sm/3,
         handle_deliver_sm/3,
         handle_outbind/2,
         handle_req/4,
         handle_resp/3,
         handle_unbind/3]).

%%% CONNECT EXPORTS
-export([connect/2]).

%%% MACROS
-define(ADDR, {127, 0, 0, 1}).
-define(BIND_PARAMS,
        [{system_id, "oserl"}, {password, "oserl"}, {system_type, "oserl"}]).
-define(HIGH, 0).
-define(LOW, 10).
-define(PORT, ?DEFAULT_SMPP_PORT).
-define(PORT_LISTEN, 9999).
-define(RETRIES, 3).
-define(RETRY_TIME, 5000).

%%% RECORDS
-record(st,
        {mode,
         silent,
         bound,
         bind_req,
         reqs = [],
         msg_id = 1,
         failure = 0,
         recv = 0,
         success = 0}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(Mode) ->
    start_link(Mode, true).

start_link(Mode, Silent) ->
    Opts = [{rps, 1}, {queue_file, "./sample_esme.dqueue"}],
    gen_esme:start_link({local, ?MODULE}, ?MODULE, [Mode, Silent], Opts).


stop() ->
    gen_esme:call(?MODULE, stop).

%%%-----------------------------------------------------------------------------
%%% SEND EXPORTS
%%%-----------------------------------------------------------------------------
send(Src, Dest, Msg, Opts) ->
    Params = params(Src, Dest),
    Priority = proplists:get_value(priority, Opts, ?LOW),
    submit(Msg, Params, [], Priority).

%%%-----------------------------------------------------------------------------
%%% RPS EXPORTS
%%%-----------------------------------------------------------------------------
pause() ->
    gen_esme:pause(?MODULE).


resume() ->
    gen_esme:resume(?MODULE).


rps_avg() ->
    gen_esme:rps_avg(?MODULE).


rps() ->
    gen_esme:rps(?MODULE).


rps_max() ->
    gen_esme:rps_max(?MODULE).


rps_max(Rps) ->
    gen_esme:rps_max(?MODULE, Rps).

%%%-----------------------------------------------------------------------------
%%% STATUS EXPORTS
%%%-----------------------------------------------------------------------------
bound() ->
    gen_esme:call(?MODULE, bound, infinity).


failure() ->
    gen_esme:call(?MODULE, failure, infinity).


recv() ->
    gen_esme:call(?MODULE, recv, infinity).


reset() ->
    gen_esme:cast(?MODULE, reset).


silent(Silent) ->
    gen_esme:cast(?MODULE, {silent, Silent}).


success() ->
    gen_esme:call(?MODULE, success, infinity).


show_warnings() ->
    Pid = whereis(?MODULE),
    spawn_link(fun() -> show_warnings_loop(Pid) end).

show_warnings_loop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    show_warnings_loop(Pid, Ref).

show_warnings_loop(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, Info} ->
            ct:print("sample_esme exited: ~p~n", [Info]);
        _Other ->
            show_warnings_loop(Pid, Ref)
    after 200 ->
            case process_info(Pid, message_queue_len) of
                {message_queue_len, Len} when Len > 100 ->
                    ct:print("WARNING: ~p messages in the inbox~n", [Len]);
                _Other ->
                    ok
            end,
            show_warnings_loop(Pid, Ref)
    end.

%%%-----------------------------------------------------------------------------
%%% SMPP EXPORTS
%%%-----------------------------------------------------------------------------
broadcast_sm(Params) ->
    gen_esme:broadcast_sm(?MODULE, Params, []).


cancel_broadcast_sm(Params) ->
    gen_esme:cancel_broadcast_sm(?MODULE, Params, []).


cancel_sm(Params) ->
    gen_esme:cancel_sm(?MODULE, Params, []).


data_sm(Params) ->
    gen_esme:data_sm(?MODULE, Params, []).


query_broadcast_sm(Params) ->
    gen_esme:query_broadcast_sm(?MODULE, Params, []).


query_sm(Params) ->
    gen_esme:query_sm(?MODULE, Params, []).


replace_sm(Params) ->
    gen_esme:replace_sm(?MODULE, Params, []).


submit_multi(Params) ->
    case proplists:get_value(short_message, Params) of
        Sm when is_list(Sm), length(Sm) > ?SM_MAX_SIZE ->
            RefNum = smpp_ref_num:next(?MODULE),
            F = fun(X) -> gen_esme:submit_multi(?MODULE, X, []) end,
            lists:foreach(F, smpp_sm:split(Params, RefNum, udh));
        _DoNotFragment ->
            gen_esme:submit_multi(?MODULE, Params, [])
    end.


submit_sm(Params) ->
    case proplists:get_value(short_message, Params) of
        Sm when is_list(Sm), length(Sm) > ?SM_MAX_SIZE ->
            RefNum = smpp_ref_num:next(?MODULE),
            F = fun(X) -> gen_esme:submit_sm(?MODULE, X, []) end,
            lists:foreach(F, smpp_sm:split(Params, RefNum, udh));
        _DoNotFragment ->
            gen_esme:submit_sm(?MODULE, Params, [])
    end.

submit_sm(_Params, 0) ->
    ok;
submit_sm(Params, N) ->
    submit_sm(Params),
    submit_sm(Params, N -1).

%%%-----------------------------------------------------------------------------
%%% QUEUE EXPORTS
%%%-----------------------------------------------------------------------------
queue_broadcast_sm(Params) ->
    gen_esme:queue_broadcast_sm(?MODULE, Params, []).

queue_broadcast_sm(Params, Priority) ->
    gen_esme:queue_broadcast_sm(?MODULE, Params, [], Priority).


queue_cancel_broadcast_sm(Params) ->
    gen_esme:queue_cancel_broadcast_sm(?MODULE, Params, []).

queue_cancel_broadcast_sm(Params, Priority) ->
    gen_esme:queue_cancel_broadcast_sm(?MODULE, Params, [], Priority).


queue_cancel_sm(Params) ->
    gen_esme:queue_cancel_sm(?MODULE, Params, []).

queue_cancel_sm(Params, Priority) ->
    gen_esme:queue_cancel_sm(?MODULE, Params, [], Priority).


queue_data_sm(Params) ->
    gen_esme:queue_data_sm(?MODULE, Params, []).

queue_data_sm(Params, Priority) ->
    gen_esme:queue_data_sm(?MODULE, Params, [], Priority).


queue_len() ->
    gen_esme:queue_len(?MODULE).


queue_out() ->
    gen_esme:queue_out(?MODULE).

queue_out(Num) ->
    gen_esme:queue_out(?MODULE, Num).


queue_query_broadcast_sm(Params) ->
    gen_esme:queue_query_broadcast_sm(?MODULE, Params, []).

queue_query_broadcast_sm(Params, Priority) ->
    gen_esme:queue_query_broadcast_sm(?MODULE, Params, [], Priority).


queue_query_sm(Params) ->
    gen_esme:queue_query_sm(?MODULE, Params, []).

queue_query_sm(Params, Priority) ->
    gen_esme:queue_query_sm(?MODULE, Params, [], Priority).


queue_replace_sm(Params) ->
    gen_esme:queue_replace_sm(?MODULE, Params, []).

queue_replace_sm(Params, Priority) ->
    gen_esme:queue_replace_sm(?MODULE, Params, [], Priority).


queue_submit_multi(Params) ->
    case proplists:get_value(short_message, Params) of
        Sm when is_list(Sm), length(Sm) > ?SM_MAX_SIZE ->
            RefNum = smpp_ref_num:next(?MODULE),
            F = fun(X) -> gen_esme:queue_submit_multi(?MODULE, X, []) end,
            lists:foreach(F, smpp_sm:split(Params, RefNum, udh));
        _DoNotFragment ->
            gen_esme:queue_submit_multi(?MODULE, Params, [])
    end.

queue_submit_multi(Params, Priority) ->
    case proplists:get_value(short_message, Params) of
        Sm when is_list(Sm), length(Sm) > ?SM_MAX_SIZE ->
            RefNum = smpp_ref_num:next(?MODULE),
            F = fun(X) ->
                        gen_esme:queue_submit_multi(?MODULE, X, [], Priority)
                end,
            lists:foreach(F, smpp_sm:split(Params, RefNum, udh));
        _DoNotFragment ->
            gen_esme:queue_submit_multi(?MODULE, Params, [], Priority)
    end.


queue_submit_sm(Params) ->
    case proplists:get_value(short_message, Params) of
        Sm when is_list(Sm), length(Sm) > ?SM_MAX_SIZE ->
            RefNum = smpp_ref_num:next(?MODULE),
            F = fun(X) -> gen_esme:queue_submit_sm(?MODULE, X, []) end,
            lists:foreach(F, smpp_sm:split(Params, RefNum, udh));
        _DoNotFragment ->
            gen_esme:queue_submit_sm(?MODULE, Params, [])
    end.

queue_submit_sm(Params, Priority) ->
    case proplists:get_value(short_message, Params) of
        Sm when is_list(Sm), length(Sm) > ?SM_MAX_SIZE ->
            RefNum = smpp_ref_num:next(?MODULE),
            F = fun(X) ->
                        gen_esme:queue_submit_sm(?MODULE, X, [], Priority)
                end,
            lists:foreach(F, smpp_sm:split(Params, RefNum, udh));
        _DoNotFragment ->
            gen_esme:queue_submit_sm(?MODULE, Params, [], Priority)
    end.

%%%-----------------------------------------------------------------------------
%%% LOG EXPORTS
%%%-----------------------------------------------------------------------------
add_log_handler(Handler) ->
    gen_esme:add_log_handler(?MODULE, Handler, []).


delete_log_handler(Handler) ->
    gen_esme:delete_log_handler(?MODULE, Handler, []).


swap_log_handler(Handler1, Handler2) ->
    gen_esme:swap_log_handler(?MODULE, {Handler1, []}, {Handler2, []}).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Mode, Silent]) ->
    spawn_link(fun() -> connect(Mode, Silent) end),
    {ok, #st{bound = false, mode = Mode, silent = Silent}}.


terminate(_Reason, _St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(stop, _From, St) ->
    Self = self(),
    spawn(fun() -> gen_esme:unbind(Self, []) end),
    {stop, normal, ok, St};
handle_call(bound, _From, St) ->
    {reply, St#st.bound, St};
handle_call(failure, _From, St) ->
    {reply, St#st.failure, St};
handle_call(recv, _From, St) ->
    {reply, St#st.recv, St};
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
%%% HANDLE ESME EXPORTS
%%%-----------------------------------------------------------------------------
handle_accept(Addr, From, St) ->
    erlang:error(function_clause, [Addr, From, St]).


handle_alert_notification(Pdu, St) ->
    format(St#st.silent, "Alert notification: ~p~n", [Pdu]),
    {noreply, St}.


handle_closed(Reason, St) ->
    format(St#st.silent, "Session closed due to reason: ~p~n", [Reason]),
    Connect = fun() ->
                      timer:sleep(?RETRY_TIME),
                      connect(St#st.mode, St#st.silent)
              end,
    spawn_link(Connect),
    {noreply, St#st{bound = false}}.


handle_data_sm(Pdu, _From, St) when (St#st.msg_id rem 10) == 0 ->
    format(St#st.silent, "Data SM: ~p~n", [Pdu]),
    {reply, {error, ?ESME_RINVNUMMSGS}, St#st{msg_id = St#st.msg_id + 1}};
handle_data_sm(Pdu, _From, St) ->
    format(St#st.silent, "Data SM: ~p~n", [Pdu]),
    Reply = {ok, [{message_id, integer_to_list(St#st.msg_id)}]},
    {reply, Reply, St#st{msg_id = St#st.msg_id + 1, recv = St#st.recv + 1}}.


handle_deliver_sm(Pdu, _From, St) when (St#st.msg_id rem 10) == 0 ->
    format(St#st.silent, "Deliver SM: ~p~n", [Pdu]),
    {reply, {error, ?ESME_RTHROTTLED}, St#st{msg_id = St#st.msg_id + 1}};
handle_deliver_sm(Pdu, _From, St) ->
    format(St#st.silent, "Deliver SM: ~p~n", [Pdu]),
    Reply = {ok, [{message_id, integer_to_list(St#st.msg_id)}]},
    {reply, Reply, St#st{msg_id = St#st.msg_id + 1, recv = St#st.recv + 1}}.


handle_outbind(Pdu, St) ->
    erlang:error(function_clause, [Pdu, St]).


handle_req({bind_transmitter, _Params}, _Args, Ref, St) ->
    {noreply, St#st{bind_req = Ref}};
handle_req({bind_receiver, _Params}, _Args, Ref, St) ->
    {noreply, St#st{bind_req = Ref}};
handle_req({bind_transceiver, _Params}, _Args, Ref, St) ->
    {noreply, St#st{bind_req = Ref}};
handle_req(Req, Args, Ref, St) ->
    {noreply, St#st{reqs = [{Req, Args, Ref} | St#st.reqs]}}.


handle_resp({ok, PduResp}, Ref, #st{bind_req = Ref} = St) ->
    SystemId = smpp_operation:get_value(system_id, PduResp),
    format(St#st.silent, "Bound to ~s~n", [SystemId]),
    Retry = fun({{_, Params}, Args, _}) -> submit(Params, Args, ?HIGH) end,
    lists:foreach(Retry, St#st.reqs),
    gen_esme:resume(?MODULE),
    {noreply, St#st{bound = true, reqs = []}};
handle_resp({error, {command_status, Status}}, Ref, #st{bind_req = Ref} = St) ->
    Reason = smpp_error:format(Status),
    format(St#st.silent, "Bind error: ~s~n", [Reason]),
    gen_esme:close(?MODULE),
    {noreply, St};
handle_resp({error, Reason}, Ref, #st{bind_req = Ref} = St) ->
    format(St#st.silent, "Could not bind: ~p~n", [Reason]),
    {noreply, St};
handle_resp({ok, PduResp}, Ref, St) ->
    {{Req, _Args, Ref}, Reqs} = cl_lists:keyextract(Ref, 3, St#st.reqs),
    format(St#st.silent, "~nRequest: ~p~nResponse: ~p~n", [Req, PduResp]),
    case smpp_operation:get_value(congestion_state, PduResp) of
        Congestion when Congestion > 90 ->
            format(St#st.silent, "Peer congested: ~p~n", [Congestion]);
        _Other ->
            ok
    end,
    {noreply, St#st{reqs = Reqs, success = St#st.success + 1}};
handle_resp({error, {command_status, Status}}, Ref, St) ->
    {{Req, Args, Ref}, Reqs} = cl_lists:keyextract(Ref, 3, St#st.reqs),
    Reason = smpp_error:format(Status),
    format(St#st.silent, "~nRequest: ~p~nStatus: ~s~n", [Req, Reason]),
    case proplists:get_value(retries, Args, ?RETRIES) of
        0 ->
            format(St#st.silent, "Max retries exceeded: ~p~n", [Req]),
            {noreply, St#st{reqs = Reqs, failure = St#st.failure + 1}};
        N ->
            {_CmdName, Params} = Req,
            NewArgs = [{retries, N - 1} | proplists:delete(retries, Args)],
            submit(Params, NewArgs, ?HIGH),
            {noreply, St#st{reqs = Reqs}}
    end;
handle_resp({error, Reason}, Ref, St) ->
    {{Req, Args, Ref}, Reqs} = cl_lists:keyextract(Ref, 3, St#st.reqs),
    format(St#st.silent, "Request: ~p~nFailure: ~p~n", [Req, Reason]),
    {_CmdName, Params} = Req,
    submit(Params, Args, ?LOW),
    {noreply, St#st{reqs = Reqs}}.



handle_unbind(_Pdu, _From, St) ->
    {reply, ok, St}.

%%%-----------------------------------------------------------------------------
%%% CONNECT EXPORTS
%%%-----------------------------------------------------------------------------
bind(tx) ->
    gen_esme:bind_transmitter(?MODULE, ?BIND_PARAMS, []);
bind(rx) ->
    gen_esme:bind_receiver(?MODULE, ?BIND_PARAMS, []);
bind(trx) ->
    gen_esme:bind_transceiver(?MODULE, ?BIND_PARAMS, []).


connect(outbind, Silent) ->
    format(Silent, "~nListening at ~p for an outbind~n", [?PORT_LISTEN]),
    gen_esme:listen(?MODULE, [{addr, ?ADDR}, {port, ?PORT_LISTEN}]);
connect(Mode, Silent) ->
    Peer = format_peer(?ADDR, ?PORT),
    case gen_esme:open(?MODULE, ?ADDR, [{port, ?PORT}]) of
        ok ->
            format(Silent, "~nConnected to ~s~n", [Peer]),
            bind(Mode);
        {error, Reason} ->
            format(Silent, "Cannot connect to ~s~nError: ~p~n", [Peer, Reason]),
            timer:sleep(?RETRY_TIME),
            ?MODULE:connect(Mode, Silent)
    end.

%%%-----------------------------------------------------------------------------
%%% FORMAT FUNCTIONS
%%%-----------------------------------------------------------------------------
format(false, Msg, Args) ->
    ct:print(Msg, Args);
format(true, _Msg, _Args) ->
    ok.


format_peer({A, B, C, D}, Port) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
destination(Dest) ->
    case cl_lists:is_deep(Dest) of
        false ->
            {destination_addr, Dest};
        true ->
            {dest_address, lists:map(fun(X) -> dest_addr_sme(X) end, Dest)}
    end.


dest_addr_sme(DestAddr) ->
    smpp_base:dest_address_sme(
      ?DEST_FLAG_SME, ?TON_INTERNATIONAL, ?NPI_ISDN, DestAddr).


is_multi(Params) ->
    proplists:get_value(destination_addr, Params) == undefined.


params(Src, Dest) ->
    [destination(Dest),
     {source_addr_ton, ?TON_ALPHANUMERIC},
     {source_addr, Src}].


submit(Msg, Params, Args, Priority) when length(Msg) > ?SM_MAX_SIZE ->
    RefNum = smpp_ref_num:next(?MODULE),
    L = smpp_sm:split([{short_message, Msg} | Params], RefNum, udh),
    lists:foreach(fun(X) -> submit(X, Args, Priority) end, L);
submit(Msg, Params, Args, Priority) ->
    submit([{short_message, Msg} | Params], Args, Priority).


submit(Params, Args, Priority) ->
    case is_multi(Params) of
        true ->
            gen_esme:queue_submit_multi(?MODULE, Params, Args, Priority);
        false ->
            gen_esme:queue_submit_sm(?MODULE, Params, Args, Priority)
    end.

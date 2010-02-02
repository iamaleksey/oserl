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
-module(mc_skel).
-behaviour(gen_mc).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").
-include_lib("oserl/include/smpp_globals.hrl").

%%% START/STOP EXPORTS
-export([start_link/2]).

%%% EXTERNAL EXPORTS
-export([]).

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

%%% RECORDS
-record(st, {}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(Args, Opts) ->
    gen_mc:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([]) ->
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(Req, From, St) ->
    erlang:error(function_clause, [Req, From, St]).


handle_cast(Req, St) ->
    erlang:error(function_clause, [Req, St]).


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
handle_accept(Pid, Addr, From, St) ->
    erlang:error(function_clause, [Pid, Addr, From, St]).


handle_bind_receiver(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_bind_transceiver(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_bind_transmitter(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_broadcast_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_cancel_broadcast_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_cancel_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_closed(_Pid, _Reason, St) ->
    {noreply, St}.


handle_data_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_query_broadcast_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_query_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_replace_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_req(_Pid, _Req, _Args, _Ref, St) ->
    {noreply, St}.


handle_resp(_Pid, _Resp, _Ref, St) ->
    {noreply, St}.


handle_submit_multi(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_submit_sm(Pid, Pdu, From, St) ->
    erlang:error(function_clause, [Pid, Pdu, From, St]).


handle_unbind(_Pid, _Pdu, _From, St) ->
    {reply, ok, St}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------

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
-module(esme_skel).
-behaviour(gen_esme).

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

%%% MACROS

%%% RECORDS
-record(st, {}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(Args, Params) ->
    gen_esme:start_link({local, ?MODULE}, ?MODULE, Args, Params).

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
%%% HANDLE ESME EXPORTS
%%%-----------------------------------------------------------------------------
handle_accept(Addr, From, St) ->
    erlang:error(function_clause, [Addr, From, St]).


handle_alert_notification(Pdu, St) ->
    erlang:error(function_clause, [Pdu, St]).


handle_closed(Reason, St) ->
    {stop, Reason, St}.


handle_data_sm(Pdu, From, St) ->
    erlang:error(function_clause, [Pdu, From, St]).


handle_deliver_sm(Pdu, From, St) ->
    erlang:error(function_clause, [Pdu, From, St]).


handle_outbind(Pdu, St) ->
    erlang:error(function_clause, [Pdu, St]).


handle_req(_Req, _Args, _Ref, St) ->
    {noreply, St}.


handle_resp(_Resp, _Ref, St) ->
    {noreply, St}.


handle_unbind(_Pdu, _From, St) ->
    {reply, ok, St}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------

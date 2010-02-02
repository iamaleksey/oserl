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
-module(smpp_session).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% EXTERNAL EXPORTS
-export([congestion/3, connect/1, listen/1]).

%%% MACROS
-define(CONNECT_OPTS, [binary, {packet, 0}, {active, false}]).
-define(CONNECT_TIME, 30000).
-define(LISTEN_OPTS(Addr),
        if
            Addr == undefined ->
                [binary, {packet, 0}, {active, false}, {reuseaddr, true}];
            true ->
                [binary,
                 {packet, 0},
                 {active, false},
                 {reuseaddr, true},
                 {ip, Addr}]
        end).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
%% Computes the congestion state.
%%
%% - CongestionSt: Current ``congestion_state`` value.
%% - WaitTime: Are the microseconds waiting for the PDU.
%% - Timestamp: Represents the moment when the PDU was received.
%%
%% The time since ``Timestamp`` is the PDU dispatching time.  If
%% this value equals the ``WaitTime`` (i.e. ``DispatchTime/WaitTime = 1``),
%% then we shall assume optimum load (value 85).  Having this in mind the
%% instant congestion state value is calculated.  Notice this value cannot be
%% greater than 99.
congestion(CongestionSt, WaitTime, Timestamp) ->
    case (timer:now_diff(now(), Timestamp) div (WaitTime + 1)) * 85 of
        Val when Val < 1 ->
            0;
        Val when Val > 99 ->  % Out of bounds
            ((19 * CongestionSt) + 99) div 20;
        Val ->
            ((19 * CongestionSt) + Val) div 20
    end.


connect(Opts) ->
    case proplists:get_value(sock, Opts, undefined) of
        undefined ->
            Addr = proplists:get_value(addr, Opts),
            Port = proplists:get_value(port, Opts, ?DEFAULT_SMPP_PORT),
            gen_tcp:connect(Addr, Port, ?CONNECT_OPTS, ?CONNECT_TIME);
        Sock ->
            case inet:setopts(Sock, ?CONNECT_OPTS) of
                ok ->
                    {ok, Sock};
                Error ->
                    Error
            end
    end.


listen(Opts) ->
    case proplists:get_value(lsock, Opts, undefined) of
        undefined ->
            Addr = proplists:get_value(addr, Opts, default_addr()),
            Port = proplists:get_value(port, Opts, ?DEFAULT_SMPP_PORT),
            gen_tcp:listen(Port, ?LISTEN_OPTS(Addr));
        LSock ->
            Addr = proplists:get_value(addr, Opts, default_addr()),
            case inet:setopts(LSock, ?LISTEN_OPTS(Addr)) of
                ok ->
                    {ok, LSock};
                Error ->
                    Error
            end
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
default_addr() ->
    {ok, Host} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(Host, inet),
    Addr.

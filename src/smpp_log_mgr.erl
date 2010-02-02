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
-module(smpp_log_mgr).

%%% START/STOP EXPORTS
-export([start/0, start_link/0, stop/1]).

%%% HANDLER EXPORTS
-export([add_handler/3, delete_handler/3, swap_handler/3]).

%%% EVENT EXPORTS
-export([pdu/2]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start() ->
    gen_event:start().


start_link() ->
    gen_event:start_link().


stop(Pid) ->
    gen_event:stop(Pid).

%%%-----------------------------------------------------------------------------
%%% HANDLER EXPORTS
%%%-----------------------------------------------------------------------------
add_handler(Pid, Handler, Args) ->
    gen_event:add_handler(Pid, Handler, Args).


delete_handler(Pid, Handler, Args) ->
    gen_event:delete_handler(Pid, Handler, Args).


swap_handler(Pid, HandlerArgs1, HandlerArgs2) ->
    gen_event:swap_handler(Pid, HandlerArgs1, HandlerArgs2).

%%%-----------------------------------------------------------------------------
%%% EVENT EXPORTS
%%%-----------------------------------------------------------------------------
pdu(Pid, IoList) when is_list(IoList) ->
    pdu(Pid, concat_binary(IoList));
pdu(Pid, Pdu) when is_binary(Pdu) ->
    gen_event:notify(Pid, {pdu, Pdu}).

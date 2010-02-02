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
-module(smpp_tty_log_hlr).
-behaviour(gen_event).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE EVENT EXPORTS
-export([handle_call/2, handle_event/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% MACROS
-define(FILTER,
        fun(BinPdu) ->
                case smpp_pdu_syntax:command_status(BinPdu) of
                    ?ESME_ROK ->
                        false;
                    _Status ->
                        true
                end
        end).
-define(FORMAT,
        fun(BinPdu) ->
                {ok, Pdu} = smpp_operation:unpack(BinPdu),
                smpp_operation:to_list(Pdu)
        end).

%%% RECORDS
-record(st, {file, filter, format, io_device}).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init({NewArgs, OldArgs}) ->
    init(merge_args(NewArgs, OldArgs));
init(Args) ->
    Filter = proplists:get_value(filter, Args, ?FILTER),
    Format = proplists:get_value(format, Args, ?FORMAT),
    case proplists:get_value(file, Args, undefined) of
        undefined ->
            {ok, #st{filter = Filter, format = Format}};
        File ->
            case file:open(File, [append]) of
                {ok, IoDevice} ->
                    {ok, #st{file = File,
                             filter = Filter,
                             format = Format,
                             io_device = IoDevice}};
                Error ->
                    Error
            end
    end.


terminate(Reason, St) when St#st.io_device /= undefined ->
    ok = file:close(St#st.io_device),
    terminate(Reason, St#st{io_device = undefined});
terminate(_Reason, St) ->
    init_args(St).

%%%-----------------------------------------------------------------------------
%%% HANDLE EVENT EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(Req, St) ->
    erlang:error(function_clause, [Req, St]).


handle_event({pdu, BinPdu}, St) ->
    case catch (St#st.filter)(BinPdu) of
        true when St#st.io_device == undefined ->
            io:format("~p~n", [(St#st.format)(BinPdu)]);
        true ->
            io:format(St#st.io_device, "~p~n", [(St#st.format)(BinPdu)]);
        _Otherwise ->
            ok
    end,
    {ok, St};
handle_event(_Other, St) ->
    {ok, St}.


handle_info(_Info, St) ->
    {ok, St}.

%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
init_args(St) ->
    [{file, St#st.file}, {filter, St#st.filter}, {format, St#st.format}].


merge_args(Args1, Args2) ->
    cl_lists:ukeymerge(1, lists:keysort(1, Args1), lists:keysort(1, Args2)).

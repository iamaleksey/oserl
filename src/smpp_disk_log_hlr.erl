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
-module(smpp_disk_log_hlr).
-behaviour(gen_event).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% EXPORTS
-export([count/3, match/3, match/4]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE EVENTS EXPORTS
-export([handle_call/2, handle_event/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% MACROS
-define(FILTER,
        fun(BinPdu) ->
                case smpp_pdu_syntax:command_id(BinPdu) of
                    ?COMMAND_ID_ENQUIRE_LINK ->
                        false;
                    ?COMMAND_ID_ENQUIRE_LINK_RESP ->
                        false;
                    _CmdId ->
                        true
                end
        end).
-define(FORMAT, fun(BinPdu) -> BinPdu end).
-define(NAME, smpp_log).
-define(SIZE, {10485760, 20}). % 10M x 20 = 200M

%%% RECORDS
-record(st, {name, file, filter, format, size}).

%%%-----------------------------------------------------------------------------
%%% EXPORTS
%%%-----------------------------------------------------------------------------
count(Name, Date, Pred) ->
    count(Name, Date, Pred, start, 0).

count(Name, Date, Pred, Continuation, Count) ->
    case match(Name, Date, Pred, Continuation) of
        eof ->
            Count;
        {Continuation2, List} ->
            count(Name, Date, Pred, Continuation2, Count + length(List));
        {Continuation2, List, _Badbytes} ->
            count(Name, Date, Pred, Continuation2, Count + length(List))
    end.


match(Name, Date, Pred) ->
    match(Name, Date, Pred, start).

match(Name, Date, Pred, Continuation) ->
    F = fun({_, Pdu}) when Date == any ->
                Pred(Pdu);
           ({Now, Pdu}) ->
                T = calendar:now_to_local_time(Now),
                case Date of
                    {from, T1} when T >= T1 ->
                        Pred(Pdu);
                    {until, T1} when T =< T1 ->
                        Pred(Pdu);
                    {lapse, T1, T2} when T >= T1, T =< T2 ->
                        Pred(Pdu);
                    _Otherwise ->
                        false
                end
        end,
    case disk_log:chunk(Name, Continuation) of
        {Continuation2, List} ->
            {Continuation2, [X || X <- List, F(X)]};
        {Continuation2, List, Badbytes} ->
            {Continuation2, [X || X <- List, F(X)], Badbytes};
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init({NewArgs, OldArgs}) ->
    init(merge_args(NewArgs, OldArgs));
init(Args) ->
    St = init_st(Args),
    ArgL = [{name, St#st.name},
            {file, St#st.file},
            {size, St#st.size},
            {type, wrap}],
    case disk_log:open(ArgL) of
        {error, Reason} ->
            {error, Reason};
        _OK ->
            {ok, St}
    end.


terminate(_Reason, St) ->
    ok = disk_log:close(St#st.name),
    init_args(St).

%%%-----------------------------------------------------------------------------
%%% HANDLE EVENTS EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(Req, St) ->
    erlang:error(function_clause, [Req, St]).


handle_event({pdu, Pdu}, St) ->
    case catch (St#st.filter)(Pdu) of
        true ->
            disk_log:alog(St#st.name, {now(), (St#st.format)(Pdu)});
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
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
init_args(St) ->
    [{name, St#st.name},
     {filter, St#st.filter},
     {file, St#st.file},
     {format, St#st.format},
     {size, St#st.size}].


init_st(Args) ->
    #st{name = proplists:get_value(name, Args, ?NAME),
        file = proplists:get_value(file, Args, atom_to_list(?NAME)),
        filter = proplists:get_value(filter, Args, ?FILTER),
        format = proplists:get_value(format, Args, ?FORMAT),
        size = proplists:get_value(size, Args, ?SIZE)}.


merge_args(Args1, Args2) ->
    cl_lists:ukeymerge(1, lists:keysort(1, Args1), lists:keysort(1, Args2)).



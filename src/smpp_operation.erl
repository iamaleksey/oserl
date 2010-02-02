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
-module(smpp_operation).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").
-include("smpp_pdu.hrl").

%%% PARAM EXPORTS
-export([get_value/2, get_value/3, merge/2]).

%%% PDU EXPORTS
-export([new/3, new/4, request_failure_code/1, to_list/1]).

%%% PACK/UNPACK EXPORTS
-export([pack/1, unpack/1]).

%%%-----------------------------------------------------------------------------
%%% PARAM EXPORTS
%%%-----------------------------------------------------------------------------
get_value(command_id, {CmdId, _Status, _SeqNum, _Body}) ->
    CmdId;
get_value(command_status, {_CmdId, Status, _SeqNum, _Body}) ->
    Status;
get_value(sequence_number, {_CmdId, _Status, SeqNum, _Body}) ->
    SeqNum;
get_value(Name, {_CmdId, _Status, _SeqNum, Body}) ->
    proplists:get_value(Name, Body, undefined).

get_value(Name, Pdu, Default) ->
    case get_value(Name, Pdu) of
        undefined ->
            Default;
        Value ->
            Value
    end.


merge(Params1, Params2) ->
    cl_lists:ukeymerge(1, lists:keysort(1,Params1), lists:keysort(1,Params2)).

%%%-----------------------------------------------------------------------------
%%% PDU EXPORTS
%%%-----------------------------------------------------------------------------
new(CmdId, SeqNum, InitParams) ->
    new(CmdId, ?ESME_ROK, SeqNum, InitParams).

new(CmdId, Status, SeqNum, InitParams) ->
    smpp_pdu_syntax:new_pdu(CmdId, Status, SeqNum, InitParams).


request_failure_code(?COMMAND_ID_BIND_TRANSMITTER) ->
    ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BIND_RECEIVER) ->
    ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BIND_TRANSCEIVER) ->
    ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BROADCAST_SM) ->
    ?ESME_RBCASTFAIL;
request_failure_code(?COMMAND_ID_CANCEL_BROADCAST_SM) ->
    ?ESME_RBCASTCANCELFAIL;
request_failure_code(?COMMAND_ID_CANCEL_SM) ->
    ?ESME_RCANCELFAIL;
request_failure_code(?COMMAND_ID_SUBMIT_SM) ->
    ?ESME_RSUBMITFAIL;
request_failure_code(?COMMAND_ID_DATA_SM) ->
    ?ESME_RSUBMITFAIL;
request_failure_code(?COMMAND_ID_SUBMIT_MULTI) ->
    ?ESME_RSUBMITFAIL;
request_failure_code(?COMMAND_ID_QUERY_BROADCAST_SM) ->
    ?ESME_RBCASTQUERYFAIL;
request_failure_code(?COMMAND_ID_QUERY_SM) ->
    ?ESME_RQUERYFAIL;
request_failure_code(?COMMAND_ID_REPLACE_SM) ->
    ?ESME_RREPLACEFAIL;
request_failure_code(_CmdId) ->
    ?ESME_RUNKNOWNERR.


to_list({CmdId, Status, SeqNum, Body}) ->
    [{command_id, CmdId},
     {command_status, Status},
     {sequence_number, SeqNum} | Body].

%%%-----------------------------------------------------------------------------
%%% PACK/UNPACK EXPORTS
%%%-----------------------------------------------------------------------------
pack(Pdu) ->
    case smpp_pdu_syntax:command_id(Pdu) of
        ?COMMAND_ID_DELIVER_SM ->
            smpp_pdu_syntax:pack(Pdu, ?DELIVER_SM);
        ?COMMAND_ID_DELIVER_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?DELIVER_SM_RESP);
        ?COMMAND_ID_SUBMIT_SM ->
            smpp_pdu_syntax:pack(Pdu, ?SUBMIT_SM);
        ?COMMAND_ID_SUBMIT_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?SUBMIT_SM_RESP);
        ?COMMAND_ID_SUBMIT_MULTI ->
            smpp_pdu_syntax:pack(Pdu, ?SUBMIT_MULTI);
        ?COMMAND_ID_SUBMIT_MULTI_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?SUBMIT_MULTI_RESP);
        ?COMMAND_ID_DATA_SM ->
            smpp_pdu_syntax:pack(Pdu, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?DATA_SM_RESP);
        ?COMMAND_ID_ALERT_NOTIFICATION ->
            smpp_pdu_syntax:pack(Pdu, ?ALERT_NOTIFICATION);
        ?COMMAND_ID_BROADCAST_SM ->
            smpp_pdu_syntax:pack(Pdu, ?BROADCAST_SM);
        ?COMMAND_ID_BROADCAST_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?BROADCAST_SM_RESP);
        ?COMMAND_ID_QUERY_SM ->
            smpp_pdu_syntax:pack(Pdu, ?QUERY_SM);
        ?COMMAND_ID_QUERY_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?QUERY_SM_RESP);
        ?COMMAND_ID_REPLACE_SM ->
            smpp_pdu_syntax:pack(Pdu, ?REPLACE_SM);
        ?COMMAND_ID_REPLACE_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?REPLACE_SM_RESP);
        ?COMMAND_ID_CANCEL_SM ->
            smpp_pdu_syntax:pack(Pdu, ?CANCEL_SM);
        ?COMMAND_ID_CANCEL_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?CANCEL_SM_RESP);
        ?COMMAND_ID_QUERY_BROADCAST_SM ->
            smpp_pdu_syntax:pack(Pdu, ?QUERY_BROADCAST_SM);
        ?COMMAND_ID_QUERY_BROADCAST_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?QUERY_BROADCAST_SM_RESP);
        ?COMMAND_ID_CANCEL_BROADCAST_SM ->
            smpp_pdu_syntax:pack(Pdu, ?CANCEL_BROADCAST_SM);
        ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?CANCEL_BROADCAST_SM_RESP);
        ?COMMAND_ID_ENQUIRE_LINK ->
            smpp_pdu_syntax:pack(Pdu, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK ->
            smpp_pdu_syntax:pack(Pdu, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND ->
            smpp_pdu_syntax:pack(Pdu, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?UNBIND_RESP);
        ?COMMAND_ID_OUTBIND ->
            smpp_pdu_syntax:pack(Pdu, ?OUTBIND);
        ?COMMAND_ID_BIND_RECEIVER ->
            smpp_pdu_syntax:pack(Pdu, ?BIND_RECEIVER);
        ?COMMAND_ID_BIND_RECEIVER_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?BIND_RECEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSCEIVER ->
            smpp_pdu_syntax:pack(Pdu, ?BIND_TRANSCEIVER);
        ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?BIND_TRANSCEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSMITTER ->
            smpp_pdu_syntax:pack(Pdu, ?BIND_TRANSMITTER);
        ?COMMAND_ID_BIND_TRANSMITTER_RESP ->
            smpp_pdu_syntax:pack(Pdu, ?BIND_TRANSMITTER_RESP);
        Other ->
            SeqNum = smpp_pdu_syntax:sequence_number(Pdu),
            {error, Other, ?ESME_RINVCMDID, SeqNum}
    end.


unpack(BinPdu) ->
    case smpp_pdu_syntax:command_id(BinPdu) of
        ?COMMAND_ID_DELIVER_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?DELIVER_SM);
        ?COMMAND_ID_DELIVER_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?DELIVER_SM_RESP);
        ?COMMAND_ID_SUBMIT_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?SUBMIT_SM);
        ?COMMAND_ID_SUBMIT_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?SUBMIT_SM_RESP);
        ?COMMAND_ID_SUBMIT_MULTI ->
            smpp_pdu_syntax:unpack(BinPdu, ?SUBMIT_MULTI);
        ?COMMAND_ID_SUBMIT_MULTI_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?SUBMIT_MULTI_RESP);
        ?COMMAND_ID_DATA_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?DATA_SM_RESP);
        ?COMMAND_ID_ALERT_NOTIFICATION ->
            smpp_pdu_syntax:unpack(BinPdu, ?ALERT_NOTIFICATION);
        ?COMMAND_ID_BROADCAST_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?BROADCAST_SM);
        ?COMMAND_ID_BROADCAST_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?BROADCAST_SM_RESP);
        ?COMMAND_ID_QUERY_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?QUERY_SM);
        ?COMMAND_ID_QUERY_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?QUERY_SM_RESP);
        ?COMMAND_ID_REPLACE_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?REPLACE_SM);
        ?COMMAND_ID_REPLACE_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?REPLACE_SM_RESP);
        ?COMMAND_ID_CANCEL_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?CANCEL_SM);
        ?COMMAND_ID_CANCEL_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?CANCEL_SM_RESP);
        ?COMMAND_ID_QUERY_BROADCAST_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?QUERY_BROADCAST_SM);
        ?COMMAND_ID_QUERY_BROADCAST_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?QUERY_BROADCAST_SM_RESP);
        ?COMMAND_ID_CANCEL_BROADCAST_SM ->
            smpp_pdu_syntax:unpack(BinPdu, ?CANCEL_BROADCAST_SM);
        ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?CANCEL_BROADCAST_SM_RESP);
        ?COMMAND_ID_ENQUIRE_LINK ->
            smpp_pdu_syntax:unpack(BinPdu, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK ->
            smpp_pdu_syntax:unpack(BinPdu, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND ->
            smpp_pdu_syntax:unpack(BinPdu, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?UNBIND_RESP);
        ?COMMAND_ID_OUTBIND ->
            smpp_pdu_syntax:unpack(BinPdu, ?OUTBIND);
        ?COMMAND_ID_BIND_RECEIVER ->
            smpp_pdu_syntax:unpack(BinPdu, ?BIND_RECEIVER);
        ?COMMAND_ID_BIND_RECEIVER_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?BIND_RECEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSCEIVER ->
            smpp_pdu_syntax:unpack(BinPdu, ?BIND_TRANSCEIVER);
        ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?BIND_TRANSCEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSMITTER ->
            smpp_pdu_syntax:unpack(BinPdu, ?BIND_TRANSMITTER);
        ?COMMAND_ID_BIND_TRANSMITTER_RESP ->
            smpp_pdu_syntax:unpack(BinPdu, ?BIND_TRANSMITTER_RESP);
        Other ->
            SeqNum = smpp_pdu_syntax:sequence_number(BinPdu),
            {error, Other, ?ESME_RINVCMDID, SeqNum}
    end.

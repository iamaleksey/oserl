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
-module(smpp_pdu_syntax).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").
-include("smpp_pdu_syntax.hrl").

%%% HEADER EXPORTS
-export([command_id/1, command_status/1, sequence_number/1]).

%%% PDU EXPORTS
-export([new_pdu/4, pack/2, unpack/2]).

%%%-----------------------------------------------------------------------------
%%% HEADER EXPORTS
%%%-----------------------------------------------------------------------------
command_id(<<_Len:32, CmdId:32, _Status:32, _SeqNum:32, _Body/binary>>) ->
    CmdId;
command_id({CmdId, _Status, _SeqNum, _Body}) ->
    CmdId.


command_status(<<_Len:32, _CmdId:32, Status:32, _SeqNum:32, _Body/binary>>) ->
    Status;
command_status({_CmdId, Status, _SeqNum, _Body}) ->
    Status.


sequence_number(<<_Len:32, _CmdId:32, _Status:32, SeqNum:32, _Body/binary>>) ->
    SeqNum;
sequence_number({_CmdId, _Status, SeqNum, _Body}) ->
    SeqNum.

%%%-----------------------------------------------------------------------------
%%% PDU EXPORTS
%%%-----------------------------------------------------------------------------
new_pdu(CmdId, 0, SeqNum, Body) ->
    {CmdId,  0, SeqNum, Body};
new_pdu(CmdId, Status, SeqNum, _Body) ->
    {CmdId, Status, SeqNum, []}.


pack({CmdId, 0, SeqNum, Body}, PduType) ->
    case pack_body(Body, PduType) of
        {ok, BodyL} ->
		    BodyBin = list_to_binary(BodyL),
            Len = size(BodyBin) + 16,
            {ok, [<<Len:32, CmdId:32, 0:32, SeqNum:32>>, BodyBin]};
        {error, Status} ->
            {error, CmdId, Status, SeqNum}
    end;
pack({CmdId, Status, SeqNum, _Body}, _PduType) ->
    {ok, [<<16:32, CmdId:32, Status:32, SeqNum:32>>]}.


unpack(<<Len:32, CmdId:32, ?ESME_ROK:32, SeqNum:32, Body/binary>>, PduType)
  when Len == size(Body) + 16 ->
    case unpack_body(Body, PduType) of
        {ok, BodyParams} ->
            {ok, new_pdu(CmdId, ?ESME_ROK, SeqNum, BodyParams)};
        {error, Status} ->
            {error, CmdId, Status, SeqNum}
    end;
unpack(<<Len:32, CmdId:32, Status:32, SeqNum:32, Body/binary>>, _PduType)
  when Len == size(Body) + 16 ->
    {ok, new_pdu(CmdId, Status, SeqNum, [])};
unpack(<<_Len:32, CmdId:32, _Status:32, SeqNum:32, _Body/binary>>, _PduType) ->
    {error, CmdId, ?ESME_RINVCMDLEN, SeqNum}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
get_value(Name, Body) ->
    get_value(Name, Body, []).

get_value(_Name, [], Acc) ->
    {undefined, Acc};
get_value(Name, [{Name, Value} | T], Acc) ->
    {Value, Acc ++ T};
get_value(Name, [H | T], Acc) ->
    get_value(Name, T, [H | Acc]).


pack_body(Body, P) ->
    case pack_stds(Body, P#pdu.std_types) of
        {ok, Stds, BodyTlvs} ->
            case pack_tlvs(BodyTlvs, P#pdu.tlv_types) of
                {ok, Tlvs, BodyOpts} ->
                    case pack_opts(BodyOpts, P#pdu.opt_types) of
                        {ok, Opts} ->
                            {ok, [Stds, Tlvs, Opts]};
                        OptError ->
                            OptError
                    end;
                TlvError ->
                    TlvError
            end;
        StdError ->
            StdError
    end.


pack_stds(Body, StdTypes) ->
    pack_stds(Body, StdTypes, []).

pack_stds(Body, [], Acc) ->
    {ok, lists:reverse(Acc), Body};
pack_stds(Body, [Type | Types], Acc) ->
    {Value, NewBody} = get_value(smpp_param_syntax:get_name(Type), Body),
    case smpp_param_syntax:encode(Value, Type) of
        {ok, Bin} ->
            pack_stds(NewBody, Types, [Bin | Acc]);
        Error ->
            Error
    end.


pack_tlvs(Body, TlvTypes) ->
    pack_tlvs(Body, TlvTypes, []).

pack_tlvs(Body, [], Acc) ->
    {ok, Acc, Body};
pack_tlvs(Body, [Type | Types], Acc) ->
    {Value, NewBody} = get_value(smpp_param_syntax:get_name(Type), Body),
    case smpp_param_syntax:encode(Value, Type) of
        {ok, Bin} ->
            pack_tlvs(NewBody, Types, [Bin | Acc]);
        Error ->
            Error
    end.


pack_opts(Body, OptTypes) ->
    pack_opts(Body, OptTypes, []).

pack_opts(_Body, [], Acc) ->
    {ok, Acc};
pack_opts(Body, [Type | Types], Acc) ->
    {Value, NewBody} = get_value(smpp_param_syntax:get_name(Type), Body),
    case smpp_param_syntax:encode(Value, Type) of
        {ok, BinValue} ->
            pack_opts(NewBody, Types, [BinValue | Acc]);
        {error, ?ESME_RMISSINGTLV} ->
            pack_opts(NewBody, Types, Acc);
        Error ->
            Error
    end.


unpack_body(BinBody, P) ->
    case unpack_stds(BinBody, P#pdu.std_types) of
        {ok, StdValues, BinTlvs} ->
            case unpack_tlvs(BinTlvs, P#pdu.tlv_types) of
                {ok, TlvValues, BinOpts} ->
                    case unpack_opts(BinOpts, P#pdu.opt_types) of
                        {ok, OptValues} ->
                            {ok, StdValues ++ TlvValues ++ OptValues};
                        OptError ->
                            OptError
                    end;
                TlvError ->
                    TlvError
            end;
        StdError ->
            StdError
    end.


unpack_stds(BinBody, StdTypes) ->
    unpack_stds(BinBody, StdTypes, []).

unpack_stds(BinTlvs, [], Acc) ->
    {ok, Acc, BinTlvs};
unpack_stds(BinBody, [Type | Types], Acc) ->
    case smpp_param_syntax:decode(BinBody, Type) of
        {ok, Value, RestBinBody} ->
            Name = smpp_param_syntax:get_name(Type),
            unpack_stds(RestBinBody, Types, [{Name, Value} | Acc]);
        Error ->
            Error
    end.


unpack_tlvs(BinTlvs, TlvTypes) ->
    unpack_tlvs(BinTlvs, TlvTypes, []).

unpack_tlvs(BinOpts, [], Acc) ->
    {ok, Acc, BinOpts};
unpack_tlvs(BinTlvs, [Type | Types], Acc) ->
    case smpp_param_syntax:decode(BinTlvs, Type) of
        {ok, Value, RestBinTlvs} ->
            Name = smpp_param_syntax:get_name(Type),
            unpack_tlvs(RestBinTlvs, Types, [{Name, Value} | Acc]);
        Error ->
            Error
    end.


unpack_opts(BinOpts, []) ->
    case smpp_param_syntax:chop_tlv(BinOpts) of
        {ok, _Tlv, RestUnusedOpts} ->
            % Remaining octets seem to be a collection of unsupported TLVs.
            % Following compatibility guidelines recommend to silently discard
            % unsupported TLVs (if they are well-formed).
            %
            % After the first TLV was chopped, we call unpack_tlvs/3 cause, in
            % case of an error, we rather return the ?ESME_RINVTLVSTREAM error
            % instead of the ?ESME_RINVCMDLEN value returned by this function.
            unpack_opts(RestUnusedOpts, [], []);
        {error, <<>>} ->
            {ok, []};
        _Error ->
            % Guess that no unsupported TLVs were appended to the body,
            % just dealing with a malformed PDU.
            {error, ?ESME_RINVCMDLEN}
    end;
unpack_opts(BinOpts, OptTypes) ->
    unpack_opts(BinOpts, OptTypes, []).

unpack_opts(<<>>, _OptTypes, Acc) ->
    {ok, Acc};
unpack_opts(UnusedOpts, [], Acc) ->
    case smpp_param_syntax:chop_tlv(UnusedOpts) of
        {ok, _Tlv, RestUnusedOpts} ->
            unpack_opts(RestUnusedOpts, [], Acc);
        _Error ->  % Malformed TLV
            {error, ?ESME_RINVTLVSTREAM}
    end;
unpack_opts(BinOpts, [Type | Types], Acc) ->
    case smpp_param_syntax:decode(BinOpts, Type) of
        {ok, Value, RestBinOpts} ->
            Name = smpp_param_syntax:get_name(Type),
            unpack_opts(RestBinOpts, Types, [{Name, Value}|Acc]);
        {error, ?ESME_RMISSINGTLV} -> % Ignore undefined optional TLVs
            unpack_opts(BinOpts, Types, Acc);
        Error ->
            Error
    end.


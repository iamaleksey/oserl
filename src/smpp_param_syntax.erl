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
-module(smpp_param_syntax).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").
-include("smpp_param_syntax.hrl").

%%% EXTERNAL EXPORTS
-export([chop_tlv/1, decode/2, encode/2, get_name/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
chop_tlv(<<Tag:16, Len:16, Binary/binary>>) ->
    case Binary of
        <<Val:Len/binary-unit:8, Rest/binary>> ->
            {ok, <<Tag:16, Len:16, Val/binary>>, Rest};
        _TruncatedVal ->
            {error, Binary}
    end;
chop_tlv(Binary) ->
    {error, Binary}.


decode(Binary, ParamType) when is_record(ParamType, standard) ->
    decode_std(Binary, ParamType);
decode(Binary, ParamType) when is_record(ParamType, tlv) ->
    decode_tlv(Binary, ParamType);
decode(_Binary, _ParamType) ->
    {error, ?ESME_RUNKNOWNERR}.


encode(Value, ParamType) when is_record(ParamType, standard) ->
    encode_std(Value, ParamType);
encode(Value, ParamType) when is_record(ParamType, tlv) ->
    encode_tlv(Value, ParamType);
encode(_Value, _ParamType) ->
    {error, ?ESME_RUNKNOWNERR}.


get_name(ParamType) when is_record(ParamType, standard) ->
    ParamType#standard.name;
get_name(ParamType) when is_record(ParamType, tlv) ->
    ParamType#tlv.name.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
decode_std(Binary, #standard{domain = Domain, error = Error}) ->
    case smpp_base_syntax:decode(Binary, Domain) of
        {error, _Reason} ->
            {error, Error};
        Ok ->
            Ok
    end.


%% Extracts a TLV from a Binary.  This functions searches for the TLV in
%% the binary and decodes it.  Assumes that Binary is a concatenation of
%% encoded TLVs.  If the TLV is multiple, the function gets every occurrence of
%% the TLV and returns the list of values, otherwise, once the first
%% occurrence is found the function stops searching.
%%
%% The Rest of the binary, without the decoded octets, is returned in the
%% resulting term.  The order of the remainder TLVs is preserved within Rest.
decode_tlv(Binary, TlvType) ->
    case decode_tlv(Binary, TlvType, [], []) of
        {ok, [], _Rest} ->
            {error, ?ESME_RMISSINGTLV};
        {ok, Values, Rest} when TlvType#tlv.multiple == false ->
            {ok, hd(Values), Rest};
        {ok, Values, Rest} ->
            {ok, Values, Rest};
        Error ->
            Error
    end.

decode_tlv(<<>>, _TlvType, Values, Acc) ->
    {ok, lists:reverse(Values), list_to_binary(lists:reverse(Acc))};
decode_tlv(<<T:16, L:16, Bin/binary>>, #tlv{tag = T} = TlvType, Values, Acc) ->
    % The Tag matches, try to decode the TLV
    case Bin of
        <<V:L/binary-unit:8, Rest/binary>> ->
            % The domain is updated to fit the Length (L) of the value (V)
            % of the TLV.  Fit never enlarges the size, see base_syntax:fit/2.
            Domain = smpp_base_syntax:fit(TlvType#tlv.domain, L),
            case decode_tlv_value(V, TlvType#tlv{domain = Domain}) of
                {ok, undefined} ->
                    decode_tlv(Rest, TlvType, Values, Acc);
                {ok, Value} when TlvType#tlv.multiple == false ->
                    % On single TLVs no further searching needed
                    decode_tlv(<<>>, TlvType, [Value], [Rest|Acc]);
                {ok, Value} ->
                    % If multiple values allowed, keep on searching
                    decode_tlv(Rest, TlvType, [Value|Values], Acc);
                Error ->
                    Error
            end;
        _TruncatedVal ->
            % The Value is shorter than Len octets, report the error
            {error, ?ESME_RINVTLVSTREAM}
    end;
decode_tlv(<<T:16, L:16, Bin/binary>>, TlvType, Values, Acc) ->
    % The Tag doesn't match, move the TLV to Acc and keep on searching.
    case Bin of
        <<V:L/binary-unit:8, Rest/binary>> ->
            Tlv = <<T:16, L:16, V:L/binary-unit:8>>,
            decode_tlv(Rest, TlvType, Values, [Tlv|Acc]);
        _TruncatedVal ->
            % The remainder of the binary is shorter than Len octets.
            {error, ?ESME_RINVTLVSTREAM}
    end;
decode_tlv(_Binary, _ParamType, _Values, _Acc) ->
    % 1-3 octets of data remaining, indicating a corrupt PDU.
    {error, ?ESME_RINVTLVSTREAM}.


decode_tlv_value(BinaryValue, TlvType) ->
    case smpp_base_syntax:decode(BinaryValue, TlvType#tlv.domain) of
        {ok, Value, <<>>} ->
            {ok, Value};
        {ok, _Value, _Something} ->
            % Uhmm, not all the value was decoded... TLV length is greater
            % than the size permitted by the domain specifier.
            {error, ?ESME_RINVTLVLEN};
        {error, _Reason} ->
            % The value doesn't match the declared domain for this TLV, it may
            % be a reserved value though, in such a case the SMPP protocol
            % specification recomends to return the default value.  Notice
            % that the default value could be the atom <tt>undefined</tt>.
            case smpp_base_syntax:decode(BinaryValue, TlvType#tlv.reserved) of
                {ok, _Value, <<>>} ->
                    {ok, TlvType#tlv.default};
                _Error ->
                    case TlvType#tlv.error of
                        undefined ->
                            {error, ?ESME_RINVTLVVAL};
                        ErrorCode ->
                            {error, ErrorCode}
                    end
            end
    end.


encode_std(undefined, #standard{default = undefined, error = undefined}) ->
	{error, ?ESME_RUNKNOWNERR};
encode_std(undefined, #standard{default = undefined, error = Error}) ->
	{error, Error};
encode_std(undefined, #standard{domain = Domain, default = Default}) ->
    % Assumes default values should always be OK...if not, it'll be easier to
    % review the default definitions rather than touching this clause.
	smpp_base_syntax:encode(Default, Domain);
encode_std(Value, #standard{domain = Domain, error = Error}) ->
    case smpp_base_syntax:encode(Value, Domain) of
        {error, _Reason} when Error == undefined ->
            {error, ?ESME_RUNKNOWNERR};
        {error, _Reason} ->
            {error, Error};
        Ok ->
            Ok
    end.


encode_tlv(undefined, _TlvType) ->
    {error, ?ESME_RMISSINGTLV};
encode_tlv(Values, #tlv{multiple = true} = TlvType) ->
    encode_multiple_tlv(Values, TlvType);
encode_tlv(Value, TlvType) ->
    encode_single_tlv(Value, TlvType).


encode_multiple_tlv([], _TlvType) ->
    {error, ?ESME_RMISSINGTLV};
encode_multiple_tlv(Values, TlvType) when is_list(Values) ->
    encode_multiple_tlv(Values, TlvType, []);
encode_multiple_tlv(_Value, #tlv{error = undefined}) ->
    {error, ?ESME_RINVTLVVAL};
encode_multiple_tlv(_Value, TlvType) ->
    {error, TlvType#tlv.error}.

encode_multiple_tlv([], _TlvType, Acc) ->
    {ok, Acc};
encode_multiple_tlv([Value|Values], TlvType, Acc) ->
    case encode_single_tlv(Value, TlvType) of
        {ok, Binary} ->
            encode_multiple_tlv(Values, TlvType, [Binary | Acc]);
        Error ->
            Error
    end.

encode_single_tlv(Value, #tlv{tag = Tag, domain = Domain, error = Error}) ->
    case smpp_base_syntax:encode(Value, Domain) of
        {ok, Binary} ->
            {ok, [<<Tag:16/integer, (size(Binary)):16/integer>>, Binary]};
        {error, _Reason} when Error == undefined ->
            {error, ?ESME_RINVTLVVAL};
        {error, _Reason} ->
            {error, Error}
    end.

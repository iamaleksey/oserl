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
-module(smpp_sm).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").
-include("smpp_base.hrl").

%%% EXTERNAL EXPORTS
-export([add_ie/2,
         chop_udh/1,
         unpack/2,
         destination_port/1,
         ie/2,
         join_user_data/1,
         message_user_data/1,
         originator_port/1,
         pack/2,
         port_addressing_8/3,
         port_addressing_16/3,
         reference_number/1,
         reply_addresses/1,
         reply_destination_address/1,
         reply_source_address/1,
         split/3,
         split/4,
         split_user_data/2,
         split_user_data/3,
         total_segments/1,
         udhi/1,
         udhi/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
add_ie(ParamList, Ie) ->
    {Tag, Data} = message_user_data(ParamList),
    case udhi(ParamList) of
        true ->
            {[_Udhl|Ies], Rest} = chop_udh(Data),
            NewData = udh([Ie, Ies]) ++ Rest,
            [{Tag, NewData} | proplists:delete(Tag, ParamList)];
        false ->
            NewData = udh([Ie]) ++ Data,
            [{Tag, NewData} | udhi(proplists:delete(Tag, ParamList), true)]
    end.


chop_udh([Udhl|_] = Data) ->
    lists:split(Udhl + 1, Data).


destination_port(Pdu) ->
    {_Tag, Data} = message_user_data(Pdu),
    case catch ie(?IEI_PORT_16, Data) of
        [?IEI_PORT_16, _, DestPort1, DestPort2, _, _] ->
            <<DestPort:16>> = <<DestPort1:8, DestPort2:8>>,
            DestPort;
        _Error ->
            [?IEI_PORT_8, _, DestPort, _] = ie(?IEI_PORT_8, Data),
            DestPort
    end.


ie(Iei, [Udhl|T]) ->
    ie(Iei, T, Udhl).

ie(Iei, [Iei, Iedl|_] = Data, _Len) ->
    lists:sublist(Data, Iedl + 2);
ie(Iei, [_Other, Iedl|_] = Data, Len) ->
    NewLen = Len - Iedl - 2,
    ie(Iei, lists:sublist(Data, Iedl + 3, NewLen), NewLen).


join_user_data(Segments) ->
    Choped = [chop_udh(S) || S <- Segments], % UDH + Data
    P = fun({[_,_,_,Ref,_,X],_}, {[_,_,_,Ref,_,Y],_}) -> X < Y end,
    [{[_,_,_,Ref,_,_], Data}|T] = lists:sort(P, Choped),     % Get Ref from UDH
    lists:foldl(fun({_,D}, {Acc, R}) -> {Acc ++ D, R} end, {Data, Ref}, T).


message_user_data(ParamList) when is_list(ParamList) ->
    case proplists:get_value(message_payload, ParamList) of
        undefined ->
            {short_message, proplists:get_value(short_message, ParamList)};
        MessagePayload ->
            {message_payload, MessagePayload}
    end;
message_user_data(Pdu) ->
    case smpp_operation:get_value(message_payload, Pdu) of
        undefined ->
            {short_message, smpp_operation:get_value(short_message, Pdu)};
        MessagePayload ->
            {message_payload, MessagePayload}
    end.


originator_port(Pdu) ->
    {_Tag, Data} = message_user_data(Pdu),
    case catch ie(?IEI_PORT_16, Data) of
        [?IEI_PORT_16, _, _, _, OrigPort1, OrigPort2] ->
            <<OrigPort:16>> = <<OrigPort1:8, OrigPort2:8>>,
            OrigPort;
        _Error ->
            [?IEI_PORT_8, _, _, OrigPort] = ie(?IEI_PORT_8, Data),
            OrigPort
    end.


pack([], _DataCoding) ->
    [];
pack(UserData, DataCoding) when % DataCoding == 2#00000000;
                                DataCoding == 2#00010000;
                                DataCoding == 2#00010100;
                                DataCoding == 2#00011000;
                                DataCoding == 2#00011100;
                                DataCoding == 2#00110000;
                                DataCoding == 2#00110100;
                                DataCoding == 2#00111000;
                                DataCoding == 2#00111100 ->
    % 00x1xx00
    pack_7bit(UserData);
pack(UserData, DataCoding) when DataCoding >= 2#11000000,
                                DataCoding =< 2#11011111 ->
    % 110xxxxx
    pack_7bit(UserData);
pack(UserData, DataCoding) when DataCoding >= 2#11110000,
                                DataCoding =< 2#11110011 ->
    % 111100xx
    pack_7bit(UserData);
pack(UserData, DataCoding) when DataCoding == 2#00010010;
                                DataCoding == 2#00010110;
                                DataCoding == 2#00011010;
                                DataCoding == 2#00011110;
                                DataCoding == 2#00110010;
                                DataCoding == 2#00110110;
                                DataCoding == 2#00111010;
                                DataCoding == 2#00111110 ->
    % 00x1xx10
    pack_16bit(UserData);
pack(UserData, _DataCoding) ->
    UserData.  % Leave 8 bit as is


port_addressing_8(ParamList, DestPort, OrigPort) ->
    Ie = port_addressing_8_ie(DestPort, OrigPort),
    add_ie(ParamList, Ie).


port_addressing_16(ParamList, DestPort, OrigPort) ->
    Ie = port_addressing_16_ie(DestPort, OrigPort),
    add_ie(ParamList, Ie).


reference_number(Pdu) ->
    ShortMessage = smpp_operation:get_value(short_message, Pdu),
    [?IEI_CONCAT, _, RefNum|_] = ie(?IEI_CONCAT, ShortMessage),
    RefNum.


reply_addresses(Pdu) ->
    reply_source_address(Pdu) ++ reply_destination_address(Pdu).


reply_destination_address(Pdu) ->
    [{dest_addr_ton, smpp_operation:get_value(source_addr_ton, Pdu)},
     {dest_addr_npi, smpp_operation:get_value(source_addr_npi, Pdu)},
     {destination_addr, smpp_operation:get_value(source_addr, Pdu)}].


reply_source_address(Pdu) ->
    [{source_addr_ton, smpp_operation:get_value(dest_addr_ton, Pdu)},
     {source_addr_npi, smpp_operation:get_value(dest_addr_npi, Pdu)},
     {source_addr, smpp_operation:get_value(destination_addr, Pdu)}].


split(ParamList, RefNum, ConcatMethod) ->
    split(ParamList, RefNum, ConcatMethod, ?SM_MAX_SEGMENT_SIZE).


split(ParamList, RefNum, udh, MaxSegmentSize) ->
    SM = proplists:get_value(short_message, ParamList),
    % The short message may already include an User Data Header
    {Ies, Segments} = case udhi(ParamList) of
                          true ->
                              {[Udhl|Rest], Data} = chop_udh(SM),
                              {Rest, segments(Data, MaxSegmentSize - Udhl)};
                          false ->
                              {[], segments(SM, MaxSegmentSize)}
                      end,
    TotalSegments = length(Segments),
    % Remove the long SM and add the UDHI if not present
    NewParamList = udhi(proplists:delete(short_message, ParamList), true),
    % Add the Concat Information Element to the UDH of every Segment and
    % build a new Param List for each.
    F = fun(Segment, SeqNum) ->
                ConcatIe = concat_ie(RefNum, TotalSegments, SeqNum),
                ShortMessage = udh([ConcatIe | Ies]) ++ Segment,
                {[{short_message, ShortMessage} | NewParamList], SeqNum + 1}
        end,
    element(1, lists:mapfoldl(F, 1, Segments));
split(ParamList, RefNum, sar, MaxSegmentSize) ->
    SM = proplists:get_value(short_message, ParamList),
    % The short message may already include an User Data Header
    Segments = case udhi(ParamList) of
                   true ->
                       {[Udhl | _Rest] = Udh, Data} = chop_udh(SM),
                       L = segments(Data, MaxSegmentSize - Udhl),
                       % Add the header to every segment
                       [Udh ++ X || X <- L];
                   false ->
                       segments(SM, MaxSegmentSize)
               end,
    TotalSegments = length(Segments),
    % Remove the long SM from the Param List
    NewParamList = proplists:delete(short_message, ParamList),
    % Create the sar TLVs for every Segment and build a new Param List for each
    F = fun(Segment, SeqNum) ->
                Params = [{short_message, Segment},
                          {sar_msg_ref_num, RefNum},
                          {sar_segment_seqnum, SeqNum},
                          {sar_total_segments, TotalSegments} | NewParamList],
                {Params, SeqNum + 1}
        end,
    element(1, lists:mapfoldl(F, 1, Segments)).


split_user_data(Data, RefNum) ->
    split_user_data(Data, RefNum, ?SM_MAX_SEGMENT_SIZE).

split_user_data(Data, RefNum, MaxSegmentSize) ->
    Segments = segments(Data, MaxSegmentSize),
    TotalSegments = length(Segments),
    F = fun(Segment, SeqNum) ->
                ConcatIe = concat_ie(RefNum, TotalSegments, SeqNum),
                {udh([ConcatIe]) ++ Segment, SeqNum + 1}
        end,
    element(1, lists:mapfoldl(F, 1, Segments)).


total_segments(Pdu) ->
    ShortMessage = smpp_operation:get_value(short_message, Pdu),
    [?IEI_CONCAT, _, _, TotalSegments | _] = ie(?IEI_CONCAT, ShortMessage),
    TotalSegments.


unpack([], _DataCoding) ->
    [];
unpack(PackedUserData, DataCoding) when % DataCoding == 2#00000000;
                                        DataCoding == 2#00010000;
                                        DataCoding == 2#00010100;
                                        DataCoding == 2#00011000;
                                        DataCoding == 2#00011100;
                                        DataCoding == 2#00110000;
                                        DataCoding == 2#00110100;
                                        DataCoding == 2#00111000;
                                        DataCoding == 2#00111100 ->
    % 00x1xx00
    unpack_7bit(PackedUserData);
unpack(PackedUserData, DataCoding) when DataCoding >= 2#11000000,
                                        DataCoding =< 2#11011111 ->
    % 110xxxxx
    unpack_7bit(PackedUserData);
unpack(PackedUserData, DataCoding) when DataCoding >= 2#11110000,
                                        DataCoding =< 2#11110011 ->
    % 111100xx
    unpack_7bit(PackedUserData);
unpack(PackedUserData, DataCoding) when DataCoding == 2#00010010;
                                        DataCoding == 2#00010110;
                                        DataCoding == 2#00011010;
                                        DataCoding == 2#00011110;
                                        DataCoding == 2#00110010;
                                        DataCoding == 2#00110110;
                                        DataCoding == 2#00111010;
                                        DataCoding == 2#00111110 ->
    % 00x1xx10
    unpack_16bit(PackedUserData);
unpack(PackedUserData, _DataCoding) ->
    PackedUserData.  % Leave 8 bit as is


udhi(ParamList) when is_list(ParamList) ->
    case proplists:get_value(esm_class, ParamList) of
        undefined ->
            false;
        EsmClass ->
            udhi_value(EsmClass)
    end;
udhi(Pdu) ->
    case smpp_operation:get_value(esm_class, Pdu) of
        undefined ->
            false;
        EsmClass ->
            udhi_value(EsmClass)
    end.


udhi(ParamList, Udhi) ->
    case proplists:get_value(esm_class, ParamList) of
        undefined when Udhi == true ->
            [{esm_class, ?ESM_CLASS_GSM_UDHI} | ParamList];
        undefined ->
            ParamList;
        EsmClass when Udhi == true ->
            NewEsmClass = EsmClass bor ?ESM_CLASS_GSM_UDHI,
            lists:keyreplace(esm_class, 1, ParamList, {esm_class,NewEsmClass});
        EsmClass ->
            NewEsmClass = EsmClass band (?ESM_CLASS_GSM_UDHI bxor 2#11111111),
            lists:keyreplace(esm_class, 1, ParamList, {esm_class,NewEsmClass})
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
concat_ie(RefNum, TotalSegments, SeqNum) ->
    [?IEI_CONCAT, ?IEDL_CONCAT, RefNum, TotalSegments, SeqNum].


pack_7bit(UserData) ->
	case list_to_binary(UserData) of
		<<_Head:1, Septet:7>> ->
			[Septet];
		<<_Head:1, Septet:7, T/binary>> ->
			pack_7bit(T, Septet, [], 1)
	end.

pack_7bit(<<_Head:1, Septet:7>>, Rem, Acc, 7) ->
    Char = (Septet * 2) + Rem,
    lists:reverse([Char | Acc]);
pack_7bit(<<Byte:8>>, Rem, Acc, Len) ->
    Offset = 7 - Len,
    <<_Head:1, NewRem:Offset/integer, Val:Len/integer>> = <<Byte>>,
    Char = (Val * trunc(math:pow(2, Offset + 1))) + Rem,
    lists:reverse([NewRem, Char | Acc]);
pack_7bit(<<_Head:1, Septet:7, T/binary>>, _Rem, Acc, 8) ->
    pack_7bit(T, Septet, Acc, 1);
pack_7bit(<<Byte:8/integer, T/binary>>, Rem, Acc, Len) ->
    Offset = 7 - Len,
    <<_Head:1, NewRem:Offset/integer, Val:Len/integer>> = <<Byte>>,
    Char = (Val * trunc(math:pow(2, Offset + 1))) + Rem,
    pack_7bit(T, NewRem, [Char | Acc], Len + 1).


pack_16bit(L) ->
    pack_16bit(L, []).

pack_16bit([], Acc) ->
    lists:reverse(Acc);
pack_16bit([H|T], Acc) ->
	Byte1 = H div 256,
	Byte2 = H - (Byte1 * 256),
    pack_16bit(T, [Byte2, Byte1 | Acc]).


port_addressing_8_ie(DestPort, OrigPort) ->
    [DestPort1] = binary_to_list(<<DestPort:8>>),
    [OrigPort1] = binary_to_list(<<OrigPort:8>>),
    [?IEI_PORT_8, ?IEDL_PORT_8, DestPort1, OrigPort1].


port_addressing_16_ie(DestPort, OrigPort) ->
    [DestPort1, DestPort2] = binary_to_list(<<DestPort:16>>),
    [OrigPort1, OrigPort2] = binary_to_list(<<OrigPort:16>>),
    [?IEI_PORT_16, ?IEDL_PORT_16, DestPort1, DestPort2, OrigPort1, OrigPort2].


%% Splits ``Data`` in ``Segments`` of at most ``Len`` characters.
segments(Data, Len) ->
    segments(Data, Len, []).

segments(Data, Len, Acc) ->
    case catch lists:split(Len, Data) of
        {'EXIT', _Badarg} ->
            lists:reverse([Data | Acc]);
        {Segment, []} ->
            lists:reverse([Segment | Acc]);
        {Segment, Rest} ->
            segments(Rest, Len, [Segment | Acc])
    end.


unpack_7bit(UserData7Bit) ->
	case list_to_binary(UserData7Bit) of
		<<Septet:8/integer>> ->
			[Septet];
		<<Rem:1/integer, Char:7/integer, T/binary>> ->
			unpack_7bit(T, Rem, [Char], 2)
	end.

unpack_7bit(<<Septet:7/integer, Val:1/integer>>, Rem, Acc, 7) ->
    Char = (Val * trunc(math:pow(2, 6))) + Rem,
    lists:reverse([Septet, Char | Acc]);
unpack_7bit(<<Byte:8/integer>>, Rem, Acc, Offset) ->
    Len = 8 - Offset,
    <<_Padding:Offset/integer, Val:Len/integer>> = <<Byte>>,
    Char = (Val * trunc(math:pow(2, Offset - 1))) + Rem,
    lists:reverse([Char | Acc]);
unpack_7bit(<<NewRem:1/integer, Char:7/integer, T/binary>>, Rem, Acc, 1) ->
    unpack_7bit(T, NewRem, [Char,Rem | Acc], 2);
unpack_7bit(<<Byte:8/integer, T/binary>>, Rem, Acc, Offset) ->
    Len = 8 - Offset,
    <<NewRem:Offset/integer, Val:Len/integer>> = <<Byte>>,
    NewOffset = if Offset == 7 -> 1; true -> Offset + 1 end,
    Char = (Val * trunc(math:pow(2, Offset - 1))) + Rem,
    unpack_7bit(T, NewRem, [Char | Acc], NewOffset).


unpack_16bit(UserData16Bit) ->
    unpack_16bit(list_to_binary(UserData16Bit), []).

unpack_16bit(<<>>, Acc) ->
    lists:reverse(Acc);
unpack_16bit(<<H:16/integer, T/binary>>, Acc) ->
    unpack_16bit(T, [H|Acc]).


udh(IeList) ->
    Udh = lists:flatten(IeList),
    [length(Udh) | Udh].


udhi_value(EsmClass)
  when (EsmClass band ?ESM_CLASS_GSM_UDHI) == ?ESM_CLASS_GSM_UDHI ->
    true;
udhi_value(_EsmClass) ->
    false.

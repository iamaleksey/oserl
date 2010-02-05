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
-module(smpp_base_syntax).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").
-include("smpp_base_syntax.hrl").

%%% EXTERNAL EXPORTS
-export([decode/2, encode/2, fit/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
decode(Binary, #constant{value = Value} = Type) ->
    Cons = if
               is_list(Value) ->
                   list_to_binary(Value);
               is_integer(Value) ->
                   <<Value/integer>>;
               is_binary(Value) ->
                   <<Value/binary>>
           end,
    Size = size(Cons),
    case Binary of
        <<Cons:Size/binary-unit:8, Rest/binary>> ->
            {ok, Value, Rest};
        <<Other:Size/binary-unit:8, _Rest/binary>> ->
            {error, {type_mismatch, Type, Other}};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #integer{size = Size} = Type) ->
    case Binary of
        <<Value:Size/integer-unit:8, Rest/binary>> ->
            {ok, Value, Rest};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #c_octet_string{format = F} = Type) when F /= undefined ->
    case decode(Binary, Type#c_octet_string{format = undefined}) of
        {ok, Value, Rest} ->
            case F(Value) of
                true ->
                    {ok, Value, Rest};
                false ->
                    {error, {type_mismatch, Type, Value}}
            end;
        Error ->
            Error
    end;
decode(Binary, #c_octet_string{fixed = true, size = Size} = Type) ->
    Len = Size - 1,
    case Binary of
        <<?NULL_CHARACTER:8, Rest/binary>> ->
            {ok, "", Rest};
        <<Value:Len/binary-unit:8, ?NULL_CHARACTER:8, Rest/binary>> ->
            {ok, binary_to_list(Value), Rest};
        <<NotNullTerminated:Size/binary-unit:8, _Rest/binary>> ->
            {error, {type_mismatch, Type, binary_to_list(NotNullTerminated)}};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #c_octet_string{fixed = false, size = Size} = Type) ->
    case cl_binary:take_until(Binary, <<?NULL_CHARACTER:8>>, Size) of
        {ok, UntilNull, <<?NULL_CHARACTER:8, Rest/binary>>} ->
            {ok, binary_to_list(UntilNull), Rest};
        {error, {not_found, _Null, UntilSize}} ->
            {error, {type_mismatch, Type, binary_to_list(UntilSize)}}
    end;
decode(Binary, #octet_string{format = F} = Type) when F /= undefined ->
    case decode(Binary, Type#octet_string{format = undefined}) of
        {ok, Value, Rest} ->
            case F(Value) of
                true ->
                    {ok, Value, Rest};
                false ->
                    {error, {type_mismatch, Type, Value}}
            end;
        Error ->
            Error
    end;
decode(Binary, #octet_string{fixed = true, size = Size} = Type) ->
    case Binary of
        <<Value:Size/binary-unit:8, Rest/binary>> ->
            {ok, binary_to_list(Value), Rest};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #octet_string{fixed = false, size = Size}) ->
    case Binary of
        <<Value:Size/binary-unit:8, Rest/binary>> ->
            {ok, binary_to_list(Value), Rest};
        <<Value/binary>> ->
            {ok, binary_to_list(Value), <<>>}
    end;
decode(Binary, #list{type = InnerType, size = Size} = Type) ->
    Len = (Size div 256) + 1,
    case Binary of
        <<Times:Len/integer-unit:8, Bin/binary>> ->
            case decode_iter(Bin, InnerType, Times) of
                {error, Reason} ->
                    {error, {type_mismatch, Type, Reason}};
                Ok ->
                    Ok
            end;
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #composite{name = undefined, tuple = Tuple} = Type) ->
    case decode_list(Binary, tuple_to_list(Tuple)) of
        {ok, Values, Rest} ->
            {ok, list_to_tuple(Values), Rest};
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}}
    end;
decode(Binary, #composite{name = Name, tuple = Tuple} = Type) ->
    case decode_list(Binary, tuple_to_list(Tuple)) of
        {ok, Values, Rest} ->
            {ok, list_to_tuple([Name|Values]), Rest};
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}}
    end;
decode(Binary, #union{types = Types} = Type) when length(Types) > 0 ->
    case decode_try(Binary, Types) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok
    end;
decode(Binary, Type) ->
    {error, {type_mismatch, Type, Binary}}.


decode_iter(Binary, Type, Times) ->
    decode_iter(Binary, Type, Times, []).

decode_iter(Binary, Type, Times, Acc) when length(Acc) < Times ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            decode_iter(Rest, Type, Times, [Value|Acc]);
        Error ->
            Error
    end;
decode_iter(Binary, _Type, _Times, Acc) ->
    {ok, lists:reverse(Acc), Binary}.


decode_list(Binary, Types) ->
    decode_list(Binary, Types, []).

decode_list(Binary, [], Values) ->
    {ok, lists:reverse(Values), Binary};
decode_list(Binary, [Type|Types], Values) ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            decode_list(Rest, Types, [Value|Values]);
        Error ->
            Error
    end.


%% Tries to decode a Value using all ``Types`` on a list, first
%% successful match is returned.  If none of the ``Types`` apply, this
%% function tries to guess the closest error description (error with highest
%% priority).
decode_try(Binary, Types) ->
    decode_try(Binary, Types, {}, 0).

decode_try(_Binary, [], Error, _Priority) ->
    Error;
decode_try(Binary, [Type|Types], Error, Priority) ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            {ok, Value, Rest};
        NewError ->
            NewPriority = error_priority(NewError),
            if
                NewPriority >= Priority ->
                    decode_try(Binary, Types, NewError, NewPriority);
                true ->
                    decode_try(Binary, Types, Error, Priority)
            end
    end.


encode(Value, #constant{value = Value}) when is_list(Value) ->
    {ok, list_to_binary(Value)};
encode(Value, #constant{value = Value}) when is_integer(Value) ->
    {ok, <<Value/integer>>};
encode(Value, #constant{value = Value}) when is_binary(Value) ->
    {ok, <<Value/binary>>};
encode(Value, #integer{size = Size, min = Min, max = Max})
  when is_integer(Value), Value >= Min, Value =< Max  ->
    {ok, <<Value:Size/integer-unit:8>>};
encode(Value, #c_octet_string{format = F} = Type) when F /= undefined ->
    case F(Value) of
        true ->
            encode(Value, Type#c_octet_string{format = undefined});
        false ->
            {error, {type_mismatch, Type, Value}}
    end;
encode(Value, #c_octet_string{fixed = true, size = Size})
  when is_list(Value), (length(Value) == Size - 1) or (length(Value) == 0) ->
    {ok, list_to_binary(Value ++ [?NULL_CHARACTER])};
encode(Value, #c_octet_string{size = Size})
  when is_list(Value), length(Value) < Size -> % was =<
    {ok, list_to_binary(Value ++ [?NULL_CHARACTER])};
encode(Value, #octet_string{format = F} = Type) when F /= undefined ->
    case F(Value) of
        true ->
            encode(Value, Type#octet_string{format = undefined});
        false ->
            {error, {type_mismatch, Type, Value}}
    end;
encode(Value, #octet_string{fixed = true, size = Size})
  when is_list(Value), (length(Value) == Size) or (length(Value) == 0) ->
    {ok, list_to_binary(Value)};
encode(Value, #octet_string{size = Size})
  when is_list(Value), length(Value) =< Size ->
    {ok, list_to_binary(Value)};
encode(Values, #list{type = InnerType, size = Size} = Type)
  when is_list(Values), length(Values) =< Size ->
    case encode_iter(Values, InnerType) of
        {ok, Binary} ->
            LenSize = (Size div 256) + 1,
            {ok, <<(length(Values)):LenSize/integer-unit:8, Binary/binary>>};
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}}
    end;
encode(Value, #composite{name = undefined, tuple = Tuple} = Type)
  when is_tuple(Value), size(Value) == size(Tuple) ->
    case encode_list(tuple_to_list(Value), tuple_to_list(Tuple)) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok
    end;
encode(Value, #composite{name = Name, tuple = Tuple} = Type)
  when element(1, Value) == Name, size(Value) - 1 == size(Tuple) ->
    case encode_list(tl(tuple_to_list(Value)), tuple_to_list(Tuple)) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok
    end;
encode(Value, #union{types = Types} = Type) when length(Types) > 0 ->
    case encode_try(Value, Types) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok
    end;
encode(Value, Type) ->
    {error, {type_mismatch, Type, Value}}.


encode_iter(Values, Type) ->
    encode_iter(Values, Type, []).

encode_iter([], _Type, Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
encode_iter([Value|Values], Type, Acc) ->
    case encode(Value, Type) of
        {ok, Binary} ->
            encode_iter(Values, Type, [Binary|Acc]);
        Error ->
            Error
    end.


%% This functions encodes a list of values using the corresponding type
%% on the list of types and returns the concatenation of every encoded binary.
%% Used to transverse complex data structures (lists and composites).
encode_list(Values, Types) ->
    encode_list(Values, Types, []).

encode_list([], _Types, Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
encode_list([Value|Values], [Type|Types], Acc) ->
    case encode(Value, Type) of
        {ok, Binary} ->
            encode_list(Values, Types, [Binary|Acc]);
        Error ->
            Error
    end.


%% Tries to encode a Value against a list of Types, first successful match
%% is returned.  If none of the Types apply, this function tries to guess
%% the closest error description (error with greatest priority).
encode_try(Value, Types) ->
    encode_try(Value, Types, {}, 0).

encode_try(_Value, [], Error, _Priority) ->
    Error;
encode_try(Value, [Type|Types], Error, Priority) ->
    case encode(Value, Type) of
        {ok, Binary} ->
            {ok, Binary};
        NewError ->
            NewPriority = error_priority(NewError),
            if
                NewPriority >= Priority ->
                    encode_try(Value, Types, NewError, NewPriority);
                true ->
                    encode_try(Value, Types, Error, Priority)
            end
    end.


fit(#integer{size = Size} = Type, NewSize) when NewSize < Size ->
    Type#integer{size = NewSize};
fit(#c_octet_string{size = Size} = Type, NewSize) when NewSize =< Size ->
    Type#c_octet_string{size = NewSize, fixed = true};
fit(#octet_string{size = Size} = Type, NewSize) when NewSize =< Size ->
    Type#octet_string{size = NewSize, fixed = true};
fit(#list{size = Size} = Type, NewSize) when NewSize < Size ->
    Type#list{size = Size};
fit(Type, _Size) ->
    Type.


%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
%% Gets the priority of an Error report (the highest).
%%
%% FIXME:  This patch is a *SIMPLE* way of adding some sort of priorities
%% on errors.  It works fine for PDU encoding but a better approach should be
%% provided in the future.
%%
%% Just about every PDU failure has an error_depth of 2, produced by the
%% command_id constant mismatch.  Whenever the command_id constant test
%% succeeds, if an error ocurrs on a later field of the PDU, even this error
%% would have the same depth (2), it should be consider a better match...
%% well this is just what this patch does.
error_priority({error, {type_mismatch, _Type, Reason}}) ->
    error_priority(Reason, 1);
error_priority(_Error) ->
    0.

error_priority({type_mismatch, _Type, {type_mismatch, Type, Reason}}, Depth) ->
    error_priority({type_mismatch, Type, Reason}, Depth + 1);
error_priority({type_mismatch, T, _R}, Depth) when is_record(T, integer);
                                                   is_record(T, c_octet_string);
                                                   is_record(T, octet_string) ->
    (Depth * 3) + 1;
error_priority({type_mismatch, T, _R}, Depth) when is_record(T, union);
                                                   is_record(T, list);
                                                   is_record(T, composite) ->
    (Depth * 3) + 2;
error_priority(_Other, Depth) -> % contants and unknown errors
    (Depth * 3) + 0.

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
-ifndef(smpp_base_syntax).
-define(smpp_base_syntax, true).

%%% MACROS
%% SMPP Parameter Field Size Notation.
-define(CONSTANT(Value),
        #constant{value = Value}).
-define(INTEGER(Size),
        #integer{size = Size, min = 0, max = math:pow(256, Size) - 1}).
-define(C_OCTET_STRING(Fixed, Size),
        #c_octet_string{fixed = Fixed, size = Size, format = undefined}).
-define(OCTET_STRING(Fixed, Size),
        #octet_string{fixed = Fixed, size = Size, format = undefined}).
-define(LIST(Type),
        #list{type = Type, size = 255}).
-define(COMPOSITE(Name, Tuple),
        #composite{name = Name, tuple = Tuple}).
-define(UNION(Types),
        #union{types = Types}).

%% Simplified Field Size Notation Macros for readability.
%%
%% Some options are implicitly assigned.  Most of the SMPP PDU fields
%% definitions are more readable using these macros.
-define(BOUND_INTEGER(Size, Max),
        #integer{size = Size, min = 0, max = Max}).
-define(RANGE_INTEGER(Size, Min, Max),
        #integer{size = Size, min = Min, max = Max}).

-define(HEX_C_OCTET_STRING(Fixed, Size),
        #c_octet_string{
            fixed  = Fixed,
            size   = Size,
            format = fun(Str) -> cl_string:is_hex(Str) end}).
-define(HEX_OCTET_STRING(Fixed, Size),
        #octet_string{
            fixed  = Fixed,
            size   = Size,
            format = fun(Str) -> cl_string:is_hex(Str) end}).

-define(DEC_C_OCTET_STRING(Fixed, Size),
        #c_octet_string{
            fixed  = Fixed,
            size   = Size,
            format = fun(Str) -> cl_string:is_dec(Str) end}).
-define(DEC_OCTET_STRING(Fixed, Size),
        #octet_string{
            fixed  = Fixed,
            size   = Size,
            format = fun(Str) -> cl_string:is_dec(Str) end}).

-define(ANONYMOUS_COMPOSITE(Tuple), #composite{tuple = Tuple}).

-define(VAR_C_OCTET_STRING(Size),   ?C_OCTET_STRING(false, Size)).
-define(VAR_OCTET_STRING(Size),     ?OCTET_STRING(false, Size)).
-define(FIXED_C_OCTET_STRING(Size), ?C_OCTET_STRING(true, Size)).
-define(FIXED_OCTET_STRING(Size),   ?OCTET_STRING(true, Size)).

-define(VAR_HEX_C_OCTET_STRING(Size),   ?HEX_C_OCTET_STRING(false, Size)).
-define(VAR_HEX_OCTET_STRING(Size),     ?HEX_OCTET_STRING(false, Size)).
-define(FIXED_HEX_C_OCTET_STRING(Size), ?HEX_C_OCTET_STRING(true, Size)).
-define(FIXED_HEX_OCTET_STRING(Size),   ?HEX_OCTET_STRING(true, Size)).

-define(VAR_DEC_C_OCTET_STRING(Size),   ?DEC_C_OCTET_STRING(false, Size)).
-define(VAR_DEC_OCTET_STRING(Size),     ?DEC_OCTET_STRING(false, Size)).
-define(FIXED_DEC_C_OCTET_STRING(Size), ?DEC_C_OCTET_STRING(true, Size)).
-define(FIXED_DEC_OCTET_STRING(Size),   ?DEC_OCTET_STRING(true, Size)).

-define(SIZED_LIST(Type, Size),
        #list{type = Type, size = Size}).

%% Time strings
-define(ATIME_C_OCTET_STRING,
        #c_octet_string{
            fixed  = true,
            size   = 17,
            format = fun(Str) -> cl_string:is_atime(Str) end}).
-define(RTIME_C_OCTET_STRING,
        #c_octet_string{
            fixed  = true,
            size   = 17,
            format = fun(Str) -> cl_string:is_rtime(Str) end}).

%% Sets
-define(EMPTY,     ?UNION([])).
-define(SET(List), ?UNION(lists:map(fun(C) -> ?CONSTANT(C) end, List))).


%%% RECORDS
-record(constant, {value}).

-record(integer, {size, min, max}).

-record(c_octet_string, {fixed, size, format}).

-record(octet_string, {fixed, size, format}).

-record(list, {type, size}).

-record(composite, {name, tuple}).

-record(union, {types}).

-endif.  % -ifndef(smpp_base_syntax)


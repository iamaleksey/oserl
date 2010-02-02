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
-module(smpp_base).

%%% INCLUDE FILES
-include("smpp_base.hrl").

%%% STANDARD VALUES EXPORTS
-export([dest_address_sme/4, dest_address_dl/2, unsuccess_sme/4]).

%%% TLV VALUES EXPORTS
-export([broadcast_area/2,
         broadcast_content_type/2,
         broadcast_frequency_interval/2,
         subaddress/2,
         callback_num/4,
         callback_num_atag/2,
         telematics_id/2,
         its_session_info/2,
         ms_validity_absolute/1,
         ms_validity_relative/3,
         network_error_code/2]).

%%%-----------------------------------------------------------------------------
%%% STANDARD VALUES EXPORTS
%%%-----------------------------------------------------------------------------
dest_address_sme(DestFlag, DestAddrTon, DestAddrNpi, DestAddr) ->
    ?DEST_ADDRESS_SME_VALUE(DestFlag, DestAddrTon, DestAddrNpi, DestAddr).


dest_address_dl(DestFlag, DlName) ->
    ?DEST_ADDRESS_DL_VALUE(DestFlag, DlName).


unsuccess_sme(DestAddrTon, DestAddrNpi, DestAddr, StatusCode) ->
    ?UNSUCCESS_SME_VALUE(DestAddrTon, DestAddrNpi, DestAddr, StatusCode).

%%%-----------------------------------------------------------------------------
%%% TLV VALUES EXPORTS
%%%-----------------------------------------------------------------------------
broadcast_area(Format, Details) ->
    ?BROADCAST_AREA_VALUE(Format, Details).


broadcast_content_type(NetworkType, Service) ->
    ?BROADCAST_CONTENT_TYPE_VALUE(NetworkType, Service).


broadcast_frequency_interval(TimeUnit, Number) ->
    ?BROADCAST_FREQUENCY_INTERVAL_VALUE(TimeUnit, Number).


subaddress(Tag, Data) ->
    ?SUBADDRESS_VALUE(Tag, Data).


callback_num(DigitModeIndicator, AddrTon, AddrNpi, NumberDigits) ->
    ?CALLBACK_NUM_VALUE(DigitModeIndicator, AddrTon, AddrNpi, NumberDigits).


callback_num_atag(DataCoding, DisplayCharacters) ->
    ?CALLBACK_NUM_ATAG_VALUE(DataCoding, DisplayCharacters).


telematics_id(ProtocolId, Reserved) ->
    ?TELEMATICS_ID_VALUE(ProtocolId, Reserved).


its_session_info(SessionNumber, SequenceNumber) ->
    ?ITS_SESSION_INFO_VALUE(SessionNumber, SequenceNumber).


ms_validity_absolute(Behaviour) ->
    ?MS_VALIDITY_ABSOLUTE_VALUE(Behaviour).


ms_validity_relative(Behaviour, TimeUnit, Number) ->
    ?MS_VALIDITY_RELATIVE_VALUE(Behaviour, TimeUnit, Number).


network_error_code(Type, Error) ->
    ?NETWORK_ERROR_CODE_VALUE(Type, Error).

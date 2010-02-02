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
-ifndef(smpp_base).
-define(smpp_base, true).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").  % Some global definitions
-include("smpp_base_syntax.hrl").   % The syntax used in this file

%%% COMMON MACROS
%% Specifies the SME address. For mobile terminated/originated messages,
%% this is the directory number of the recipient/originating MS. IP address
%% must be in "aaa.bbb.ccc.ddd" notation.
%%
%% Used on: destination_addr, dest_address, esme_addr, source_addr,
%% unsuccess_sme
-define(ADDR_21_DATATYPE, ?VAR_C_OCTET_STRING(21)).
-define(ADDR_21_DOMAIN,   ?VAR_C_OCTET_STRING(21)).
-define(ADDR_21_RESERVED, ?EMPTY).

-define(ADDR_65_DATATYPE, ?VAR_C_OCTET_STRING(65)).
-define(ADDR_65_DOMAIN,   ?VAR_C_OCTET_STRING(65)).
-define(ADDR_65_RESERVED, ?EMPTY).

%% Used on: dest_addr_subunit, source_addr_subunit
-define(ADDR_SUBUNIT_DATATYPE, ?INTEGER(1)).
-define(ADDR_SUBUNIT_DOMAIN,   ?BOUND_INTEGER(1, 16#04)).
-define(ADDR_SUBUNIT_RESERVED, ?RANGE_INTEGER(1, 16#05, 16#FF)).

%% Used on: dest_bearer_type, source_bearer_type
-define(BEARER_TYPE_DATATYPE, ?INTEGER(1)).
-define(BEARER_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 16#08)).
-define(BEARER_TYPE_RESERVED, ?RANGE_INTEGER(1, 16#09, 16#FF)).

%% A broadcast_area value should be defined using the broadcast_area record.
%%
%% Used on: broadcast_area_identifier, failed_broadcast_area_identifier.
-define(BROADCAST_AREA_FORMAT_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_AREA_FORMAT_DOMAIN,   ?BOUND_INTEGER(1, 16#02)).
-define(BROADCAST_AREA_FORMAT_RESERVED, ?RANGE_INTEGER(1, 16#03, 16#FF)).

-define(BROADCAST_AREA_DETAILS_DATATYPE, ?VAR_OCTET_STRING(100)).
-define(BROADCAST_AREA_DETAILS_DOMAIN,   ?VAR_OCTET_STRING(100)).
-define(BROADCAST_AREA_DETAILS_RESERVED, ?VAR_OCTET_STRING(100)).

-define(BROADCAST_AREA_DATATYPE,
        ?COMPOSITE(broadcast_area,
                   {?BROADCAST_AREA_FORMAT_DATATYPE,
                    ?BROADCAST_AREA_DETAILS_DATATYPE})).
-define(BROADCAST_AREA_DOMAIN,
        ?COMPOSITE(broadcast_area,
                   {?BROADCAST_AREA_FORMAT_DOMAIN,
                    ?BROADCAST_AREA_DETAILS_DOMAIN})).
-define(BROADCAST_AREA_RESERVED,
        ?COMPOSITE(broadcast_area,
                   {?BROADCAST_AREA_FORMAT_RESERVED,
                    ?BROADCAST_AREA_DETAILS_RESERVED})).

%% Used on: data_coding, callback_num_atag.
-define(ENCODING_SCHEME_DATATYPE, ?INTEGER(1)).
-define(ENCODING_SCHEME_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00001010),
		        ?RANGE_INTEGER(1, 2#00001101, 2#00001110),
                ?RANGE_INTEGER(1, 2#11000000, 2#11011111),
                ?RANGE_INTEGER(1, 2#11110000, 2#11111111)])).
-define(ENCODING_SCHEME_RESERVED,
        ?UNION([?RANGE_INTEGER(1, 2#00001011, 2#00001100),
                ?RANGE_INTEGER(1, 2#00001111, 2#10111111),
                ?RANGE_INTEGER(1, 2#11100000, 2#11101111)])).

%% Used by: message_id, receipted_message_id
-define(MESSAGE_IDENTIFIER_DATATYPE, ?VAR_C_OCTET_STRING(65)).
-define(MESSAGE_IDENTIFIER_DOMAIN,   ?VAR_C_OCTET_STRING(65)).
-define(MESSAGE_IDENTIFIER_RESERVED, ?EMPTY).

%% Used by: message_state_std, message_state_tlv
-define(MESSAGE_STATE_DATATYPE, ?INTEGER(1)).
-define(MESSAGE_STATE_DOMAIN,   ?BOUND_INTEGER(1, 9)).
-define(MESSAGE_STATE_RESERVED, ?EMPTY).

%% Unique address that may be derived and assigned by the node owner
%% without establishing a central assignment and management authority.
%%
%% Used on: dest_network_id, source_network_id
-define(NETWORK_ID_DATATYPE, ?VAR_C_OCTET_STRING(65)).
-define(NETWORK_ID_DOMAIN,   ?VAR_C_OCTET_STRING(65)).
-define(NETWORK_ID_RESERVED, ?EMPTY).

%% Used by: dest_network_type, source_network_type
-define(NETWORK_TYPE_DATATYPE, ?INTEGER(1)).
-define(NETWORK_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 16#08)).
-define(NETWORK_TYPE_RESERVED, ?RANGE_INTEGER(1, 16#09, 16#FF)).

%% Sequence of 6 decimal digits identifying the node.
%%
%% Used on: dest_node_id, source_node_id
-define(NODE_ID_DATATYPE, ?FIXED_DEC_OCTET_STRING(6)).
-define(NODE_ID_DOMAIN,   ?FIXED_DEC_OCTET_STRING(6)).
-define(NODE_ID_RESERVED, ?EMPTY).

%% **IMPORTANT NOTE:** Some operators in Central America use NPI values of 48.
%% If only stardard values should be accepted, replace the last ranges of
%% ``NPI_DOMAIN`` and ``NPI_RESERVED`` by the following:
%%
%% Standard domain range  ``?RANGE_INTEGER(1, 2#00010010, 2#00010010)]))``
%% Standard reserverd range ``?RANGE_INTEGER(1, 2#00010011, 2#11111111)]))``
%%
%% Used on: addr_npi, source_addr_npi, dest_addr_npi, esme_addr_npi,
%% dest_address, unsuccess_sme, callback_num
-define(NPI_DATATYPE, ?INTEGER(1)).
-define(NPI_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00000001),
                ?RANGE_INTEGER(1, 2#00000011, 2#00000100),
                ?RANGE_INTEGER(1, 2#00000110, 2#00000110),
                ?RANGE_INTEGER(1, 2#00001000, 2#00001010),
                ?RANGE_INTEGER(1, 2#00001110, 2#00001110),
                ?RANGE_INTEGER(1, 2#00010010, 2#00110000)])).

-define(NPI_RESERVED,
        ?UNION([?RANGE_INTEGER(1, 2#00000010, 2#00000010),
		        ?RANGE_INTEGER(1, 2#00000101, 2#00000101),
                ?RANGE_INTEGER(1, 2#00000111, 2#00000111),
                ?RANGE_INTEGER(1, 2#00001011, 2#00001101),
		        ?RANGE_INTEGER(1, 2#00110001, 2#11111111)])).


%% Used on: dest_port, source_port
-define(PORT_DATATYPE, ?INTEGER(2)).
-define(PORT_DOMAIN,   ?INTEGER(2)).
-define(PORT_RESERVED, ?EMPTY).

%% According to [3GPP TS 23.040] GSM values are listed below.
%%
%% On both, TDMA and CDMA, protocol_id is Ignored for mobile terminated
%% messages and set to NULL by the MC for mobile originated messages.
%%
%% [3GPP TS 23.040] recommends to reject messages with a TP-Protocol
%% Identifier containing a reserced value or one which is not supported.
%% That's why PROTOCOL_IDENTIFIER_RESERVED is set to  ``?EMPTY``.
%%
%% Used on: protocol_id
%%
%% **TODO** Review the domain declaration.  What about the SM-AL protocol
%% identifier values (when bits 7, 6 and 5 are 0).
-define(PROTOCOL_IDENTIFIER_DATATYPE, ?INTEGER(1)).
-define(PROTOCOL_IDENTIFIER_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00011111),
                ?RANGE_INTEGER(1, 2#00100000, 2#00101101),
                ?RANGE_INTEGER(1, 2#00110000, 2#00110010),
                ?RANGE_INTEGER(1, 2#00111000, 2#00111111),
                ?RANGE_INTEGER(1, 2#01000000, 2#01000111),
                ?RANGE_INTEGER(1, 2#01011110, 2#01011111),
                ?RANGE_INTEGER(1, 2#01111100, 2#01111111),
                ?RANGE_INTEGER(1, 2#11000000, 2#11111111)])). % SC use
-define(PROTOCOL_IDENTIFIER_RESERVED, ?EMPTY).

%% A list with all the SMPP Error Codes can be found on **smpp_globals.hrl**
%%
%% Used on: command_status, error_status_code, unsuccess_sme
-define(SMPP_ERROR_DATATYPE, ?INTEGER(4)).
-define(SMPP_ERROR_DOMAIN,
        ?UNION([?RANGE_INTEGER(4, 16#00000000, 16#00000008),
                ?RANGE_INTEGER(4, 16#0000000A, 16#0000000F),
                ?RANGE_INTEGER(4, 16#00000011, 16#00000011),
                ?RANGE_INTEGER(4, 16#00000013, 16#00000015),
                ?RANGE_INTEGER(4, 16#00000033, 16#00000034),
                ?RANGE_INTEGER(4, 16#00000040, 16#00000040),
                ?RANGE_INTEGER(4, 16#00000042, 16#00000045),
                ?RANGE_INTEGER(4, 16#00000048, 16#00000051),
                ?RANGE_INTEGER(4, 16#00000053, 16#00000055),
                ?RANGE_INTEGER(4, 16#00000058, 16#00000058),
                ?RANGE_INTEGER(4, 16#00000061, 16#00000067),
                ?RANGE_INTEGER(4, 16#000000C0, 16#000000C4),
                ?RANGE_INTEGER(4, 16#000000FE, 16#000000FF),
                ?RANGE_INTEGER(4, 16#00000100, 16#00000112)])).
-define(SMPP_ERROR_RESERVED,
        ?UNION([?RANGE_INTEGER(4, 16#00000009, 16#00000009),
                ?RANGE_INTEGER(4, 16#00000010, 16#00000010),
                ?RANGE_INTEGER(4, 16#00000012, 16#00000012),
                ?RANGE_INTEGER(4, 16#00000016, 16#00000032),
                ?RANGE_INTEGER(4, 16#00000035, 16#0000003F),
                ?RANGE_INTEGER(4, 16#00000041, 16#00000041),
                ?RANGE_INTEGER(4, 16#00000046, 16#00000047),
                ?RANGE_INTEGER(4, 16#00000052, 16#00000052),
                ?RANGE_INTEGER(4, 16#00000056, 16#00000057),
                ?RANGE_INTEGER(4, 16#00000059, 16#00000060),
                ?RANGE_INTEGER(4, 16#00000068, 16#000000BF),
                ?RANGE_INTEGER(4, 16#000000C5, 16#000000FD),
                ?RANGE_INTEGER(4, 16#00000113, 16#FFFFFFFF)])).

%% Used on: interface_version, sc_interface_version
-define(SMPP_VERSION_DATATYPE, ?INTEGER(1)).
-define(SMPP_VERSION_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 16#00, 16#34),
                ?CONSTANT(16#50)])).
-define(SMPP_VERSION_RESERVED,
        ?UNION([?RANGE_INTEGER(1, 16#35, 16#49),
                ?RANGE_INTEGER(1, 16#51, 16#FF)])).

%% A subaddress value should be defined using the subaddress record.
%%
%% Used on: dest_subaddress, source_subaddress
-define(SUBADDRESS_TAG_DATATYPE, ?INTEGER(1)).
-define(SUBADDRESS_TAG_DOMAIN,   ?SET([2#10000000, 2#10001000, 2#10100000])).
-define(SUBADDRESS_TAG_RESERVED,
        ?UNION([?RANGE_INTEGER(1, 2#00000001, 2#01111111),
                ?RANGE_INTEGER(1, 2#10000001, 2#10000111),
                ?RANGE_INTEGER(1, 2#10001001, 2#10011111),
                ?RANGE_INTEGER(1, 2#10100001, 2#11111111)])).

-define(SUBADDRESS_DATA_DATATYPE, ?VAR_OCTET_STRING(22)).
-define(SUBADDRESS_DATA_DOMAIN,   ?VAR_OCTET_STRING(22)).
-define(SUBADDRESS_DATA_RESERVED, ?VAR_OCTET_STRING(22)).

-define(SUBADDRESS_DATATYPE,
        ?COMPOSITE(subaddress,
                   {?SUBADDRESS_TAG_DATATYPE,
                    ?SUBADDRESS_DATA_DATATYPE})).
-define(SUBADDRESS_DOMAIN,
        ?COMPOSITE(subaddress,
                   {?SUBADDRESS_TAG_DOMAIN,
                    ?SUBADDRESS_DATA_DOMAIN})).
-define(SUBADDRESS_RESERVED,
        ?COMPOSITE(subaddress,
                   {?SUBADDRESS_TAG_RESERVED,
                    ?SUBADDRESS_DATA_RESERVED})).

%% A telematics_id value must be defined using the telematics_id record.
%%
%% Used on: dest_telematics_id, source_telematics_id
-define(TELEMATICS_ID_RESERVED_DATATYPE, ?INTEGER(1)).
-define(TELEMATICS_ID_RESERVED_DOMAIN,   ?INTEGER(1)).

-define(TELEMATICS_ID_DATATYPE,
        ?COMPOSITE(telematics_id,
                   {?PROTOCOL_IDENTIFIER_DATATYPE,
                    ?TELEMATICS_ID_RESERVED_DATATYPE})).
-define(TELEMATICS_ID_DOMAIN,
        ?COMPOSITE(telematics_id,
                   {?PROTOCOL_IDENTIFIER_DOMAIN,
                    ?TELEMATICS_ID_RESERVED_DOMAIN})).
-define(TELEMATICS_ID_RESERVED, ?EMPTY).


%% **IMPORTANT NOTE:** Some operators in Central America use TON values of 8.
%% If only stardard values should be accepted, replace the TON_DOMAIN and
%% TON_RESERVED by the following:
%%
%% ``-define(TON_DOMAIN,   ?BOUND_INTEGER(1, 2#00000110)).``
%% ``-define(TON_RESERVED, ?RANGE_INTEGER(1, 2#00000111, 2#11111111)).``
%%
%% Used on: addr_ton, source_addr_ton, dest_addr_ton, esme_addr_ton,
%% dest_address, unsuccess_sme, callback_num
-define(TON_DATATYPE, ?INTEGER(1)).
-define(TON_DOMAIN,   ?BOUND_INTEGER(1, 2#00000111)).
-define(TON_RESERVED, ?RANGE_INTEGER(1, 2#00001000, 2#11111111)).

%%% STANDARD FIELDS
-define(ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(ADDR_TON_RESERVED, ?TON_RESERVED).

-define(SOURCE_ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(SOURCE_ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(SOURCE_ADDR_TON_RESERVED, ?TON_RESERVED).

-define(DEST_ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(DEST_ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(DEST_ADDR_TON_RESERVED, ?TON_RESERVED).

-define(ESME_ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(ESME_ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(ESME_ADDR_TON_RESERVED, ?TON_RESERVED).

-define(ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(ADDR_NPI_RESERVED, ?NPI_RESERVED).

-define(SOURCE_ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(SOURCE_ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(SOURCE_ADDR_NPI_RESERVED, ?NPI_RESERVED).

-define(DEST_ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(DEST_ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(DEST_ADDR_NPI_RESERVED, ?NPI_RESERVED).

-define(ESME_ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(ESME_ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(ESME_ADDR_NPI_RESERVED, ?NPI_RESERVED).

%% - Single SME address
%% - Range of addresses using UNIX Regular Expression notation
-define(ADDRESS_RANGE_DATATYPE, ?VAR_C_OCTET_STRING(41)).
-define(ADDRESS_RANGE_DOMAIN,   ?VAR_C_OCTET_STRING(41)).
-define(ADDRESS_RANGE_RESERVED, ?EMPTY).

-define(ERROR_STATUS_CODE_DATATYPE, ?SMPP_ERROR_DATATYPE).
-define(ERROR_STATUS_CODE_DOMAIN,   ?SMPP_ERROR_DOMAIN).
-define(ERROR_STATUS_CODE_RESERVED, ?SMPP_ERROR_RESERVED).

-define(DATA_CODING_DATATYPE, ?ENCODING_SCHEME_DATATYPE).
-define(DATA_CODING_DOMAIN,   ?ENCODING_SCHEME_DOMAIN).
-define(DATA_CODING_RESERVED, ?ENCODING_SCHEME_RESERVED).

-define(DESTINATION_ADDR_21_DATATYPE, ?ADDR_21_DATATYPE).
-define(DESTINATION_ADDR_21_DOMAIN,   ?ADDR_21_DOMAIN).
-define(DESTINATION_ADDR_21_RESERVED, ?ADDR_21_RESERVED).

-define(DESTINATION_ADDR_65_DATATYPE, ?ADDR_65_DATATYPE).
-define(DESTINATION_ADDR_65_DOMAIN,   ?ADDR_65_DOMAIN).
-define(DESTINATION_ADDR_65_RESERVED, ?ADDR_65_RESERVED).

-define(DEST_FLAG_DATATYPE,   ?INTEGER(1)).
-define(DEST_FLAG_SME_DOMAIN, ?CONSTANT(16#01)).  % SME Address
-define(DEST_FLAG_DL_DOMAIN,  ?CONSTANT(16#02)).  % Distribution List Name
-define(DEST_FLAG_RESERVED,   ?EMPTY).

-define(DL_NAME_DATATYPE, ?VAR_C_OCTET_STRING(21)).
-define(DL_NAME_DOMAIN,   ?VAR_C_OCTET_STRING(21)).
-define(DL_NAME_RESERVED, ?EMPTY).

%% This field is a composite field containing a mandatory field and then
%% either an SME address or a Distribution List.  Additionally this field
%% can be encoded multiple times.
-define(DEST_ADDRESS_SME_DATATYPE,
        ?COMPOSITE(dest_address_sme,
                   {?DEST_FLAG_DATATYPE,
                    ?TON_DATATYPE,
                    ?NPI_DATATYPE,
                    ?ADDR_21_DATATYPE})).
-define(DEST_ADDRESS_SME_DOMAIN,
        ?COMPOSITE(dest_address_sme,
                   {?DEST_FLAG_SME_DOMAIN,
                    ?TON_DOMAIN,
                    ?NPI_DOMAIN,
                    ?ADDR_21_DOMAIN})).

-define(DEST_ADDRESS_DL_DATATYPE,
        ?COMPOSITE(dest_address_dl,
                   {?DEST_FLAG_DATATYPE,
                    ?DL_NAME_DATATYPE})).
-define(DEST_ADDRESS_DL_DOMAIN,
        ?COMPOSITE(dest_address_dl,
                   {?DEST_FLAG_DL_DOMAIN,
                    ?DL_NAME_DOMAIN})).

-define(DEST_ADDRESS_ITEM_DATATYPE,
        ?UNION([?DEST_ADDRESS_SME_DATATYPE,
                ?DEST_ADDRESS_DL_DATATYPE])).
-define(DEST_ADDRESS_ITEM_DOMAIN,
        ?UNION([?DEST_ADDRESS_SME_DOMAIN,
                ?DEST_ADDRESS_DL_DOMAIN])).

-define(DEST_ADDRESS_DATATYPE, ?LIST(?DEST_ADDRESS_ITEM_DATATYPE)).
-define(DEST_ADDRESS_DOMAIN,   ?LIST(?DEST_ADDRESS_ITEM_DOMAIN)).

%% Specifies the address of an ESME to which an alert_notification should
%% be routed.
-define(ESME_ADDR_DATATYPE, ?ADDR_65_DATATYPE).
-define(ESME_ADDR_DOMAIN,   ?ADDR_65_DOMAIN).
-define(ESME_ADDR_RESERVED, ?ADDR_65_RESERVED).

-define(ESM_CLASS_DATATYPE, ?INTEGER(1)).

-define(ESM_CLASS_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00000111),
                ?RANGE_INTEGER(1, 2#00100000, 2#00100011),
                ?RANGE_INTEGER(1, 2#00001000, 2#00001011),
                ?RANGE_INTEGER(1, 2#00010000, 2#00010011),
                ?RANGE_INTEGER(1, 2#00011000, 2#00011011),
                ?RANGE_INTEGER(1, 2#01000000, 2#01000111),
                ?RANGE_INTEGER(1, 2#01100000, 2#01100011),
                ?RANGE_INTEGER(1, 2#01001000, 2#01001011),
                ?RANGE_INTEGER(1, 2#01010000, 2#01010011),
                ?RANGE_INTEGER(1, 2#01011000, 2#01011011),
                ?RANGE_INTEGER(1, 2#10000000, 2#10000111),
                ?RANGE_INTEGER(1, 2#10100000, 2#10100011),
                ?RANGE_INTEGER(1, 2#10001000, 2#10001011),
                ?RANGE_INTEGER(1, 2#10010000, 2#10010011),
                ?RANGE_INTEGER(1, 2#11000000, 2#11000111),
                ?RANGE_INTEGER(1, 2#11100000, 2#11100011),
                ?RANGE_INTEGER(1, 2#11001000, 2#11001011),
                ?RANGE_INTEGER(1, 2#11010000, 2#11010011)])).
% never happens?      ?RANGE_INTEGER(1, 2#10011000, 2#10011011), CDMA & GSM?
% never happens?      ?RANGE_INTEGER(1, 2#11011000, 2#11011011), CDMA & GSM?
-define(ESM_CLASS_RESERVED, ?EMPTY).

-define(INTERFACE_VERSION_DATATYPE, ?SMPP_VERSION_DATATYPE).
-define(INTERFACE_VERSION_DOMAIN,   ?SMPP_VERSION_DOMAIN).
-define(INTERFACE_VERSION_RESERVED, ?SMPP_VERSION_RESERVED).

-define(MESSAGE_ID_DATATYPE, ?MESSAGE_IDENTIFIER_DATATYPE).
-define(MESSAGE_ID_DOMAIN,   ?MESSAGE_IDENTIFIER_DOMAIN).
-define(MESSAGE_ID_RESERVED, ?MESSAGE_IDENTIFIER_RESERVED).

-define(MESSAGE_STATE_STD_DATATYPE, ?MESSAGE_STATE_DATATYPE).
-define(MESSAGE_STATE_STD_DOMAIN,   ?MESSAGE_STATE_DOMAIN).
-define(MESSAGE_STATE_STD_RESERVED, ?MESSAGE_STATE_RESERVED).

%% The password is normally defined by the MC system administrator.
-define(PASSWORD_DATATYPE, ?VAR_C_OCTET_STRING(9)).
-define(PASSWORD_DOMAIN,   ?VAR_C_OCTET_STRING(9)).
-define(PASSWORD_RESERVED, ?EMPTY).

-define(PRIORITY_FLAG_DATATYPE, ?INTEGER(1)).
-define(PRIORITY_FLAG_DOMAIN,   ?BOUND_INTEGER(1, 4)).
-define(PRIORITY_FLAG_RESERVED, ?RANGE_INTEGER(1, 5, 255)).

-define(PROTOCOL_ID_DATATYPE, ?PROTOCOL_IDENTIFIER_DATATYPE).
-define(PROTOCOL_ID_DOMAIN,   ?PROTOCOL_IDENTIFIER_DOMAIN).
-define(PROTOCOL_ID_RESERVED, ?PROTOCOL_IDENTIFIER_RESERVED).

-define(REGISTERED_DELIVERY_DATATYPE, ?INTEGER(1)).
-define(REGISTERED_DELIVERY_DOMAIN,   ?BOUND_INTEGER(1, 2#00011111)).
-define(REGISTERED_DELIVERY_RESERVED,
        ?RANGE_INTEGER(1, 2#00100000, 2#11111111)).

-define(REPLACE_IF_PRESENT_FLAG_DATATYPE, ?INTEGER(1)).
-define(REPLACE_IF_PRESENT_FLAG_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(REPLACE_IF_PRESENT_FLAG_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

%% Either absolute or relative.
-define(SCHEDULE_DELIVERY_TIME_DATATYPE, ?FIXED_C_OCTET_STRING(17)).
-define(SCHEDULE_DELIVERY_TIME_DOMAIN,   ?UNION([?ATIME_C_OCTET_STRING,
		                                         ?RTIME_C_OCTET_STRING])).
-define(SCHEDULE_DELIVERY_TIME_RESERVED, ?EMPTY).

%% Either absolute or relative.
-define(VALIDITY_PERIOD_DATATYPE, ?FIXED_C_OCTET_STRING(17)).
-define(VALIDITY_PERIOD_DOMAIN,   ?UNION([?ATIME_C_OCTET_STRING,
                                          ?RTIME_C_OCTET_STRING])).
-define(VALIDITY_PERIOD_RESERVED, ?EMPTY).

%% It must be specified in absolute time format.
-define(FINAL_DATE_DATATYPE, ?FIXED_C_OCTET_STRING(17)).
-define(FINAL_DATE_DOMAIN,   ?ATIME_C_OCTET_STRING).
-define(FINAL_DATE_RESERVED, ?EMPTY).

-define(SERVICE_TYPE_DATATYPE, ?VAR_C_OCTET_STRING(6)).
-define(SERVICE_TYPE_DOMAIN,   ?VAR_C_OCTET_STRING(6)).
-define(SERVICE_TYPE_RESERVED, ?EMPTY).

-define(SHORT_MESSAGE_DATATYPE, ?LIST(?INTEGER(1))).
-define(SHORT_MESSAGE_DOMAIN,   ?LIST(?INTEGER(1))).
-define(SHORT_MESSAGE_RESERVED, ?EMPTY).

-define(SM_DEFAULT_MSG_ID_DATATYPE, ?INTEGER(1)).
-define(SM_DEFAULT_MSG_ID_DOMAIN,   ?INTEGER(1)).
-define(SM_DEFAULT_MSG_ID_RESERVED, ?EMPTY).

%% This field is implicitly encoded within short_message.
-define(SM_LENGTH_DATATYPE, ?INTEGER(1)).
-define(SM_LENGTH_DOMAIN,   ?INTEGER(1)).
-define(SM_LENGTH_RESERVED, ?EMPTY).

%% Specifies the address of the SME which originated this message.
-define(SOURCE_ADDR_21_DATATYPE, ?ADDR_21_DATATYPE).
-define(SOURCE_ADDR_21_DOMAIN,   ?ADDR_21_DOMAIN).
-define(SOURCE_ADDR_21_RESERVED, ?ADDR_21_RESERVED).

-define(SOURCE_ADDR_65_DATATYPE, ?ADDR_65_DATATYPE).
-define(SOURCE_ADDR_65_DOMAIN,   ?ADDR_65_DOMAIN).
-define(SOURCE_ADDR_65_RESERVED, ?ADDR_65_RESERVED).

%% Identifies an ESME or a MC at bind time.
-define(SYSTEM_ID_DATATYPE, ?VAR_C_OCTET_STRING(16)).
-define(SYSTEM_ID_DOMAIN,   ?VAR_C_OCTET_STRING(16)).
-define(SYSTEM_ID_RESERVED, ?EMPTY).

% Used to categorize the type of ESME that is binding to the MC.
%
% Some MCs may not require this parameter, in this case a NULL can be used.
-define(SYSTEM_TYPE_DATATYPE, ?VAR_C_OCTET_STRING(13)).
-define(SYSTEM_TYPE_DOMAIN,   ?VAR_C_OCTET_STRING(13)).
-define(SYSTEM_TYPE_RESERVED, ?EMPTY).

%% The range of values returned depends on the underlying
%% telecommunications network.
-define(ERROR_CODE_DATATYPE, ?INTEGER(1)).
-define(ERROR_CODE_DOMAIN,   ?INTEGER(1)).

%% This field is a composite field containing an SME address and an error
%% code.  Additionally this field can be encoded multiple times.
-define(UNSUCCESS_SME_ITEM_DATATYPE,
        ?COMPOSITE(unsuccess_sme,
                   {?TON_DATATYPE,
                    ?NPI_DATATYPE,
                    ?ADDR_21_DATATYPE,
                    ?SMPP_ERROR_DATATYPE})).
-define(UNSUCCESS_SME_ITEM_DOMAIN,
        ?COMPOSITE(unsuccess_sme,
                   {?TON_DOMAIN,
                    ?NPI_DOMAIN,
                    ?ADDR_21_DOMAIN,
                    ?SMPP_ERROR_DOMAIN})).

-define(UNSUCCESS_SME_DATATYPE, ?LIST(?UNSUCCESS_SME_ITEM_DATATYPE)).
-define(UNSUCCESS_SME_DOMAIN,   ?LIST(?UNSUCCESS_SME_ITEM_DOMAIN)).

%%% TLV FIELDS
%% Free format text.
-define(ADDITIONAL_STATUS_INFO_TEXT_DATATYPE, ?VAR_C_OCTET_STRING(256)).
-define(ADDITIONAL_STATUS_INFO_TEXT_DOMAIN,   ?VAR_C_OCTET_STRING(256)).
-define(ADDITIONAL_STATUS_INFO_TEXT_RESERVED, ?EMPTY).

-define(ALERT_ON_MESSAGE_DELIVERY_DATATYPE, ?INTEGER(1)).
-define(ALERT_ON_MESSAGE_DELIVERY_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(ALERT_ON_MESSAGE_DELIVERY_RESERVED, ?RANGE_INTEGER(1, 4, 255)).

%% The first octet represents the Billing Format tag and indicates the
%% format of the billing information in the remaining octets.
-define(BILLING_IDENTIFICATION_DATATYPE, ?VAR_OCTET_STRING(1024)).
-define(BILLING_IDENTIFICATION_DOMAIN,   ?VAR_OCTET_STRING(1024)).
-define(BILLING_IDENTIFICATION_RESERVED, ?EMPTY).

-define(BROADCAST_AREA_IDENTIFIER_DATATYPE,        ?BROADCAST_AREA_DATATYPE).
-define(BROADCAST_AREA_IDENTIFIER_DOMAIN,          ?BROADCAST_AREA_DOMAIN).
-define(BROADCAST_AREA_IDENTIFIER_RESERVED,        ?BROADCAST_AREA_RESERVED).

-define(FAILED_BROADCAST_AREA_IDENTIFIER_DATATYPE, ?BROADCAST_AREA_DATATYPE).
-define(FAILED_BROADCAST_AREA_IDENTIFIER_DOMAIN,   ?BROADCAST_AREA_DOMAIN).
-define(FAILED_BROADCAST_AREA_IDENTIFIER_RESERVED, ?BROADCAST_AREA_RESERVED).

%% - 0-100 = allowed range.
%% - 255 = Information not available
-define(BROADCAST_AREA_SUCCESS_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_AREA_SUCCESS_DOMAIN,
        ?UNION([?BOUND_INTEGER(1, 100), ?CONSTANT(255)])).
-define(BROADCAST_AREA_SUCCESS_RESERVED, ?RANGE_INTEGER(1, 101, 254)).

%% The value is a free format Octet String
-define(BROADCAST_CONTENT_TYPE_INFO_DATATYPE, ?VAR_OCTET_STRING(255)).
-define(BROADCAST_CONTENT_TYPE_INFO_DOMAIN,   ?VAR_OCTET_STRING(255)).
-define(BROADCAST_CONTENT_TYPE_INFO_RESERVED, ?EMPTY).

-define(BROADCAST_CHANNEL_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_CHANNEL_INDICATOR_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(BROADCAST_CHANNEL_INDICATOR_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

%% A broadcast_content_type value should be defined using the
%% broadcast_content_type record.
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_RESERVED, ?RANGE_INTEGER(1,4,255)).

-define(BROADCAST_CONTENT_TYPE_SERVICE_DATATYPE,      ?INTEGER(2)).
-define(BROADCAST_CONTENT_TYPE_SERVICE_DOMAIN,
        ?UNION([?RANGE_INTEGER(2, 16#0000, 16#0002),
                ?RANGE_INTEGER(2, 16#0010, 16#0023),
                ?RANGE_INTEGER(2, 16#0030, 16#0041),
                ?RANGE_INTEGER(2, 16#0070, 16#0071),
                ?RANGE_INTEGER(2, 16#0080, 16#0085),
                ?RANGE_INTEGER(2, 16#0100, 16#0100)])).
-define(BROADCAST_CONTENT_TYPE_SERVICE_RESERVED,
        ?UNION([?RANGE_INTEGER(2, 16#0003, 16#0009),
                ?RANGE_INTEGER(2, 16#0024, 16#002F),
                ?RANGE_INTEGER(2, 16#0042, 16#006F),
                ?RANGE_INTEGER(2, 16#0072, 16#007F),
                ?RANGE_INTEGER(2, 16#0086, 16#009F),
                ?RANGE_INTEGER(2, 16#0101, 16#FFFF)])).

-define(BROADCAST_CONTENT_TYPE_DATATYPE,
        ?COMPOSITE(broadcast_content_type,
                   {?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DATATYPE,
                    ?BROADCAST_CONTENT_TYPE_SERVICE_DATATYPE})).
-define(BROADCAST_CONTENT_TYPE_DOMAIN,
        ?COMPOSITE(broadcast_content_type,
                   {?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DOMAIN,
                    ?BROADCAST_CONTENT_TYPE_SERVICE_DOMAIN})).
-define(BROADCAST_CONTENT_TYPE_RESERVED,
        ?COMPOSITE(broadcast_content_type,
                   {?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_RESERVED,
                    ?BROADCAST_CONTENT_TYPE_SERVICE_RESERVED})).

%% It must be specified in absolute time format "YYMMDDhhmmsstnnp"
-define(BROADCAST_END_TIME_DATATYPE, ?FIXED_C_OCTET_STRING(17)).
-define(BROADCAST_END_TIME_DOMAIN,   ?ATIME_C_OCTET_STRING).
-define(BROADCAST_END_TIME_RESERVED, ?EMPTY).

%% The value is one of the SMPP Error Code Values as defined for
%% error_status_code
-define(BROADCAST_ERROR_STATUS_DATATYPE, ?ERROR_STATUS_CODE_DATATYPE).
-define(BROADCAST_ERROR_STATUS_DOMAIN,   ?ERROR_STATUS_CODE_DOMAIN).
-define(BROADCAST_ERROR_STATUS_RESERVED, ?ERROR_STATUS_CODE_RESERVED).

-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DOMAIN,
        ?UNION([?CONSTANT(16#00), ?RANGE_INTEGER(1, 16#08, 16#0E)])).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_RESERVED,
        ?UNION([?RANGE_INTEGER(1, 16#01, 16#07),
                ?RANGE_INTEGER(1, 16#0F, 16#FF)])).

-define(BROADCAST_FREQUENCY_INTERVAL_NUMBER_DATATYPE, ?INTEGER(2)).
-define(BROADCAST_FREQUENCY_INTERVAL_NUMBER_DOMAIN,   ?INTEGER(2)).
-define(BROADCAST_FREQUENCY_INTERVAL_NUMBER_RESERVED, ?INTEGER(2)).

-define(BROADCAST_FREQUENCY_INTERVAL_DATATYPE,
        ?COMPOSITE(broadcast_frequency_interval,
                   {?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DATATYPE,
                    ?BROADCAST_FREQUENCY_INTERVAL_NUMBER_DATATYPE})).
-define(BROADCAST_FREQUENCY_INTERVAL_DOMAIN,
        ?COMPOSITE(broadcast_frequency_interval,
                   {?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DOMAIN,
                    ?BROADCAST_FREQUENCY_INTERVAL_NUMBER_DOMAIN})).
-define(BROADCAST_FREQUENCY_INTERVAL_RESERVED,
        ?COMPOSITE(broadcast_frequency_interval,
                   {?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_RESERVED,
                    ?BROADCAST_FREQUENCY_INTERVAL_NUMBER_RESERVED})).

-define(BROADCAST_MESSAGE_CLASS_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_MESSAGE_CLASS_DOMAIN,   ?BOUND_INTEGER(1, 16#03)).
-define(BROADCAST_MESSAGE_CLASS_RESERVED, ?RANGE_INTEGER(1, 16#04, 16#FF)).

%% broadcast_rep_num
%%
%% The value 0 has the following significance:
%%
%% - If no validity_period has been specified for a broadcast, then the
%%   broadcasts should be repeated indefinitely.
%% - If a validity_period and a broadcast_frequency_interval have been
%%   specified, then 0 in this field indicates that the broadcast_rep_num is
%%   implicit according to the settings of these parameters.
%%
%% Where a broadcast priority (i.e. priority_flag setting) of 1 (Immediate
%% Broadcast) has been requested, then the broadcast_rep_num parameter should
%% not be supplied and be ignored if supplied.
-define(BROADCAST_REP_NUM_DATATYPE, ?INTEGER(2)).
-define(BROADCAST_REP_NUM_DOMAIN,   ?INTEGER(2)).
-define(BROADCAST_REP_NUM_RESERVED, ?EMPTY).

%% The value is a free format Octet String.
-define(BROADCAST_SERVICE_GROUP_DATATYPE, ?VAR_OCTET_STRING(255)).
-define(BROADCAST_SERVICE_GROUP_DOMAIN,   ?VAR_OCTET_STRING(255)).
-define(BROADCAST_SERVICE_GROUP_RESERVED, ?EMPTY).

%% A callback_num value should be defined using the callback_num record.
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_DOMAIN,   ?BOUND_INTEGER(1, 1)).

-define(CALLBACK_NUM_NUMBER_DIGITS_DATATYPE, ?VAR_OCTET_STRING(16)).
-define(CALLBACK_NUM_NUMBER_DIGITS_DOMAIN,   ?VAR_OCTET_STRING(16)).

-define(CALLBACK_NUM_DATATYPE,
        ?COMPOSITE(callback_num,
                   {?CALLBACK_NUM_DIGIT_MODE_INDICATOR_DATATYPE,
                    ?TON_DATATYPE,
                    ?NPI_DATATYPE,
                    ?CALLBACK_NUM_NUMBER_DIGITS_DATATYPE})).
-define(CALLBACK_NUM_DOMAIN,
        ?COMPOSITE(callback_num,
                   {?CALLBACK_NUM_DIGIT_MODE_INDICATOR_DOMAIN,
                    ?TON_DOMAIN,
                    ?NPI_DOMAIN,
                    ?CALLBACK_NUM_NUMBER_DIGITS_DOMAIN})).
-define(CALLBACK_NUM_RESERVED, ?EMPTY).

%% A callback_num_atag value should be defined using the callback_num record.
-define(CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DATATYPE, ?VAR_OCTET_STRING(64)).
-define(CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DOMAIN,   ?VAR_OCTET_STRING(64)).

-define(CALLBACK_NUM_ATAG_DATATYPE,
        ?COMPOSITE(callback_num_atag,
                   {?ENCODING_SCHEME_DATATYPE,
                     ?CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DATATYPE})).
-define(CALLBACK_NUM_ATAG_DOMAIN,
        ?COMPOSITE(callback_num_atag,
                   {?ENCODING_SCHEME_DOMAIN,
                     ?CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DOMAIN})).
-define(CALLBACK_NUM_ATAG_RESERVED, ?EMPTY).

-define(CALLBACK_NUM_PRES_IND_DATATYPE, ?INTEGER(1)).
-define(CALLBACK_NUM_PRES_IND_DOMAIN,
        ?RANGE_INTEGER(1, 2#00000000, 2#00001011)).
-define(CALLBACK_NUM_PRES_IND_RESERVED,
        ?RANGE_INTEGER(1, 2#00001100, 2#00001111)).

-define(CONGESTION_STATE_DATATYPE, ?INTEGER(1)).
-define(CONGESTION_STATE_DOMAIN,   ?BOUND_INTEGER(1, 99)).
-define(CONGESTION_STATE_RESERVED, ?RANGE_INTEGER(1, 100, 255)).

-define(DELIVERY_FAILURE_REASON_DATATYPE, ?INTEGER(1)).
-define(DELIVERY_FAILURE_REASON_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(DELIVERY_FAILURE_REASON_RESERVED, ?RANGE_INTEGER(1, 4, 255)).

%% A list with all countries and global destinations with a country codes
%% can be found on **e164.hrl**
-define(DEST_ADDR_NP_COUNTRY_DATATYPE, ?INTEGER(5)).
-define(DEST_ADDR_NP_COUNTRY_DOMAIN,   ?INTEGER(5)).
-define(DEST_ADDR_NP_COUNTRY_RESERVED, ?EMPTY).

%% **TODO** Review the domain declaration for this parameter.
-define(DEST_ADDR_NP_INFORMATION_DATATYPE, ?FIXED_OCTET_STRING(10)).
-define(DEST_ADDR_NP_INFORMATION_DOMAIN,   ?FIXED_OCTET_STRING(10)).
-define(DEST_ADDR_NP_INFORMATION_RESERVED, ?EMPTY).

%% **TODO** Confirm that there are no reserved values.
-define(DEST_ADDR_NP_RESOLUTION_DATATYPE, ?INTEGER(1)).
-define(DEST_ADDR_NP_RESOLUTION_DOMAIN,   ?BOUND_INTEGER(1, 2)).
-define(DEST_ADDR_NP_RESOLUTION_RESERVED, ?EMPTY).

-define(DEST_ADDR_SUBUNIT_DATATYPE, ?ADDR_SUBUNIT_DATATYPE).
-define(DEST_ADDR_SUBUNIT_DOMAIN,   ?ADDR_SUBUNIT_DOMAIN).
-define(DEST_ADDR_SUBUNIT_RESERVED, ?ADDR_SUBUNIT_RESERVED).

-define(DEST_BEARER_TYPE_DATATYPE, ?BEARER_TYPE_DATATYPE).
-define(DEST_BEARER_TYPE_DOMAIN,   ?BEARER_TYPE_DOMAIN).
-define(DEST_BEARER_TYPE_RESERVED, ?BEARER_TYPE_RESERVED).

%% When this TLV is specified, it must be accompanied with a dest_node_id TLV.
-define(DEST_NETWORK_ID_DATATYPE, ?NETWORK_ID_DATATYPE).
-define(DEST_NETWORK_ID_DOMAIN,   ?NETWORK_ID_DOMAIN).
-define(DEST_NETWORK_ID_RESERVED, ?NETWORK_ID_RESERVED).

-define(DEST_NETWORK_TYPE_DATATYPE, ?NETWORK_TYPE_DATATYPE).
-define(DEST_NETWORK_TYPE_DOMAIN,   ?NETWORK_TYPE_DOMAIN).
-define(DEST_NETWORK_TYPE_RESERVED, ?NETWORK_TYPE_RESERVED).

-define(DEST_NODE_ID_DATATYPE, ?NODE_ID_DATATYPE).
-define(DEST_NODE_ID_DOMAIN,   ?NODE_ID_DOMAIN).
-define(DEST_NODE_ID_RESERVED, ?NODE_ID_RESERVED).

-define(DEST_SUBADDRESS_DATATYPE, ?SUBADDRESS_DATATYPE).
-define(DEST_SUBADDRESS_DOMAIN,   ?SUBADDRESS_DOMAIN).
-define(DEST_SUBADDRESS_RESERVED, ?SUBADDRESS_RESERVED).

-define(DEST_TELEMATICS_ID_DATATYPE, ?TELEMATICS_ID_DATATYPE).
-define(DEST_TELEMATICS_ID_DOMAIN,   ?TELEMATICS_ID_DOMAIN).
-define(DEST_TELEMATICS_ID_RESERVED, ?TELEMATICS_ID_RESERVED).

-define(DEST_PORT_DATATYPE, ?PORT_DATATYPE).
-define(DEST_PORT_DOMAIN,   ?PORT_DOMAIN).
-define(DEST_PORT_RESERVED, ?PORT_RESERVED).

-define(DISPLAY_TIME_DATATYPE, ?INTEGER(1)).
-define(DISPLAY_TIME_DOMAIN,   ?BOUND_INTEGER(1, 2)).
-define(DISPLAY_TIME_RESERVED, ?RANGE_INTEGER(1, 3, 255)).

-define(DPF_RESULT_DATATYPE, ?INTEGER(1)).
-define(DPF_RESULT_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(DPF_RESULT_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

-define(ITS_REPLY_TYPE_DATATYPE, ?INTEGER(1)).
-define(ITS_REPLY_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 8)).
-define(ITS_REPLY_TYPE_RESERVED, ?RANGE_INTEGER(1, 9, 255)).

%% A its_session_info value should be defined using the its_session_info record.
-define(ITS_SESSION_INFO_SESSION_NUMBER_DATATYPE, ?INTEGER(1)).
-define(ITS_SESSION_INFO_SESSION_NUMBER_DOMAIN,   ?INTEGER(1)).

-define(ITS_SESSION_INFO_SEQUENCE_NUMBER_DATATYPE, ?INTEGER(1)).
-define(ITS_SESSION_INFO_SEQUENCE_NUMBER_DOMAIN,   ?INTEGER(1)).

-define(ITS_SESSION_INFO_DATATYPE,
        ?COMPOSITE(its_session_info,
                   {?ITS_SESSION_INFO_SESSION_NUMBER_DATATYPE,
                    ?ITS_SESSION_INFO_SEQUENCE_NUMBER_DATATYPE})).
-define(ITS_SESSION_INFO_DOMAIN,
        ?COMPOSITE(its_session_info,
                   {?ITS_SESSION_INFO_SESSION_NUMBER_DOMAIN,
                    ?ITS_SESSION_INFO_SEQUENCE_NUMBER_DOMAIN})).
-define(ITS_SESSION_INFO_RESERVED, ?EMPTY).

-define(LANGUAGE_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(LANGUAGE_INDICATOR_DOMAIN,   ?INTEGER(1)).
-define(LANGUAGE_INDICATOR_RESERVED, ?EMPTY).

%% The maximun size is MC and network implementation specific.
-define(MESSAGE_PAYLOAD_DATATYPE, ?VAR_OCTET_STRING(65536)).
-define(MESSAGE_PAYLOAD_DOMAIN,   ?VAR_OCTET_STRING(65536)).
-define(MESSAGE_PAYLOAD_RESERVED, ?EMPTY).

-define(MESSAGE_STATE_TLV_DATATYPE, ?MESSAGE_STATE_DATATYPE).
-define(MESSAGE_STATE_TLV_DOMAIN,   ?MESSAGE_STATE_DOMAIN).
-define(MESSAGE_STATE_TLV_RESERVED, ?MESSAGE_STATE_RESERVED).

-define(MORE_MESSAGES_TO_SEND_DATATYPE, ?INTEGER(1)).
-define(MORE_MESSAGES_TO_SEND_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(MORE_MESSAGES_TO_SEND_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

-define(MS_AVAILABILITY_STATUS_DATATYPE, ?INTEGER(1)).
-define(MS_AVAILABILITY_STATUS_DOMAIN,   ?BOUND_INTEGER(1, 2)).
-define(MS_AVAILABILITY_STATUS_RESERVED, ?RANGE_INTEGER(1, 3, 255)).

-define(MS_MSG_WAIT_FACILITIES_DATATYPE, ?INTEGER(1)).
-define(MS_MSG_WAIT_FACILITIES_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00000011),
                ?RANGE_INTEGER(1, 2#10000000, 2#10000011)])).
-define(MS_MSG_WAIT_FACILITIES_RESERVED, ?EMPTY).

%% A ms_validity value should be defined using the ms_validity_absolute
%% or ms_validity_relative record.
-define(MS_VALIDITY_TIME_BEHAVIOUR_DATATYPE, ?INTEGER(1)).
-define(MS_VALIDITY_ABSOLUTE_TIME_DOMAIN,    ?BOUND_INTEGER(1, 3)).
-define(MS_VALIDITY_RELATIVE_TIME_DOMAIN,    ?CONSTANT(4)).
-define(MS_VALIDITY_TIME_BEHAVIOUR_RESERVED, ?RANGE_INTEGER(1, 5, 255)).

-define(MS_VALIDITY_TIME_UNIT_DATATYPE, ?INTEGER(1)).
-define(MS_VALIDITY_TIME_UNIT_DOMAIN,   ?BOUND_INTEGER(1, 2#00000110)).
-define(MS_VALIDITY_TIME_UNIT_RESERVED,
        ?RANGE_INTEGER(1, 2#00000111, 2#11111111)).

-define(MS_VALIDITY_NUMBER_DATATYPE, ?INTEGER(2)).
-define(MS_VALIDITY_NUMBER_DOMAIN,   ?INTEGER(2)).
-define(MS_VALIDITY_NUMBER_RESERVED, ?INTEGER(2)).

-define(MS_VALIDITY_ABSOLUTE_DATATYPE,
        ?COMPOSITE(ms_validity_absolute,
                   {?MS_VALIDITY_TIME_BEHAVIOUR_DATATYPE})).
-define(MS_VALIDITY_ABSOLUTE_DOMAIN,
        ?COMPOSITE(ms_validity_absolute,
                   {?MS_VALIDITY_ABSOLUTE_TIME_DOMAIN})).
-define(MS_VALIDITY_ABSOLUTE_RESERVED,
        ?COMPOSITE(ms_validity_absolute,
                   {?MS_VALIDITY_TIME_BEHAVIOUR_RESERVED})).

-define(MS_VALIDITY_RELATIVE_DATATYPE,
        ?COMPOSITE(ms_validity_relative,
                   {?MS_VALIDITY_TIME_BEHAVIOUR_DATATYPE,
                    ?MS_VALIDITY_TIME_UNIT_DATATYPE,
                    ?MS_VALIDITY_NUMBER_DATATYPE})).
-define(MS_VALIDITY_RELATIVE_DOMAIN,
        ?COMPOSITE(ms_validity_relative,
                   {?MS_VALIDITY_RELATIVE_TIME_DOMAIN,
                    ?MS_VALIDITY_TIME_UNIT_DOMAIN,
                    ?MS_VALIDITY_NUMBER_DOMAIN})).
-define(MS_VALIDITY_RELATIVE_RESERVED,
        ?COMPOSITE(ms_validity_relative,
                   {?MS_VALIDITY_TIME_BEHAVIOUR_RESERVED,
                    ?MS_VALIDITY_TIME_UNIT_RESERVED,
                    ?MS_VALIDITY_NUMBER_RESERVED})).

-define(MS_VALIDITY_DATATYPE,
        ?UNION([?MS_VALIDITY_ABSOLUTE_DATATYPE,
                ?MS_VALIDITY_RELATIVE_DATATYPE])).
-define(MS_VALIDITY_DOMAIN,
        ?UNION([?MS_VALIDITY_ABSOLUTE_DOMAIN,
                ?MS_VALIDITY_RELATIVE_DOMAIN])).
-define(MS_VALIDITY_RESERVED,
        ?UNION([?MS_VALIDITY_ABSOLUTE_RESERVED,
                ?MS_VALIDITY_RELATIVE_RESERVED])).

%% A network_error_code value should be defined using the
%% network_error_code record.
-define(NETWORK_ERROR_CODE_TYPE_DATATYPE, ?INTEGER(1)).
-define(NETWORK_ERROR_CODE_TYPE_DOMAIN,   ?RANGE_INTEGER(1, 1, 8)).
-define(NETWORK_ERROR_CODE_TYPE_RESERVED,
        ?UNION([?CONSTANT(0), ?RANGE_INTEGER(1, 9, 255)])).

-define(NETWORK_ERROR_CODE_ERROR_DATATYPE, ?INTEGER(2)).
-define(NETWORK_ERROR_CODE_ERROR_DOMAIN,   ?INTEGER(2)).
-define(NETWORK_ERROR_CODE_ERROR_RESERVED, ?INTEGER(2)).

-define(NETWORK_ERROR_CODE_DATATYPE,
        ?COMPOSITE(network_error_code,
                   {?NETWORK_ERROR_CODE_TYPE_DATATYPE,
                    ?NETWORK_ERROR_CODE_ERROR_DATATYPE})).
-define(NETWORK_ERROR_CODE_DOMAIN,
        ?COMPOSITE(network_error_code,
                   {?NETWORK_ERROR_CODE_TYPE_DOMAIN,
                    ?NETWORK_ERROR_CODE_ERROR_DOMAIN})).
-define(NETWORK_ERROR_CODE_RESERVED,
        ?COMPOSITE(network_error_code,
                   {?NETWORK_ERROR_CODE_TYPE_RESERVED,
                    ?NETWORK_ERROR_CODE_ERROR_RESERVED})).

-define(NUMBER_OF_MESSAGES_DATATYPE, ?INTEGER(1)).
-define(NUMBER_OF_MESSAGES_DOMAIN,   ?BOUND_INTEGER(1, 99)).
-define(NUMBER_OF_MESSAGES_RESERVED, ?RANGE_INTEGER(1, 100, 255)).

-define(PAYLOAD_TYPE_DATATYPE, ?INTEGER(1)).
-define(PAYLOAD_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(PAYLOAD_TYPE_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

-define(PRIVACY_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(PRIVACY_INDICATOR_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(PRIVACY_INDICATOR_RESERVED, ?RANGE_INTEGER(1, 4, 255)).

%% If not present, the MC may apply a default value.
-define(QOS_TIME_TO_LIVE_DATATYPE, ?INTEGER(4)).
-define(QOS_TIME_TO_LIVE_DOMAIN,   ?INTEGER(4)).
-define(QOS_TIME_TO_LIVE_RESERVED, ?EMPTY).

-define(RECEIPTED_MESSAGE_ID_DATATYPE, ?MESSAGE_IDENTIFIER_DATATYPE).
-define(RECEIPTED_MESSAGE_ID_DOMAIN,   ?MESSAGE_IDENTIFIER_DOMAIN).
-define(RECEIPTED_MESSAGE_ID_RESERVED, ?MESSAGE_IDENTIFIER_RESERVED).

%% Current implementation automatically fills this field with the lower
%% order bytes of the sequence_number of the first segment.
-define(SAR_MSG_REF_NUM_DATATYPE, ?INTEGER(2)).
-define(SAR_MSG_REF_NUM_DOMAIN,   ?INTEGER(2)).
-define(SAR_MSG_REF_NUM_RESERVED, ?EMPTY).

%% A Value in the range 1 to 255 indicating the sequence number of a
%% particular message within the concatenated short message.
-define(SAR_SEGMENT_SEQNUM_DATATYPE, ?INTEGER(1)).
-define(SAR_SEGMENT_SEQNUM_DOMAIN,   ?RANGE_INTEGER(1, 1, 255)).
-define(SAR_SEGMENT_SEQNUM_RESERVED, ?EMPTY).

%% A Value in the range 1 to 255 indicating the total number of fragments
%% within the concatenated short message.
-define(SAR_TOTAL_SEGMENTS_DATATYPE, ?INTEGER(1)).
-define(SAR_TOTAL_SEGMENTS_DOMAIN,   ?RANGE_INTEGER(1, 1, 255)).
-define(SAR_TOTAL_SEGMENTS_RESERVED, ?EMPTY).

-define(SC_INTERFACE_VERSION_DATATYPE, ?SMPP_VERSION_DATATYPE).
-define(SC_INTERFACE_VERSION_DOMAIN,   ?SMPP_VERSION_DOMAIN).
-define(SC_INTERFACE_VERSION_RESERVED, ?SMPP_VERSION_RESERVED).

-define(SET_DPF_DATATYPE, ?INTEGER(1)).
-define(SET_DPF_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(SET_DPF_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

-define(SMS_SIGNAL_DATATYPE, ?INTEGER(2)).
-define(SMS_SIGNAL_DOMAIN,   ?INTEGER(2)).
-define(SMS_SIGNAL_RESERVED, ?EMPTY).

-define(SOURCE_ADDR_SUBUNIT_DATATYPE, ?ADDR_SUBUNIT_DATATYPE).
-define(SOURCE_ADDR_SUBUNIT_DOMAIN,   ?ADDR_SUBUNIT_DOMAIN).
-define(SOURCE_ADDR_SUBUNIT_RESERVED, ?ADDR_SUBUNIT_RESERVED).

-define(SOURCE_BEARER_TYPE_DATATYPE, ?BEARER_TYPE_DATATYPE).
-define(SOURCE_BEARER_TYPE_DOMAIN,   ?BEARER_TYPE_DOMAIN).
-define(SOURCE_BEARER_TYPE_RESERVED, ?BEARER_TYPE_RESERVED).

%% When this TLV is specified, it must be accompanied with a source_node_id TLV.
-define(SOURCE_NETWORK_ID_DATATYPE, ?NETWORK_ID_DATATYPE).
-define(SOURCE_NETWORK_ID_DOMAIN,   ?NETWORK_ID_DOMAIN).
-define(SOURCE_NETWORK_ID_RESERVED, ?NETWORK_ID_RESERVED).

-define(SOURCE_NETWORK_TYPE_DATATYPE, ?NETWORK_TYPE_DATATYPE).
-define(SOURCE_NETWORK_TYPE_DOMAIN,   ?NETWORK_TYPE_DOMAIN).
-define(SOURCE_NETWORK_TYPE_RESERVED, ?NETWORK_TYPE_RESERVED).

-define(SOURCE_NODE_ID_DATATYPE, ?NODE_ID_DATATYPE).
-define(SOURCE_NODE_ID_DOMAIN,   ?NODE_ID_DOMAIN).
-define(SOURCE_NODE_ID_RESERVED, ?NODE_ID_RESERVED).

-define(SOURCE_PORT_DATATYPE, ?PORT_DATATYPE).
-define(SOURCE_PORT_DOMAIN,   ?PORT_DOMAIN).
-define(SOURCE_PORT_RESERVED, ?PORT_RESERVED).

-define(SOURCE_SUBADDRESS_DATATYPE, ?SUBADDRESS_DATATYPE).
-define(SOURCE_SUBADDRESS_DOMAIN,   ?SUBADDRESS_DOMAIN).
-define(SOURCE_SUBADDRESS_RESERVED, ?SUBADDRESS_RESERVED).

-define(SOURCE_TELEMATICS_ID_DATATYPE, ?TELEMATICS_ID_DATATYPE).
-define(SOURCE_TELEMATICS_ID_DOMAIN,   ?TELEMATICS_ID_DOMAIN).
-define(SOURCE_TELEMATICS_ID_RESERVED, ?TELEMATICS_ID_RESERVED).

-define(USER_MESSAGE_REFERENCE_DATATYPE, ?INTEGER(2)).
-define(USER_MESSAGE_REFERENCE_DOMAIN,   ?INTEGER(2)).
-define(USER_MESSAGE_REFERENCE_RESERVED, ?EMPTY).

%% Application specific.
%%
%% - 0 to 255 (IS-96 CDMA)
%% - 0 to 15 (CMT-136 TDMA)
-define(USER_RESPONSE_CODE_DATATYPE,    ?INTEGER(1)).
-define(USER_RESPONSE_CODE_CDMA_DOMAIN, ?INTEGER(1)).
-define(USER_RESPONSE_CODE_TDMA_DOMAIN, ?BOUND_INTEGER(1, 15)).
-define(USER_RESPONSE_CODE_RESERVED,    ?EMPTY).

-define(USSD_SERVICE_OP_DATATYPE, ?INTEGER(1)).
-define(USSD_SERVICE_OP_DOMAIN,
        ?UNION([?RANGE_INTEGER(1,  0,   3),
                ?RANGE_INTEGER(1, 16,  19),
                ?RANGE_INTEGER(1, 32, 255)])).
-define(USSD_SERVICE_OP_RESERVED,
        ?UNION([?RANGE_INTEGER(1,  4,  15),
                ?RANGE_INTEGER(1, 20,  31)])).

%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% Standard and TLV Composite Parameters Value Definitions
%%%
%%% %@see section 4.1 to 4.6 on [SMPP 5.0]
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-define(DEST_ADDRESS_SME_DEFAULT_VALUE, #dest_address_sme{}).
-define(DEST_ADDRESS_SME_VALUE(DestFlag, DestAddrTon, DestAddrNpi, DestAddr),
        #dest_address_sme{dest_flag        = DestFlag,
                          dest_addr_ton    = DestAddrTon,
                          dest_addr_npi    = DestAddrNpi,
                          destination_addr = DestAddr}).

-define(DEST_ADDRESS_DL_DEFAULT_VALUE, #dest_address_dl{}).
-define(DEST_ADDRESS_DL_VALUE(DestFlag, DlName),
        #dest_address_dl{dest_flag = DestFlag, dl_name = DlName}).

-define(UNSUCCESS_SME_DEFAULT_VALUE, #unsuccess_sme{}).
-define(UNSUCCESS_SME_VALUE(DestAddrTon, DestAddrNpi, DestAddr, StatusCode),
        #unsuccess_sme{dest_addr_ton     = DestAddrTon,
                       dest_addr_npi     = DestAddrNpi,
                       destination_addr  = DestAddr,
                       error_status_code = StatusCode}).

-define(BROADCAST_AREA_DEFAULT_VALUE, #broadcast_area{}).
-define(BROADCAST_AREA_VALUE(Format, Details),
        #broadcast_area{format = Format, details = Details}).

-define(BROADCAST_CONTENT_TYPE_DEFAULT_VALUE, #broadcast_content_type{}).
-define(BROADCAST_CONTENT_TYPE_VALUE(NetworkType, Service),
        #broadcast_content_type{network_type = NetworkType,service = Service}).

-define(BROADCAST_FREQUENCY_INTERVAL_DEFAULT_VALUE,
        #broadcast_frequency_interval{}).
-define(BROADCAST_FREQUENCY_INTERVAL_VALUE(TimeUnit, Number),
        #broadcast_frequency_interval{time_unit = TimeUnit, number = Number}).

-define(SUBADDRESS_DEFAULT_VALUE, #subaddress{}).
-define(SUBADDRESS_VALUE(Tag, Data), #subaddress{tag = Tag, data = Data}).

-define(CALLBACK_NUM_DEFAULT_VALUE, #callback_num{}).
-define(CALLBACK_NUM_VALUE(DigitModeIndicator, AddrTon, AddrNpi, NumberDigits),
        #callback_num{digit_mode_indicator = DigitModeIndicator,
                      addr_ton             = AddrTon,
                      addr_npi             = AddrNpi,
                      number_digits        = NumberDigits}).

-define(CALLBACK_NUM_ATAG_DEFAULT_VALUE, #callback_num_atag{}).
-define(CALLBACK_NUM_ATAG_VALUE(DataCoding, DisplayCharacters),
        #callback_num_atag{data_coding        = DataCoding,
                           display_characters = DisplayCharacters}).

-define(TELEMATICS_ID_DEFAULT_VALUE, #telematics_id{}).
-define(TELEMATICS_ID_VALUE(ProtocolId, Reserved),
        #telematics_id{protocol_id = ProtocolId, reserved = Reserved}).

-define(ITS_SESSION_INFO_DEFAULT_VALUE, #its_session_info{}).
-define(ITS_SESSION_INFO_VALUE(SessionNumber, SequenceNumber),
        #its_session_info{session_number  = SessionNumber,
                          sequence_number = SequenceNumber}).

-define(MS_VALIDITY_ABSOLUTE_DEFAULT_VALUE, #ms_validity_absolute{}).
-define(MS_VALIDITY_ABSOLUTE_VALUE(Behaviour),
        #ms_validity_absolute{behaviour = Behaviour}).

-define(MS_VALIDITY_RELATIVE_DEFAULT_VALUE, #ms_validity_relative{}).
-define(MS_VALIDITY_RELATIVE_VALUE(Behaviour, TimeUnit, Number),
        #ms_validity_relative{behaviour = Behaviour,
                              time_unit = TimeUnit,
                              number    = Number}).

-define(NETWORK_ERROR_CODE_DEFAULT_VALUE, #network_error_code{}).
-define(NETWORK_ERROR_CODE_VALUE(Type, Error),
        #network_error_code{type = Type, error = Error}).

-endif.  % -ifndef(smpp_base)

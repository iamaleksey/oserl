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
-ifndef(smpp_globals).
-define(smpp_globals, true).

%%% COMMON VALUES
% addr_subunit Values
-define(ADDR_SUBUNIT_UNKNOWN,          16#00). % Default
-define(ADDR_SUBUNIT_MS_DISPLAY,       16#01).
-define(ADDR_SUBUNIT_MOBILE_EQUIPMENT, 16#02).
-define(ADDR_SUBUNIT_SMART_CARD_1,     16#03). % Expected to be SIM
-define(ADDR_SUBUNIT_EXTERNAL_UNIT_2,  16#04).

% bearer_type Values
-define(BEARER_TYPE_UNKNOWN,        16#00).
-define(BEARER_TYPE_SMS,            16#01).
-define(BEARER_TYPE_CSD,            16#02).  % Circuit Switched Data
-define(BEARER_TYPE_PACKET_DATA,    16#03).
-define(BEARER_TYPE_USSD,           16#04).
-define(BEARER_TYPE_CDPD,           16#05).
-define(BEARER_TYPE_DATATAC,        16#06).  % DataTAC
-define(BEARER_TYPE_FLEX_REFLEX,    16#07).  % FLEX/ReFLEX
-define(BEARER_TYPE_CELL_BROADCAST, 16#08).  % Cell Broadcast (cell cast)

% broadcast_area_format Values
-define(BROADCAST_AREA_FORMAT_ALIAS,         16#00).
-define(BROADCAST_AREA_FORMAT_ELLIPSOID_ARC, 16#01).
-define(BROADCAST_AREA_FORMAT_POLYGON,       16#02).

% command_id Values
%
% Don't forget to update VALID_COMMAND_ID macro below if you remove or
% add any command_id.
% %@see section 4.7.5 on [SMPP 5.0]
-define(COMMAND_ID_BIND_RECEIVER,            16#00000001).
-define(COMMAND_ID_BIND_TRANSMITTER,         16#00000002).
-define(COMMAND_ID_QUERY_SM,                 16#00000003).
-define(COMMAND_ID_SUBMIT_SM,                16#00000004).
-define(COMMAND_ID_DELIVER_SM,               16#00000005).
-define(COMMAND_ID_UNBIND,                   16#00000006).
-define(COMMAND_ID_REPLACE_SM,               16#00000007).
-define(COMMAND_ID_CANCEL_SM,                16#00000008).
-define(COMMAND_ID_BIND_TRANSCEIVER,         16#00000009).
-define(COMMAND_ID_OUTBIND,                  16#0000000B).
-define(COMMAND_ID_ENQUIRE_LINK,             16#00000015).
-define(COMMAND_ID_SUBMIT_MULTI,             16#00000021).
-define(COMMAND_ID_ALERT_NOTIFICATION,       16#00000102).
-define(COMMAND_ID_DATA_SM,                  16#00000103).
-define(COMMAND_ID_BROADCAST_SM,             16#00000111).
-define(COMMAND_ID_QUERY_BROADCAST_SM,       16#00000112).
-define(COMMAND_ID_CANCEL_BROADCAST_SM,      16#00000113).
-define(COMMAND_ID_GENERIC_NACK,             16#80000000).
-define(COMMAND_ID_BIND_RECEIVER_RESP,       16#80000001).
-define(COMMAND_ID_BIND_TRANSMITTER_RESP,    16#80000002).
-define(COMMAND_ID_QUERY_SM_RESP,            16#80000003).
-define(COMMAND_ID_SUBMIT_SM_RESP,           16#80000004).
-define(COMMAND_ID_DELIVER_SM_RESP,          16#80000005).
-define(COMMAND_ID_UNBIND_RESP,              16#80000006).
-define(COMMAND_ID_REPLACE_SM_RESP,          16#80000007).
-define(COMMAND_ID_CANCEL_SM_RESP,           16#80000008).
-define(COMMAND_ID_BIND_TRANSCEIVER_RESP,    16#80000009).
-define(COMMAND_ID_ENQUIRE_LINK_RESP,        16#80000015).
-define(COMMAND_ID_SUBMIT_MULTI_RESP,        16#80000021).
-define(COMMAND_ID_DATA_SM_RESP,             16#80000103).
-define(COMMAND_ID_BROADCAST_SM_RESP,        16#80000111).
-define(COMMAND_ID_QUERY_BROADCAST_SM_RESP,  16#80000112).
-define(COMMAND_ID_CANCEL_BROADCAST_SM_RESP, 16#80000113).

% command_status Values
%
% %@see section 4.7.6 on [SMPP 5.0]
-define(ESME_ROK,                 16#00000000). % No Error
-define(ESME_RINVMSGLEN,          16#00000001). % Message Length is invalid
-define(ESME_RINVCMDLEN,          16#00000002). % Command Length is invalid
-define(ESME_RINVCMDID,           16#00000003). % Invalid Command ID
-define(ESME_RINVBNDSTS,          16#00000004). % Incorrect BIND Status for
                                                % given command
-define(ESME_RALYBND,             16#00000005). % ESME Already in Bound State
-define(ESME_RINVPRTFLG,          16#00000006). % Invalid Priority Flag
-define(ESME_RINVREGDLVFLG,       16#00000007). % Invalid Registered Delivery
                                                % Flag
-define(ESME_RSYSERR,             16#00000008). % System Error
-define(ESME_RINVSRCADR,          16#0000000A). % Invalid Source Address
-define(ESME_RINVDSTADR,          16#0000000B). % Invalid Dest Addr
-define(ESME_RINVMSGID,           16#0000000C). % Message ID is invalid
-define(ESME_RBINDFAIL,           16#0000000D). % Bind Failed
-define(ESME_RINVPASWD,           16#0000000E). % Invalid Password
-define(ESME_RINVSYSID,           16#0000000F). % Invalid System ID
-define(ESME_RCANCELFAIL,         16#00000011). % Cancel SM Failed
-define(ESME_RREPLACEFAIL,        16#00000013). % Replace SM Failed
-define(ESME_RMSGQFUL,            16#00000014). % Message Queue Full
-define(ESME_RINVSERTYP,          16#00000015). % Invalid Service Type
-define(ESME_RINVNUMDESTS,        16#00000033). % Invalid destinations number
-define(ESME_RINVDLNAME,          16#00000034). % Invalid Distribution List
                                                % name
-define(ESME_RINVDESTFLAG,        16#00000040). % Invalid Destination flag
                                                % (submit_multi)
-define(ESME_RINVSUBREP,          16#00000042). % Invalid submit with replace
-define(ESME_RINVESMCLASS,        16#00000043). % Invalid esm_class field data
-define(ESME_RCNTSUBDL,           16#00000044). % Cannot Submit to Distribution
                                                % List
-define(ESME_RSUBMITFAIL,         16#00000045). % submit_sm or submit_multi
                                                % failed
-define(ESME_RINVSRCTON,          16#00000048). % Invalid Source address TON
-define(ESME_RINVSRCNPI,          16#00000049). % Invalid Source address NPI
-define(ESME_RINVDSTTON,          16#00000050). % Invalid Destination addr TON
-define(ESME_RINVDSTNPI,          16#00000051). % Invalid Destination addr NPI
-define(ESME_RINVSYSTYP,          16#00000053). % Invalid system_type field
-define(ESME_RINVREPFLAG,         16#00000054). % Invalid replace_if_present
                                                % Flag
-define(ESME_RINVNUMMSGS,         16#00000055). % Invalid number of messages
-define(ESME_RTHROTTLED,          16#00000058). % Throttling error (ESME has
                                                % exceeded allowed msg limits)
-define(ESME_RINVSCHED,           16#00000061). % Invalid Scheduled Delivery
                                                % Time
-define(ESME_RINVEXPIRY,          16#00000062). % Invalid message validity
                                                % period (Expiry time)
-define(ESME_RINVDFTMSGID,        16#00000063). % Predefined Message Invalid or
                                                % Not Found
-define(ESME_RX_T_APPN,           16#00000064). % ESME Receiver Temporary App Err
-define(ESME_RX_P_APPN,           16#00000065). % ESME Receiver Permanent App Err
-define(ESME_RX_R_APPN,           16#00000066). % ESME Receiver Reject Message
-define(ESME_RQUERYFAIL,          16#00000067). % query_sm request failed
-define(ESME_RINVTLVSTREAM,       16#000000C0). % Error in the optional part
                                                % of the PDU Body
-define(ESME_RTLVNOTALLWD,        16#000000C1). % TLV not allowed
-define(ESME_RINVTLVLEN,          16#000000C2). % Invalid Parameter Length
-define(ESME_RMISSINGTLV,         16#000000C3). % Expected TLV missing
-define(ESME_RINVTLVVAL,          16#000000C4). % Invalid TLV Value
-define(ESME_RDELIVERYFAILURE,    16#000000FE). % Transaction Delivery Failure
-define(ESME_RUNKNOWNERR,         16#000000FF). % Unknown Error
-define(ESME_RSERTYPUNAUTH,       16#00000100). % ESME Not authorised to use
                                                % specified service_type
-define(ESME_RPROHIBITED,         16#00000101). % ESME Prohibited from using
                                                % specified operation
-define(ESME_RSERTYPUNAVAIL,      16#00000102). % Specified service_type is
                                                % unavailable
-define(ESME_RSERTYPDENIED,       16#00000103). % Specified service_type denied
-define(ESME_RINVDCS,             16#00000104). % Invalid Data Coding Scheme
-define(ESME_RINVSRCADDRSUBUNIT,  16#00000105). % Source Address Sub unit
                                                % is invalid
-define(ESME_RINVDSTADDRSUBUNIT,  16#00000106). % Destination Address Sub unit
                                                % is invalid
-define(ESME_RINVBCASTFREQINT,    16#00000107). % Broadcast Frequency Interval
                                                % is invalid
-define(ESME_RINVBCASTALIAS_NAME, 16#00000108). % Invalid Broadcast Alias Name
-define(ESME_RINVBCASTAREAFMT,    16#00000109). % Invalid Broadcast Area Format
-define(ESME_RINVNUMBCAST_AREAS,  16#0000010A). % Number of Broadcast Areas
                                                % is invalid
-define(ESME_RINVBCASTCNTTYPE,    16#0000010B). % Invalid Broadcast Content
                                                % Type
-define(ESME_RINVBCASTMSGCLASS,   16#0000010C). % Broadcast Message Class
                                                % is invalid
-define(ESME_RBCASTFAIL,          16#0000010D). % broadcast_sm operation failed
-define(ESME_RBCASTQUERYFAIL,     16#0000010E). % query_broadcast_sm failed
-define(ESME_RBCASTCANCELFAIL,    16#0000010F). % cancel_broadcast_sm failed
-define(ESME_RINVBCAST_REP,       16#00000110). % Number of Repeated Broadcasts
                                                % is invalid
-define(ESME_RINVBCASTSRVGRP,     16#00000111). % Broadcast Service Group
                                                % is invalid
-define(ESME_RINVBCASTCHANIND,    16#00000112). % Broadcast Channel Indicator
                                                % is invalid

% SMPP 3.4 Error Code Synonyms
%
% **deprecated** For backward compatibility only.
-define(ESME_RINVOPTPARSTREAM,    16#000000C0). % Error in the optional part of
                                                % the PDU Body
-define(ESME_ROPTPARNOTALLWD,     16#000000C1). % Optional Parameter not
                                                % allowed
-define(ESME_RINVPARLEN,          16#000000C2). % Invalid Parameter Length
-define(ESME_RMISSINGOPTPARAM,    16#000000C3). % Expected Optional Parameter
                                                % missing
-define(ESME_RINVOPTPARAMVAL,     16#000000C4). % Invalid Optional Parameter
                                                % Value

% encoding_scheme Values
-define(ENCODING_SCHEME_MC_SPECIFIC,  2#00000000). % Default alphabet assumed
                                                   % by MC
-define(ENCODING_SCHEME_IA5_ASCII,    2#00000001). % IA5 (CCITT T.50)/ASCII
                                                   % (ANSI X3.4)
-define(ENCODING_SCHEME_OCTET,        2#00000010). % Octet unspecified
-define(ENCODING_SCHEME_LATIN_1,      2#00000011). % Latin 1 (ISO-8859-1)
-define(ENCODING_SCHEME_BINARY,       2#00000100). % 8-bit binary: WAP, Logos,
                                                   % tones ...
-define(ENCODING_SCHEME_JIS,          2#00000101). % X 0208-1990
-define(ENCODING_SCHEME_CYRILLIC,     2#00000110). % Cyrillic (ISO-8859-5)
-define(ENCODING_SCHEME_LATIN_HEBREW, 2#00000111). % Latin/Hebrew (ISO-8859-8)
-define(ENCODING_SCHEME_UCS2,         2#00001000). % ISO/IEC-10646
-define(ENCODING_SCHEME_PICTOGRAM,    2#00001001). % Pictogram Encoding
-define(ENCODING_SCHEME_ISO_2022_JP,  2#00001010). % Music Codes
-define(ENCODING_SCHEME_KANJI_JIS,    2#00001101). % Extended Kanji JIS,
                                                   % (X 0212-1990)
-define(ENCODING_SCHEME_KS_C_5601,    2#00001110). % KS C 5601

% message_state Values
-define(MESSAGE_STATE_SCHEDULED,     0).  % Intermediate
-define(MESSAGE_STATE_ENROUTE,       1).  % Intermediate
-define(MESSAGE_STATE_DELIVERED,     2).  % Final
-define(MESSAGE_STATE_EXPIRED,       3).  % Final
-define(MESSAGE_STATE_DELETED,       4).  % Final
-define(MESSAGE_STATE_UNDELIVERABLE, 5).  % Final
-define(MESSAGE_STATE_ACCEPTED,      6).  % Final
-define(MESSAGE_STATE_UNKNOWN,       7).  % N/A
-define(MESSAGE_STATE_REJECTED,      8).  % Final
-define(MESSAGE_STATE_SKIPPED,       9).  % Final

% network_type Values
-define(NETWORK_TYPE_UNKNOWN,        16#00).
-define(NETWORK_TYPE_GENERIC,        16#00).
-define(NETWORK_TYPE_GSM,            16#01).
-define(NETWORK_TYPE_TDMA,           16#02).  % ANSI-136/TDMA
-define(NETWORK_TYPE_CDMA,           16#03).  % IS-95/CDMA
-define(NETWORK_TYPE_PDC,            16#04).
-define(NETWORK_TYPE_PHS,            16#05).
-define(NETWORK_TYPE_IDEN,           16#06).  % iDEN
-define(NETWORK_TYPE_AMPS,           16#07).
-define(NETWORK_TYPE_PAGING_NETWORK, 16#08).

% npi Values
-define(NPI_UNKNOWN,       2#00000000).
-define(NPI_ISDN,          2#00000001). % E163/E164
-define(NPI_DATA,          2#00000011). % X.121
-define(NPI_TELEX,         2#00000100). % F.69
-define(NPI_LAND_MOBILE,   2#00000110). % E.212
-define(NPI_NATIONAL,      2#00001000).
-define(NPI_PRIVATE,       2#00001001).
-define(NPI_ERMES,         2#00001010).
-define(NPI_INTERNET,      2#00001110). % IP
-define(NPI_WAP_CLIENT_ID, 2#00010010). % To be defined by WAP Forum

% protocol_id Values
-define(PROTOCOL_ID_GSM,  ?NULL_INTEGER).
-define(PROTOCOL_ID_TDMA, ?NULL_INTEGER).
-define(PROTOCOL_ID_CDMA, ?NULL_INTEGER).

% subaddress_tag Values
-define(SUBADDRESS_TAG_NSAP_EVEN, 2#10000000). % NSAP (Even) [ITUT X.213]
-define(SUBADDRESS_TAG_NSAP_ODD,  2#10001000). % NSAP (Odd) [ITUT X.213]
-define(SUBADDRESS_TAG_USER,      2#10100000). % User Specific

% ton Values
-define(TON_UNKNOWN,           2#00000000).
-define(TON_INTERNATIONAL,     2#00000001).
-define(TON_NATIONAL,          2#00000010).
-define(TON_NETWORK_SPECIFIC,  2#00000011).
-define(TON_SUBSCRIBER_NUMBER, 2#00000100).
-define(TON_ALPHANUMERIC,      2#00000101).
-define(TON_ABBREVIATED,       2#00000110).

%%% PDU FIELD VALUES
% dest_flag Values
-define(DEST_FLAG_SME, 16#01).  % SME Address
-define(DEST_FLAG_DL,  16#02).  % Distribution List Name

% esm_class Values
-define(ESM_CLASS_DEFAULT, ?NULL_INTEGER).

% esm_class Bits Values
%
% To compound the complete esm_class value from the Bits, pick a Bit Value
% from every type and do the bitwise "or" of the parts.
%
% ```
% ?ESM_CLASS_MODE_DEFAULT bor ?ESM_CLASS_TYPE_DEFAULT bor ?ESM_CLASS_GSM_UDHI
% ```
% Messaging Mode (bits 1-0)
-define(ESM_CLASS_MODE_DEFAULT,        2#00000000).
-define(ESM_CLASS_MODE_DATAGRAM,       2#00000001).
-define(ESM_CLASS_MODE_FORWARD,        2#00000010).
-define(ESM_CLASS_MODE_STORE_FORWARD,  2#00000011).

% Message Type (bits 5-2)
-define(ESM_CLASS_TYPE_DEFAULT,                            2#00000000).
-define(ESM_CLASS_TYPE_MC_DELIVERY_RECEIPT,                2#00000100).
-define(ESM_CLASS_TYPE_INTERMEDIATE_DELIVERY_NOTIFICATION, 2#00100000).

% ASNI-41 Specific (bits 5-2)
-define(ESM_CLASS_ANSI41_DELIVERY_ACK,       2#00001000).
-define(ESM_CLASS_ANSI41_MANUAL_ACK,         2#00010000).
-define(ESM_CLASS_ANSI41_CONVERSATION_ABORT, 2#00011000).

% GSM Specific (bits 7-6)
-define(ESM_CLASS_GSM_NO_FEATURES,     2#00000000).
-define(ESM_CLASS_GSM_UDHI,            2#01000000).
-define(ESM_CLASS_GSM_REPLY_PATH,      2#10000000).
-define(ESM_CLASS_GSM_UDHI_REPLY_PATH, 2#11000000).

% priority_flag Values
-define(PRIORITY_FLAG_GSM_SMS_NON_PRIORITY,        0).
-define(PRIORITY_FLAG_GSM_SMS_PRIORITY,            1).  % 2-3 have same meaning
-define(PRIORITY_FLAG_GSM_CBS_NORMAL,              0).
-define(PRIORITY_FLAG_GSM_CBS_IMMEDIATE_BROADCAST, 1).
-define(PRIORITY_FLAG_GSM_CBS_HIGH_PRIORITY,       2).
-define(PRIORITY_FLAG_GSM_CBS_RESERVED,            3).
-define(PRIORITY_FLAG_GSM_CBS_PRIORITY_BACKGROUND, 4).
-define(PRIORITY_FLAG_TDMA_BULK,                   0).
-define(PRIORITY_FLAG_TDMA_NORMAL,                 1).
-define(PRIORITY_FLAG_TDMA_URGENT,                 2).
-define(PRIORITY_FLAG_TDMA_VERY_URGENT,            3).
-define(PRIORITY_FLAG_CDMA_NORMAL,                 0).
-define(PRIORITY_FLAG_CDMA_INTERACTIVE,            1).
-define(PRIORITY_FLAG_CDMA_URGENT,                 2).
-define(PRIORITY_FLAG_CDMA_EMERGENCY,              3).
-define(PRIORITY_FLAG_ANSI_41_CBS_NORMAL,          0).
-define(PRIORITY_FLAG_ANSI_41_CBS_INTERACTIVE,     1).
-define(PRIORITY_FLAG_ANSI_41_CBS_URGENT,          2).
-define(PRIORITY_FLAG_ANSI_41_CBS_EMERGENCY,       3).

% registered_delivery Value
-define(REGISTERED_DELIVERY_DEFAULT, ?NULL_INTEGER).  % No receipts requested

% registered_delivery Bits Values
%
% To compound the complete esm_class value from the Bits, pick a Bit Value
% from every type and do the bitwise "or" of the parts.
%
% ```
% ?REGISTERED_DELIVERY_MC_FAILURE bor ?REGISTERED_DELIVERY_SME_BOTH
% ```
% MC Delivery Receipt (bits 1-0)
-define(REGISTERED_DELIVERY_MC_NEVER,   2#00000000). % Never request a receipt
-define(REGISTERED_DELIVERY_MC_ALWAYS,  2#00000001). % Always, whatever outcome
-define(REGISTERED_DELIVERY_MC_FAILURE, 2#00000010). % Failure delivery outcome
-define(REGISTERED_DELIVERY_MC_SUCCESS, 2#00000011). % Success delivery outcome

% SME Originated Acknowledgement (bits 3-2)
-define(REGISTERED_DELIVERY_SME_NEVER,    2#00000000). % No Ack requested
-define(REGISTERED_DELIVERY_SME_DELIVERY, 2#00000100). % Delivery Ack requested
-define(REGISTERED_DELIVERY_SME_MANUAL,   2#00001000). % Manual/User Ack
-define(REGISTERED_DELIVERY_SME_BOTH,     2#00001100). % Delivery & Manual/User

% Intermediate Notification (bin 4)
-define(REGISTERED_DELIVERY_INTERMEDIATE_NO,  2#00000000). % Not requested
-define(REGISTERED_DELIVERY_INTERMEDIATE_YES, 2#00010000). % Notific. Requested

% replace_if_present_flag Values
-define(REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE, 0).
-define(REPLACE_IF_PRESENT_FLAG_REPLACE,        1).

% schedule_delivery_time Values
-define(SCHEDULE_DELIVERY_TIME_IMMEDIATE, ?NULL_C_OCTET_STRING).  % Immediate

% validity_period Values
-define(VALIDITY_PERIOD_DEFAULT, ?NULL_C_OCTET_STRING).  % Use MC defaults

% final_date Values
-define(FINAL_DATE_FINAL_STATE_NOT_REACHED, ?NULL_C_OCTET_STRING).

% service_type Values
%
% The following generic service_types are defined, all other values are
% carrier specific and defined by mutual agreement between the MC Service
% provider and the ESME application.
-define(SERVICE_TYPE_NULL, ?NULL_C_OCTET_STRING).  % Default
-define(SERVICE_TYPE_CMT,  "CMT").   % Cellular Messaging
-define(SERVICE_TYPE_CPT,  "CPT").   % Cellular Paging
-define(SERVICE_TYPE_VMN,  "VMN").   % Voice Mail Notification
-define(SERVICE_TYPE_VMA,  "VMA").   % Voice Mail Alerting
-define(SERVICE_TYPE_WAP,  "WAP").   % Wireless Application Protocol
-define(SERVICE_TYPE_USSD, "USSD").  % Unstructured Supplementary
                                     % Services Data
-define(SERVICE_TYPE_CBS,  "CBS").   % Cell Broadcast Service
-define(SERVICE_TYPE_GUTS, "GUTS").  % Generic UDP Transport Service

% sm_length Values
-define(SM_LENGTH_NO_DATA, ?NULL_INTEGER).

% system_type Values
-define(SYSTEM_TYPE_UNKNOWN, ?NULL_C_OCTET_STRING).
-define(SYSTEM_TYPE_VMS,     "VMS").  % Voice Mail System
-define(SYSTEM_TYPE_OTA,     "OTA").  % Over-The-Air Activation System

%%% TLV FIELD VALUES
% alert_on_message_delivery Values
-define(ALERT_ON_MESSAGE_DELIVERY_DEFAULT, 0).  % Use mobile default alert
-define(ALERT_ON_MESSAGE_DELIVERY_LOW,     1).  % Use low-priority alert
-define(ALERT_ON_MESSAGE_DELIVERY_MEDIUM,  2).  % Use medium-priority alert
-define(ALERT_ON_MESSAGE_DELIVERY_HIGH,    3).  % Use high-priority alert

% broadcast_area_success Values
-define(BROADCAST_AREA_SUCCESS_INFORMATION_NOT_AVAILABLE, 255).

% broadcast_channel_indicator Values
-define(BROADCAST_CHANNEL_INDICATOR_BASIC,    0). % Basic Channel (Default)
-define(BROADCAST_CHANNEL_INDICATOR_EXTENDED, 1). % Extended Channel

% broadcast_content_type Values
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GENERIC, 0).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GSM,     1).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_TDMA,    2).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_CDMA,    3).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INDEX,                        16#0000).
-define(BROADCAST_CONTENT_TYPE_SERVICE_EMERGENCY_BROADCASTS,         16#0001).
-define(BROADCAST_CONTENT_TYPE_SERVICE_IRDB_DOWNLOAD,                16#0002).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NEWS_FLASHES,                 16#0010).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_GENERAL_NEWS,           16#0011).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_GENERAL_NEWS,        16#0012).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_GENERAL_NEWS,        16#0013).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_GENERAL_NEWS,   16#0014).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_FINANCIAL_NEWS,         16#0015).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_FINANCIAL_NEWS,      16#0016).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_FINANCIAL_NEWS,      16#0017).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_FINANCIAL_NEWS, 16#0018).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_SPORTS_NEWS,            16#0019).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_SPORTS_NEWS,         16#001A).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_SPORTS_NEWS,         16#001B).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_SPORTS_NEWS,    16#001C).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_ENTERTAINMENT_NEWS,     16#001D).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_ENTERTAINMENT_NEWS,  16#001E).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_ENTERTAINMENT_NEWS,  16#001F).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_ENTERTAINMENT_NEWS,
        16#0020).
-define(BROADCAST_CONTENT_TYPE_SERVICE_MEDICAL,                      16#0021).
-define(BROADCAST_CONTENT_TYPE_SERVICE_DOCTORS,                      16#0022).
-define(BROADCAST_CONTENT_TYPE_SERVICE_PHARMACY,                     16#0023).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_TRAFFIC,                16#0030).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LONG_DISTANCE_TRAFFIC,        16#0031).
-define(BROADCAST_CONTENT_TYPE_SERVICE_TAXIS,                        16#0032).
-define(BROADCAST_CONTENT_TYPE_SERVICE_WEATHER,                      16#0033).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_AIRPORT_FLIGHT_SCHEDULES,
        16#0034).
-define(BROADCAST_CONTENT_TYPE_SERVICE_RESTAURANTS,                  16#0035).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LODGINGS,                     16#0036).
-define(BROADCAST_CONTENT_TYPE_SERVICE_RETAIL_DIRECTORY,             16#0037).
-define(BROADCAST_CONTENT_TYPE_SERVICE_ADVERTISEMENTS,               16#0038).
-define(BROADCAST_CONTENT_TYPE_SERVICE_STOCK_QUOTES,                 16#0039).
-define(BROADCAST_CONTENT_TYPE_SERVICE_EMPLOYMENT_OPPORTUNITIES,     16#0040).
-define(BROADCAST_CONTENT_TYPE_SERVICE_TECHNOLOGY_NEWS,              16#0041).
-define(BROADCAST_CONTENT_TYPE_SERVICE_BASE_STATION_INFORMATION,     16#0070).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NETWORK_INFORMATION,          16#0071).
-define(BROADCAST_CONTENT_TYPE_SERVICE_OPERATOR_SERVICES,            16#0080).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_DIRECTORY_ENQUIRIES, 16#0081).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_DIRECTORY_ENQUIRIES,
        16#0082).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_CUSTOMER_CARE,       16#0083).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_CUSTOMER_CARE,  16#0084).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_TIME_ZONE,              16#0085).
-define(BROADCAST_CONTENT_TYPE_SERVICE_MULTI_CATEGORY_SERVICES,      16#0100).

% broadcast_frequency_interval Values
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_AS_FREQUENTLY_AS_POSSIBLE, 0).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_SECONDS, 16#08).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MINUTES, 16#09).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_HOURS,   16#0A).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DAYS,    16#0B).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_WEEKS,   16#0C).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MONTHS,  16#0D).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_YEARS,   16#0E).

% broadcast_message_class Values
-define(BROADCAST_MESSAGE_CLASS_NO_CLASS, 16#00).% No Class Specified (Default)
-define(BROADCAST_MESSAGE_CLASS_CLASS_1,  16#01).% Class 1 (User Defined)
-define(BROADCAST_MESSAGE_CLASS_CLASS_2,  16#02).% Class 2 (User Defined)
-define(BROADCAST_MESSAGE_CLASS_CLASS_3,  16#03).% Class 3 (Terminal Equipment)

% broadcast_rep_num Values
-define(BROADCAST_REP_NUM_DEFAULT, 1).  % Implementation specific

% callback_num Values
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_TBCD,  0).
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_ASCII, 1).

% callback_num_pres_ind Values
-define(CALLBACK_NUM_PRES_IND_ALLOWED_NOT_SCREENED,       2#00000000).
-define(CALLBACK_NUM_PRES_IND_ALLOWED_PASSED,             2#00000001).
-define(CALLBACK_NUM_PRES_IND_ALLOWED_FAILED,             2#00000010).
-define(CALLBACK_NUM_PRES_IND_ALLOWED_NETWORK,            2#00000011).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_NOT_SCREENED,    2#00000100).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_PASSED,          2#00000101).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_FAILED,          2#00000110).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_NETWORK,         2#00000111).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_NOT_SCREENED, 2#00001000).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_PASSED,       2#00001001).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_FAILED,       2#00001010).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_NETWORK,      2#00001011).

% congestion_state Values
-define(CONGESTION_STATE_IDLE,                0).  %
-define(CONGESTION_STATE_LOW_LOAD,           15).  %  1-29
-define(CONGESTION_STATE_MEDIUM_LOAD,        40).  % 30-49
-define(CONGESTION_STATE_HIGH_LOAD,          65).  % 50-79
-define(CONGESTION_STATE_OPTIMUM_LOAD,       85).  % 80-89
-define(CONGESTION_STATE_NEARING_CONGESTION, 95).  % 90-99

% delivery_failure_reason Values
-define(DELIVERY_FAILURE_REASON_UNAVAILABLE,     0).  % Destination unavailable
-define(DELIVERY_FAILURE_REASON_INVALID,         1).  % Destination Address
                                                      % invalid (e.g. suspended
                                                      % no SMS capability, etc)
-define(DELIVERY_FAILURE_REASON_PERMANENT_ERROR, 2).  % Permanent network error
-define(DELIVERY_FAILURE_REASON_TEMPORARY_ERROR, 3).  % Temporary network error

% dest_addr_np_resolution Values
-define(DEST_ADDR_NP_RESOLUTION_NO_QUERY_PERFORMED, 0). % Default
-define(DEST_ADDR_NP_RESOLUTION_NUMBER_NOT_PORTED,  1). % Query performed
-define(DEST_ADDR_NP_RESOLUTION_NUMBER_PORTED,      2). % Query performed

% display_time Values
-define(DISPLAY_TIME_TEMPORARY, 0).
-define(DISPLAY_TIME_DEFAULT,   1).  % Default
-define(DISPLAY_TIME_INVOKE,    3).

% dpf_result Values
-define(DPF_RESULT_NOT_SET, 0).  % DPF not set
-define(DPF_RESULT_SET,     1).  % DPF set

% its_reply_type Values
-define(ITS_REPLY_TYPE_DIGIT,          0).
-define(ITS_REPLY_TYPE_NUMBER,         1).
-define(ITS_REPLY_TYPE_TELEPHONE_NO,   2).
-define(ITS_REPLY_TYPE_PASSWORD,       3).
-define(ITS_REPLY_TYPE_CHARACTER_LINE, 4).
-define(ITS_REPLY_TYPE_MENU,           5).
-define(ITS_REPLY_TYPE_DATE,           6).
-define(ITS_REPLY_TYPE_TIME,           7).
-define(ITS_REPLY_TYPE_CONTINUE,       8).

% language_indicator Values
-define(LANGUAGE_INDICATOR_UNSPECIFIED, 0).  % Default
-define(LANGUAGE_INDICATOR_ENGLISH,     1).
-define(LANGUAGE_INDICATOR_FRENCH,      2).
-define(LANGUAGE_INDICATOR_SPANISH,     3).
-define(LANGUAGE_INDICATOR_GERMAN,      4).
-define(LANGUAGE_INDICATOR_PORTUGUESE,  5).

% more_messages_to_send Values
-define(MORE_MESSAGES_TO_SEND_NO,  0).  % No more messages to follow
-define(MORE_MESSAGES_TO_SEND_YES, 1).  % More messages to follow (default)

% ms_availability_status Values
-define(MS_AVAILABILITY_STATUS_AVAILABLE,   0).  % MS Available (Default)
-define(MS_AVAILABILITY_STATUS_DENIED,      1).  % Suspended, no SMS capability
-define(MS_AVAILABILITY_STATUS_UNAVAILABLE, 2).  % Unavailable

% ms_msg_wait_facilities Values
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_VOICEMAIL, 2#00000000).
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_FAX,       2#00000001).
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_EMAIL,     2#00000010).
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_OTHER,     2#00000011).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_VOICEMAIL,   2#10000000).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_FAX,         2#10000001).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_EMAIL,       2#10000010).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_OTHER,       2#10000011).

% ms_validity_time_behaviour Values
-define(MS_VALIDITY_STORE_INDEFINITELY,              0). % Default
-define(MS_VALIDITY_UNTIL_POWER_DOWN,                1).
-define(MS_VALIDITY_UNTIL_REGISTRATION_AREA_CHANGES, 2).
-define(MS_VALIDITY_DISPLAY_ONLY,                    3).
-define(MS_VALIDITY_RELATIVE_TIME_PERIOD,            4).

% ms_validity_time_unit Values
-define(MS_VALIDITY_DEFAULT,           #ms_validity_absolute{}).
-define(MS_VALIDITY_TIME_UNIT_SECONDS, 2#00000000).
-define(MS_VALIDITY_TIME_UNIT_MINUTES, 2#00000001).
-define(MS_VALIDITY_TIME_UNIT_HOURS,   2#00000010).
-define(MS_VALIDITY_TIME_UNIT_DAYS,    2#00000011).
-define(MS_VALIDITY_TIME_UNIT_WEEKS,   2#00000100).
-define(MS_VALIDITY_TIME_UNIT_MONTHS,  2#00000101).
-define(MS_VALIDITY_TIME_UNIT_YEARS,   2#00000110).

% network_error_code_type Values
-define(NETWORK_ERROR_CODE_TYPE_ANSI_136_ACCESS_DENIED_REASON, 1).
-define(NETWORK_ERROR_CODE_TYPE_IS_95_ACCESS_DENIED_REASON,    2).
-define(NETWORK_ERROR_CODE_TYPE_GSM,                           3).
-define(NETWORK_ERROR_CODE_TYPE_ANSI_136_CAUSE_CODE,           4).
-define(NETWORK_ERROR_CODE_TYPE_IS_95_COUSE_CODE,              5).
-define(NETWORK_ERROR_CODE_TYPE_ANSI_41_ERROR,                 6).
-define(NETWORK_ERROR_CODE_TYPE_SMPP_ERROR,                    7).
-define(NETWORK_ERROR_CODE_TYPE_MESSAGE_CENTER_SPECIFIC,       8).

% payload_type Values
-define(PAYLOAD_TYPE_DEFAULT, 0).  % Default
-define(PAYLOAD_TYPE_WDP,     0).  % WAP
-define(PAYLOAD_TYPE_WCMP,    1).  % Wireless Control Message Protocol.

% privacy_indicator Values
-define(PRIVACY_INDICATOR_NOT_RESTRICTED, 0).  % Privacy Level 0 (Default)
-define(PRIVACY_INDICATOR_RESTRICTED,     1).  % Privacy Level 1
-define(PRIVACY_INDICATOR_CONFIDENTIAL,   2).  % Privacy Level 2
-define(PRIVACY_INDICATOR_SECRET,         3).  % Privacy Level 3

% sar_segment_seqnum Values
-define(SAR_SEGMENT_SEQNUM_FIRST, 1).

% sar_total_segments Values
-define(SAR_TOTAL_SEGMENTS_SINGLE, 1).

% set_dpf Values
-define(SET_DPF_NOT_REQUESTED, 0). % DPF for delivery to MS not requested
-define(SET_DPF_REQUESTED,     1). % Delivery Pending Flag requested (default)

% ussd_service_op Values
-define(USSD_SERVICE_OP_PSSD_INDICATION, 0).
-define(USSD_SERVICE_OP_PSSR_INDICATION, 1).
-define(USSD_SERVICE_OP_USSR_REQUEST,    2).
-define(USSD_SERVICE_OP_USSN_REQUEST,    3).
-define(USSD_SERVICE_OP_PSSD_RESPONSE,  16).
-define(USSD_SERVICE_OP_PSSR_RESPONSE,  17).
-define(USSD_SERVICE_OP_USSR_CONFIRM,   18).
-define(USSD_SERVICE_OP_USSN_CONFIRM,   19).

%%% USEFUL MACROS
%% Work with command ids and names
%%
%% %@see section 4.7.5 on [SMPP 5.0]
% true if valid, false otherwise
-define(VALID_COMMAND_ID(CmdId),
        (((CmdId >= ?COMMAND_ID_BIND_RECEIVER) and
          (CmdId =< ?COMMAND_ID_BIND_TRANSCEIVER)) or
         (CmdId == ?COMMAND_ID_OUTBIND) or
         (CmdId == ?COMMAND_ID_ENQUIRE_LINK) or
         (CmdId == ?COMMAND_ID_SUBMIT_MULTI) or
         (CmdId == ?COMMAND_ID_ALERT_NOTIFICATION) or
         (CmdId == ?COMMAND_ID_DATA_SM) or
         ((CmdId >= ?COMMAND_ID_BROADCAST_SM) and
          (CmdId =< ?COMMAND_ID_CANCEL_BROADCAST_SM)) or
         ((CmdId >= ?COMMAND_ID_GENERIC_NACK) and
          (CmdId =< ?COMMAND_ID_BIND_TRANSCEIVER_RESP)) or
         (CmdId == ?COMMAND_ID_ENQUIRE_LINK_RESP) or
         (CmdId == ?COMMAND_ID_SUBMIT_MULTI_RESP) or
         (CmdId == ?COMMAND_ID_DATA_SM_RESP) or
         ((CmdId >= ?COMMAND_ID_BROADCAST_SM_RESP) and
          (CmdId =< ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP)))).

% Gets the counterpart response command_id
-define(RESPONSE(CmdId),
		if
			CmdId == ?COMMAND_ID_OUTBIND ->
				?COMMAND_ID_GENERIC_NACK;
			CmdId == ?COMMAND_ID_ALERT_NOTIFICATION ->
				?COMMAND_ID_GENERIC_NACK;
			true ->
				CmdId + 16#80000000
		end).

% Gets the counterpart request command_id
-define(REQUEST(CmdId), (CmdId - 16#80000000)).

% Checks if it is a request or response
-define(IS_REQUEST(CmdId), CmdId < 16#80000000).
-define(IS_RESPONSE(CmdId), CmdId >= 16#80000000).

% Gets the command_id for a given command_name.
-define(COMMAND_ID(CmdName),
        if
            CmdName == bind_transmitter ->
                ?COMMAND_ID_BIND_TRANSMITTER;
            CmdName == bind_transmitter_resp ->
                ?COMMAND_ID_BIND_TRANSMITTER_RESP;
            CmdName == bind_receiver ->
                ?COMMAND_ID_BIND_RECEIVER;
            CmdName == bind_receiver_resp ->
                ?COMMAND_ID_BIND_RECEIVER_RESP;
            CmdName == bind_transceiver ->
                ?COMMAND_ID_BIND_TRANSCEIVER;
            CmdName == bind_transceiver_resp ->
                ?COMMAND_ID_BIND_TRANSCEIVER_RESP;
            CmdName == outbind ->
                ?COMMAND_ID_OUTBIND;
            CmdName == unbind ->
                ?COMMAND_ID_UNBIND;
            CmdName == unbind_resp ->
                ?COMMAND_ID_UNBIND_RESP;
            CmdName == enquire_link ->
                ?COMMAND_ID_ENQUIRE_LINK;
            CmdName == enquire_link_resp ->
                ?COMMAND_ID_ENQUIRE_LINK_RESP;
            CmdName == alert_notification ->
                ?COMMAND_ID_ALERT_NOTIFICATION;
            CmdName == generic_nack ->
                ?COMMAND_ID_GENERIC_NACK;
            CmdName == submit_sm ->
                ?COMMAND_ID_SUBMIT_SM;
            CmdName == submit_sm_resp ->
                ?COMMAND_ID_SUBMIT_SM_RESP;
            CmdName == data_sm ->
                ?COMMAND_ID_DATA_SM;
            CmdName == data_sm_resp ->
                ?COMMAND_ID_DATA_SM_RESP;
            CmdName == submit_multi ->
                ?COMMAND_ID_SUBMIT_MULTI;
            CmdName == submit_multi_resp ->
                ?COMMAND_ID_SUBMIT_MULTI_RESP;
            CmdName == deliver_sm ->
                ?COMMAND_ID_DELIVER_SM;
            CmdName == deliver_sm_resp ->
                ?COMMAND_ID_DELIVER_SM_RESP;
            CmdName == broadcast_sm ->
                ?COMMAND_ID_BROADCAST_SM;
            CmdName == broadcast_sm_resp ->
                ?COMMAND_ID_BROADCAST_SM_RESP;
            CmdName == cancel_sm ->
                ?COMMAND_ID_CANCEL_SM;
            CmdName == cancel_sm_resp ->
                ?COMMAND_ID_CANCEL_SM_RESP;
            CmdName == query_sm ->
                ?COMMAND_ID_QUERY_SM;
            CmdName == query_sm_resp ->
                ?COMMAND_ID_QUERY_SM_RESP;
            CmdName == replace_sm ->
                ?COMMAND_ID_REPLACE_SM;
            CmdName == replace_sm_resp ->
                ?COMMAND_ID_REPLACE_SM_RESP;
            CmdName == query_broadcast_sm ->
                ?COMMAND_ID_QUERY_BROADCAST_SM;
            CmdName == query_broadcast_sm_resp ->
                ?COMMAND_ID_QUERY_BROADCAST_SM_RESP;
            CmdName == cancel_broadcast_sm ->
                ?COMMAND_ID_CANCEL_BROADCAST_SM;
            CmdName == cancel_broadcast_sm_resp ->
                ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP
        end).

%% Gets the command_name for a given command_id.
-define(COMMAND_NAME(CmdId),
        if
            CmdId == ?COMMAND_ID_BIND_TRANSMITTER ->
                bind_transmitter;
            CmdId == ?COMMAND_ID_BIND_TRANSMITTER_RESP ->
                bind_transmitter_resp;
            CmdId == ?COMMAND_ID_BIND_RECEIVER ->
                bind_receiver;
            CmdId == ?COMMAND_ID_BIND_RECEIVER_RESP ->
                bind_receiver_resp;
            CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
                bind_transceiver;
            CmdId == ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
                bind_transceiver_resp;
            CmdId == ?COMMAND_ID_OUTBIND ->
                outbind;
            CmdId == ?COMMAND_ID_UNBIND ->
                unbind;
            CmdId == ?COMMAND_ID_UNBIND_RESP ->
                unbind_resp;
            CmdId == ?COMMAND_ID_ENQUIRE_LINK ->
                enquire_link;
            CmdId == ?COMMAND_ID_ENQUIRE_LINK_RESP ->
                enquire_link_resp;
            CmdId == ?COMMAND_ID_ALERT_NOTIFICATION ->
                alert_notification;
            CmdId == ?COMMAND_ID_GENERIC_NACK ->
                generic_nack;
            CmdId == ?COMMAND_ID_SUBMIT_SM ->
                submit_sm;
            CmdId == ?COMMAND_ID_SUBMIT_SM_RESP ->
                submit_sm_resp;
            CmdId == ?COMMAND_ID_DATA_SM ->
                data_sm;
            CmdId == ?COMMAND_ID_DATA_SM_RESP ->
                data_sm_resp;
            CmdId == ?COMMAND_ID_SUBMIT_MULTI ->
                submit_multi;
            CmdId == ?COMMAND_ID_SUBMIT_MULTI_RESP ->
                submit_multi_resp;
            CmdId == ?COMMAND_ID_DELIVER_SM ->
                deliver_sm;
            CmdId == ?COMMAND_ID_DELIVER_SM_RESP ->
                deliver_sm_resp;
            CmdId == ?COMMAND_ID_BROADCAST_SM ->
                broadcast_sm;
            CmdId == ?COMMAND_ID_BROADCAST_SM_RESP ->
                broadcast_sm_resp;
            CmdId == ?COMMAND_ID_CANCEL_SM ->
                cancel_sm;
            CmdId == ?COMMAND_ID_CANCEL_SM_RESP ->
                cancel_sm_resp;
            CmdId == ?COMMAND_ID_QUERY_SM ->
                query_sm;
            CmdId == ?COMMAND_ID_QUERY_SM_RESP ->
                query_sm_resp;
            CmdId == ?COMMAND_ID_REPLACE_SM ->
                replace_sm;
            CmdId == ?COMMAND_ID_REPLACE_SM_RESP ->
                replace_sm_resp;
            CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM ->
                query_broadcast_sm;
            CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM_RESP ->
                query_broadcast_sm_resp;
            CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM ->
                cancel_broadcast_sm;
            CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP ->
                cancel_broadcast_sm_resp
        end).

%% Null Settings
%%
%% %@see section 3.1.1 on [SMPP 5.0]
-define(NULL_CHARACTER, 0).
-define(NULL_INTEGER,   0).
-define(NULL_C_OCTET_STRING, "").
-define(NULL_OCTET_STRING,   "").

%% SMPP versions
%%
%% %@see section 1.4.1 on [SMPP 5.0]
-define(SMPP_VERSION_EARLIER, 16#00).  % Earlier version
-define(SMPP_VERSION_3_3,     16#33).  % Version 3.3
-define(SMPP_VERSION_3_4,     16#34).  % Version 3.4
-define(SMPP_VERSION_5_0,     16#50).  % Version 5.0

%%% RECORDS
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% PDU Composite Field Record Definitions
%%%
%%% %@see sections 4.1 to 4.6 on [SMPP 5.0]
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% %@spec {dest_address_sme,
%%         DestFlag,
%%         DestAddrTon,
%%         DestAddrNpi,
%%         DestinationAddr}
%%    DestFlag        = int()
%%    DestAddrTon     = int()
%%    DestAddrNpi     = int()
%%    DestinationAddr = string()
%%
%% %@doc dest_address_sme composite record definition.
%%
%% <p>The macro ?DEST_ADDRESS_SME_DATATYPE defines the type specifier for this
%% field.</p>
%%
%% <dl>
%%   <dt>DestFlag: </dt><dd>Identifies the kind of address, 0x01 for SME
%%     address.  Integer, 1 octets (default is ?DEST_FLAG_SME).
%%   </dd>
%%   <dt>DestAddrTon: </dt><dd>Indicates Type of Number for destination.
%%     Integer, 1 octet (default is ?TON_INTERNATIONAL).
%%   </dd>
%%   <dt>DestAddrNpi: </dt><dd>Numbering Plan Indicator for destination.
%%     Integer, 1 octet  (default is ?NPI_ISDN).
%%   </dd>
%%   <dt>DestinationAddr: </dt><dd>Destination address of this short message.
%%     For mobile terminated messages, this is the directory number of the
%%     recipient MS.  C-Octet String, Var. max 21 octets.
%%   </dd>
%% </dl>
%% %@end
-record(dest_address_sme,
        {dest_flag       = ?DEST_FLAG_SME,
         dest_addr_ton   = ?TON_INTERNATIONAL,
         dest_addr_npi   = ?NPI_ISDN,
         destination_addr}).

%% %@spec {dest_address_dl, DestFlag, DlName}
%%    DestFlag  = int()
%%    DlName    = string()
%%
%% %@doc dest_address_dl composite record definition.
%%
%% <p>The macro ?DEST_ADDRESS_DL_DATATYPE defines the type specifier for this
%% field.</p>
%%
%% <dl>
%%   <dt>DestFlag: </dt><dd>Identifies the kind of address, 0x02 for
%%     Distribution List.  Integer, 1 octets (default is ?DEST_FLAG_DL)).
%%   </dd>
%%   <dt>DlName: </dt><dd>Name of the Distribution List.  C-Octet String,
%%     Var. max 21 octets.
%%   </dd>
%% </dl>
%% %@end
-record(dest_address_dl,
        {dest_flag = ?DEST_FLAG_DL,
         dl_name}).


%% %@spec {unsuccess_sme,
%%         DestAddrTon,
%%         DestAddrNpi,
%%         DestinationAddr,
%%         ErrorStatusCode}
%%    DestAddrTon     = int()
%%    DestAddrNpi     = int()
%%    DestinationAddr = string()
%%    ErrorStatusCode = int()
%%
%% %@doc unsuccess_sme composite record definition.
%%
%% <p>The macro ?UNSUCCESS_SME_DATATYPE defines the type specifier for this
%% field.</p>
%%
%% <dl>
%%   <dt>DestAddrTon: </dt><dd>Indicates Type of Number for destination.
%%     Integer, 1 octet (default is ?TON_INTERNATIONAL).
%%   </dd>
%%   <dt>DestAddrNpi: </dt><dd>Numbering Plan Indicator for destination.
%%     Integer, 1 octet  (default is ?NPI_ISDN).
%%   </dd>
%%   <dt>DestinationAddr: </dt><dd>Destination address of this short message.
%%     For mobile terminated messages, this is the directory number of the
%%     recipient MS.  C-Octet String, Var. max 21 octets.
%%   </dd>
%%   <dt>ErrorStatusCode: </dt><dd>Indicates the success or failure of the
%%     submit_multi request to this SME address.  Check command_status
%%     macros for a complete list of SMPP Error codes.  Integer, 4 octets.
%%   </dd>
%% </dl>
%% %@end
-record(unsuccess_sme,
        {dest_addr_ton     = ?TON_INTERNATIONAL,
         dest_addr_npi     = ?NPI_ISDN,
         destination_addr,
         error_status_code}).


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% PDU TLV Record Definitions
%%%
%%% %@see section 4.8.4 on [SMPP 5.0]
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% %@spec {broadcast_area, Format, Details}
%%    Format  = int()
%%    Details = string()
%%
%% %@doc broadcast_area TLV record definition.
%%
%% <p>The macro ?BROADCAST_AREA_DATATYPE defines the type specifier for this
%% TLV.</p>
%%
%% <dl>
%%   <dt>Format: </dt><dd>Used to specify the area format.  Integer, 1 octet
%%     (default is ?BROADCAST_AREA_FORMAT_ALIAS).
%%   </dd>
%%   <dt>Details: </dt><dd>Used to specify the broadcast area details.  Octet
%%     String, Var. max 100 octets.
%%   </dd>
%% </dl>
%% %@end
-record(broadcast_area,
        {format = ?BROADCAST_AREA_FORMAT_ALIAS,
         details}).

%% %@spec {broadcast_content_type, NetworkType, Service}
%%    NetworkType = int()
%%    Service     = int()
%%
%% %@doc broadcast_content_type TLV record definition.
%%
%% <p>The macro ?BROADCAST_CONTENT_TYPE_DATATYPE defines the type specifier for
%% this TLV.</p>
%%
%% <dl>
%%   <dt>NetworkType: </dt><dd>Tag indicating the network type.  Integer, 1
%%     octet (default is ?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GSM).
%%   </dd>
%%   <dt>Service: </dt><dd>Broadcast service type.  Integer, 2 octets (default
%%     is ?BROADCAST_CONTENT_TYPE_SERVICE_MULTI_CATEGORY_SERVICES).
%%   </dd>
%% </dl>
%% %@end
-record(broadcast_content_type,
        {network_type = ?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GSM,
         service = ?BROADCAST_CONTENT_TYPE_SERVICE_MULTI_CATEGORY_SERVICES}).

%% %@spec {broadcast_frequency_interval, TimeUnit, Number}
%%    TimeUnit = int()
%%    Number   = int()
%%
%% %@doc broadcast_frequency_interval TLV record definition.
%%
%% <p>The macro ?BROADCAST_FREQUENCY_INTERVALY_DATATYPE defines the type
%% specifier for this TLV.</p>
%%
%% <dl>
%%   <dt>TimeUnit: </dt><dd>Specifies the Units of Time.  Integer, 1
%%     octet (default is ?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MINUTES).
%%   </dd>
%%   <dt>Number: </dt><dd>Number of the specified time units.  Integer,
%%     2 octets.
%%   </dd>
%% </dl>
%% %@end
-record(broadcast_frequency_interval,
        {time_unit = ?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MINUTES,
         number}).

%% %@spec {subaddress, Tag, Data}
%%    Tag  = int()
%%    Data = string()
%%
%% %@doc dest_subaddress and source_subaddress TLV record definition.
%%
%% <p>The macro ?SUBADDRESS_DATATYPE defines the type specifier for these
%% TLVs.</p>
%%
%% <dl>
%%   <dt>Tag: </dt><dd>Indicates the type of sub-addressing information
%%     included in Data, and implies the type and length of sub-addressing
%%     information which can accompany this tag value in the Data field.
%%     Integer, 1 octet (default is ?SUBADDRESS_TAG_USER).
%%   </dd>
%%   <dt>Data: </dt><dd>Contain the subaddress.  Octet String, Var. max 22
%%     octets.
%%   </dd>
%% </dl>
%% %@end
-record(subaddress,
        {tag = ?SUBADDRESS_TAG_USER,
         data}).

%% %@spec {callback_num,
%%         DigitModeIndicator,
%%         AddrTon,
%%         AddrNpi,
%%         NumberDigits}
%%    DigitModeIndicator = int()
%%    AddrTon            = int()
%%    AddrNpi            = int()
%%    NumberDigits       = string()
%%
%% %@doc callback_num TLV record definition.
%%
%% <p>The macro ?CALLBACK_NUM_DATATYPE defines the type specifier for this
%% TLV.</p>
%%
%% <dl>
%%   <dt>DigitModeIndicator: </dt><dd>Indicates that the Call Back Number is
%%     sent to the mobile as DTMF digits encoded in TBCC (Value = 0) or as
%%     ASCII digits (Value 1).  Integer, 1 octet (default is
%%     ?CALLBACK_NUM_DIGIT_MODE_INDICATOR_ASCII).
%%   </dd>
%%   <dt>AddrTon: </dt><dd>Indicates Type of Number for destination.
%%     Integer, 1 octet (default is ?TON_INTERNATIONAL).
%%   </dd>
%%   <dt>AddrNpi: </dt><dd>Numbering Plan Indicator for destination.
%%     Integer, 1 octet  (default is ?NPI_ISDN).
%%   </dd>
%%   <dt>NumberDigits: </dt><dd>The Call Back Number Digits.  Octet String,
%%     Var. max 16 octets.
%%   </dd>
%% </dl>
%% %@end
-record(callback_num,
        {digit_mode_indicator = ?CALLBACK_NUM_DIGIT_MODE_INDICATOR_ASCII,
         addr_ton             = ?TON_INTERNATIONAL,
         addr_npi             = ?NPI_ISDN,
         number_digits}).

%% %@spec {callback_num_atag, DataCoding, DisplayCharacters}
%%    DataCoding        = int()
%%    DisplayCharacters = string()
%%
%% %@doc callback_num_atag TLV record definition.
%%
%% <p>The macro ?CALLBACK_NUM_ATAG_DATATYPE defines the type specifier for this
%% TLV.</p>
%%
%% <dl>
%%   <dt>DataCoding: </dt><dd>Defines the encoding scheme of the Alpha Tag
%%     display characters.  Integer, 1 octet (default is
%%     ?ENCODING_SCHEME_LATIN_1).
%%   </dd>
%%   <dt>DisplayCharacters: </dt><dd>The Alpha Tag display Characters.  Octet
%%     String, Var. max 64 octets.
%%   </dd>
%% </dl>
%% %@end
-record(callback_num_atag,
        {data_coding = ?ENCODING_SCHEME_LATIN_1,
         display_characters}).


%% %@spec {telematics_id, ProtocolId, Reserved}
%%    ProtocolId = int()
%%    Reserver   = int()
%%
%% %@doc dest_telematics_id and source_telematics_id TLV record definition.
%%
%% <p>The macro ?TELEMATICS_ID_DATATYPE defines the type specifier for these
%% TLV.</p>
%%
%% <dl>
%%   <dt>ProtocolId: </dt><dd>Protocol Identifier.  Network specific field.
%%     Integer, 1 octet (default is ?PROTOCOL_ID_GSM).
%%   </dd>
%%   <dt>Reserved: </dt><dd>Reserved. Integer, 1 octet (default is
%%     ?NULL_INTEGER).
%%   </dd>
%% </dl>
%% %@end
-record(telematics_id,
        {protocol_id = ?PROTOCOL_ID_GSM,
         reserved    = ?NULL_INTEGER}).

%% %@spec {its_session_info, SessionNumber, SequenceNumber}
%%    SessionNumber  = int()
%%    SequenceNumber = int()
%%
%% %@doc its_session_info TLV record definition.
%%
%% <p>The macro ?ITS_SESSION_INFO_DATATYPE defines the type specifier for this
%% TLV.</p>
%%
%% <dl>
%%   <dt>SessionNumber: </dt><dd>Remains constant for each session.
%%     Integer, 1 octet (default is 0).
%%   </dd>
%%   <dt>SequenceNumber: </dt><dd>Sequence number of the dialogue unit (as
%%     assigned bye the ESME) within the session is encoded in bits 7..1. The
%%     End of Session Indicator indicates the message is the end of the
%%     conversation session and is encoded in bit 0 as follows:
%%
%%     <ul>
%%       <li>0 = End of Session Indicator Inactive.</li>
%%       <li>1 = End of Session Indicator Active.</li>
%%     </ul>
%%
%%     <p>While the end of session is inactive the SequenceNumber is an even
%%     number and is increased by 2.  The Session Indicator becomes active it
%%     should be incremented by 1, (an odd number).  Integer, 1 octet (default
%%     is 0).</p>
%%   </dd>
%% </dl>
%% %@end
-record(its_session_info,
        {session_number  = 0,
         sequence_number = 0}).

%% %@spec {ms_validity_absolute, Behaviour}
%%    Behaviour = int()
%%
%% %@doc ms_validity_absolute TLV record definition.
%%
%% <p>The macro ?MS_VALIDITY_ABSOLUTE_DATATYPE defines the type specifier for
%% this TLV.</p>
%%
%% <dl>
%%   <dt>Behaviour: </dt><dd>Validity behaviour.  Integer, 1 octet
%%     (default is ?MS_VALIDITY_STORE_INDEFINITELY).
%%   </dd>
%% </dl>
%% %@end
-record(ms_validity_absolute, {behaviour = ?MS_VALIDITY_STORE_INDEFINITELY}).

%% %@spec {ms_validity_relative, Behaviour, TimeUnit, Number}
%%    Behaviour = int()
%%    TimeUnit  = int()
%%    Number    = int()
%%
%% %@doc ms_validity_relative TLV record definition.
%%
%% <p>The macro ?MS_VALIDITY_RELATIVE_DATATYPE defines the type specifier for
%% this TLV.</p>
%%
%% <dl>
%%   <dt>Behaviour: </dt><dd>Validity behaviour.  Integer, 1 octet
%%     (default is ?MS_VALIDITY_RELATIVE_TIME_PERIOD).
%%   </dd>
%%   <dt>TimeUnit: </dt><dd>Specifies the Units of Time.  Integer, 1 octet
%%     (default is ?TIME_UNIT_SECONDS).
%%   </dd>
%%   <dt>Number: </dt><dd>The number of the specified time units.  Integer, 2
%%     octet.
%%   </dd>
%% </dl>
%% %@end
-record(ms_validity_relative,
        {behaviour = ?MS_VALIDITY_RELATIVE_TIME_PERIOD,
         time_unit = ?MS_VALIDITY_TIME_UNIT_SECONDS,
         number}).

%% %@spec {network_error_code, Type, Error}
%%    Type  = int()
%%    Error = int()
%%
%% %@doc network_error_code TLV record definition.
%%
%% <p>The macro ?NETWORK_ERROR_CODE_DATATYPE defines the type specifier for
%% this TLV.</p>
%%
%% <dl>
%%   <dt>Type: </dt><dd>Network type.  Integer, 1 octet (default is
%%     ?NETWORK_ERROR_CODE_TYPE_GSM).
%%   </dd>
%%   <dt>Error: </dt><dd>Specify the actual network error code approproate to
%%     the network type.  Integer, 2 octet.
%%   </dd>
%% </dl>
%% %@end
-record(network_error_code,
        {type  = ?NETWORK_ERROR_CODE_TYPE_GSM,
         error}).

-endif.  % -ifndef(smpp_globals)

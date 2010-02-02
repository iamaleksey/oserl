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
-ifndef(smpp_pdu).
-define(smpp_pdu, true).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").  % Some global definitions
-include("smpp_param.hrl").         % The parameters declaration
-include("smpp_pdu_syntax.hrl").    % The syntax used in this file

%%% MACROS
-define(BIND_TRANSMITTER,
        ?PDU([?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [],
             [])).

-define(BIND_TRANSMITTER_RESP,
        ?PDU([?SYSTEM_ID],
             [],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

-define(BIND_RECEIVER,
        ?PDU([?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [],
             [])).

-define(BIND_RECEIVER_RESP,
        ?PDU([?SYSTEM_ID],
             [],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

-define(BIND_TRANSCEIVER,
        ?PDU([?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [],
             [])).

-define(BIND_TRANSCEIVER_RESP,
        ?PDU([?SYSTEM_ID],
             [],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

-define(OUTBIND,
        ?PDU([?SYSTEM_ID,
              ?PASSWORD],
             [],
             [])).

-define(UNBIND,
        ?PDU([],
             [],
             [])).

-define(UNBIND_RESP,
        ?PDU([],
             [],
             [?CONGESTION_STATE])).

-define(ENQUIRE_LINK,
        ?PDU([],
             [],
             [])).

-define(ENQUIRE_LINK_RESP,
        ?PDU([],
             [],
             [?CONGESTION_STATE])).

-define(ALERT_NOTIFICATION,
        ?PDU([?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_65,
              ?ESME_ADDR_TON,
              ?ESME_ADDR_NPI,
              ?ESME_ADDR],
             [],
             [?MS_AVAILABILITY_STATUS])).

-define(GENERIC_NACK,
        ?PDU([],
             [],
             [])).

-define(SUBMIT_SM,
        ?PDU([?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21,
              ?ESM_CLASS,
              ?PROTOCOL_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [],
             [?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              ?DISPLAY_TIME,              % (CDMA, TDMA)
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MESSAGE_PAYLOAD,
              ?MORE_MESSAGES_TO_SEND,
              ?MS_MSG_WAIT_FACILITIES,
              ?MS_VALIDITY,               % (CDMA, TDMA)
              ?NUMBER_OF_MESSAGES,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,
              ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(SUBMIT_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,   % data_sm_resp only
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

-define(DATA_SM,
        ?PDU([?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_65,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_65,
              ?ESM_CLASS,
              ?REGISTERED_DELIVERY,
              ?DATA_CODING],
             [?MESSAGE_PAYLOAD],
             [?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              ?DISPLAY_TIME,              % (CDMA, TDMA)
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MORE_MESSAGES_TO_SEND,
              ?MS_MSG_WAIT_FACILITIES,
              ?MS_VALIDITY,               % (CDMA, TDMA)
              ?NUMBER_OF_MESSAGES,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,
              ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(DATA_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

-define(SUBMIT_MULTI,
        ?PDU([?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDRESS,
              ?ESM_CLASS,
              ?PROTOCOL_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [],
             [?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              ?DISPLAY_TIME,              % (CDMA, TDMA)
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MESSAGE_PAYLOAD,
              ?MORE_MESSAGES_TO_SEND,     % makes sense on submit multi?
              ?MS_MSG_WAIT_FACILITIES,
              ?MS_VALIDITY,               % (CDMA, TDMA)
              ?NUMBER_OF_MESSAGES,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,                   % makes sense on submit multi?
              ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

%% Unlike some simulators, like SMPPSim 1.1, which they ignore the field
%% ``no_unsuccess`` if no destination was unsuccessful, this
%% implementation sets to O this field if every destination was reached
%% (remember that UNSUCCESS_SME was declared as a multivalue and this base type
%% automatically encodes the number of elements at the head of the binary).
%%
%% Notice that the SMS forum recommends the approach followed here.  Excerpt
%% from the SMS Forum:
%%
%% ///
%% When all dests are sucessful, the no_unsuccess field will be set to 0.
%% There are no unsuccessful SME fields then included.
%%
%% ...here are two examples of the no_unsuccess file encodings for 0 and
%% then 2 failed destinations
%% ///
%% ```
%% Body:
%% 41424300        Message id "ABC"
%% 00              0 unsuccessful SMEs
%% ```
%%
%% ```
%% 41424300        Message id "ABC"
%% 02              2 unsuccessful SMEs
%% 01              TON=1
%% 01              NPI=1
%% 353535353500    SME "55555"
%% 00000005        SMPP Error 0x00000005
%% 02              TON=2
%% 08              NPI=3
%% 363534353500    SME "65455"
%% 00000015        SMPP Error 0x00000015
%% ```
%% ///
%% So if the ``no_unsuccess`` field is non zero, then that number of
%% SMEs must be appended in the form of TON, NPI & Address similarly to how
%% source and destination addresses are normally encoded. Also included per
%% failed address is the 4-octet error code (command status value) associated
%% with the failed address.
%% ///

-define(SUBMIT_MULTI_RESP,
        ?PDU([?MESSAGE_ID,
              ?UNSUCCESS_SME],
             [],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

-define(DELIVER_SM,
        ?PDU([?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21,
              ?ESM_CLASS,
              ?PROTOCOL_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [],
             [?CALLBACK_NUM,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_NETWORK_ID,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_PORT,
              ?DPF_RESULT,
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MESSAGE_PAYLOAD,
              ?MESSAGE_STATE_TLV,
              ?NETWORK_ERROR_CODE,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?RECEIPTED_MESSAGE_ID,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(DELIVER_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?NETWORK_ERROR_CODE])).

-define(BROADCAST_SM,
        ?PDU([?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?MESSAGE_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID],
             [?BROADCAST_AREA_IDENTIFIER,
              ?BROADCAST_CONTENT_TYPE,
              ?BROADCAST_FREQUENCY_INTERVAL,
              ?BROADCAST_REP_NUM,
              ?MESSAGE_PAYLOAD],
             [?ALERT_ON_MESSAGE_DELIVERY,   % (CDMA)
              ?BROADCAST_CHANNEL_INDICATOR,
              ?BROADCAST_CONTENT_TYPE_INFO, % (CDMA, TDMA)
              ?BROADCAST_MESSAGE_CLASS,
              ?BROADCAST_SERVICE_GROUP,     % (CDMA, TDMA)
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,           % (TDMA)
              ?CALLBACK_NUM_PRES_IND,       % (TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_SUBADDRESS,             % (CDMA, TDMA)
              ?DEST_PORT,
              ?DISPLAY_TIME,                % (CDMA, TDMA)
              ?LANGUAGE_INDICATOR,          % (CDMA, TDMA)
              ?MS_VALIDITY,                 % (CDMA, TDMA)
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,           % (CDMA, TDMA)
              ?SMS_SIGNAL,                  % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,           % (CDMA, TDMA)
              ?USER_MESSAGE_REFERENCE])).

-define(BROADCAST_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [],
             [?BROADCAST_ERROR_STATUS,
              ?CONGESTION_STATE,
              ?FAILED_BROADCAST_AREA_IDENTIFIER])).

-define(CANCEL_SM,
        ?PDU([?SERVICE_TYPE,
              ?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21],
             [],
             [])).

-define(CANCEL_SM_RESP,
        ?PDU([],
             [],
             [?CONGESTION_STATE])).

-define(QUERY_SM,
        ?PDU([?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [],
             [])).

-define(QUERY_SM_RESP,
        ?PDU([?MESSAGE_ID,
              ?FINAL_DATE,
              ?MESSAGE_STATE,
              ?ERROR_CODE],
             [],
             [?CONGESTION_STATE])).

-define(REPLACE_SM,
        ?PDU([?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [],
             [?MESSAGE_PAYLOAD])).  % Notice that it's optional

-define(REPLACE_SM_RESP,
        ?PDU([],
             [],
             [?CONGESTION_STATE])).

-define(QUERY_BROADCAST_SM,
        ?PDU([?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [],
             [?USER_MESSAGE_REFERENCE])).

-define(QUERY_BROADCAST_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [?BROADCAST_AREA_IDENTIFIER,
              ?BROADCAST_AREA_SUCCESS,
              ?MESSAGE_STATE_TLV],
             [?CONGESTION_STATE,
              ?BROADCAST_END_TIME,
              ?USER_MESSAGE_REFERENCE])).

-define(CANCEL_BROADCAST_SM,
        ?PDU([?SERVICE_TYPE,
              ?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [],
             [?BROADCAST_CONTENT_TYPE,
              ?USER_MESSAGE_REFERENCE])).

-define(CANCEL_BROADCAST_SM_RESP,
        ?PDU([],
             [],
             [?CONGESTION_STATE])).

-endif.  % -ifndef(smpp_pdu)


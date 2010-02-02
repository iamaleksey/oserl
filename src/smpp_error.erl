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
-module(smpp_error).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").

%%% EXTERNAL EXPORTS
-export([format/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
format(?ESME_ROK) ->
    "No Error.";
format(?ESME_RINVMSGLEN) ->
    "Message Length is invalid.";
format(?ESME_RINVCMDLEN) ->
    "Command Length is invalid.";
format(?ESME_RINVCMDID) ->
    "Invalid Command ID.";
format(?ESME_RINVBNDSTS) ->
    "Incorrect BIND Status for given command.";
format(?ESME_RALYBND) ->
    "ESME Already in Bound State.";
format(?ESME_RINVPRTFLG) ->
    "Invalid Priority Flag.";
format(?ESME_RINVREGDLVFLG) ->
    "Invalid Registered Delivery Flag.";
format(?ESME_RSYSERR) ->
    "System Error.";
format(?ESME_RINVSRCADR) ->
    "Invalid Source Address.";
format(?ESME_RINVDSTADR) ->
    "Invalid Destination Address.";
format(?ESME_RINVMSGID) ->
    "Message ID is invalid.";
format(?ESME_RBINDFAIL) ->
    "Bind Failed.";
format(?ESME_RINVPASWD) ->
    "Invalid Password.";
format(?ESME_RINVSYSID) ->
    "Invalid System ID.";
format(?ESME_RCANCELFAIL) ->
    "Cancel SM Failed.";
format(?ESME_RREPLACEFAIL) ->
    "Replace SM Failed.";
format(?ESME_RMSGQFUL) ->
    "Message Queue Full.";
format(?ESME_RINVSERTYP) ->
    "Invalid Service Type.";
format(?ESME_RINVNUMDESTS) ->
    "Invalid number of destinations.";
format(?ESME_RINVDLNAME) ->
    "Invalid Distribution List name.";
format(?ESME_RINVDESTFLAG) ->
    "Destination flag is invalid (submit_multi).";
format(?ESME_RINVSUBREP) ->
    "Submit w/replace functionality has been requested where it is either unsupported or inappropriate for the particular MC.";
format(?ESME_RINVESMCLASS) ->
    "Invalid esm_class field data.";
format(?ESME_RCNTSUBDL) ->
    "Cannot Submit to Distribution List.";
format(?ESME_RSUBMITFAIL) ->
    "submit_sm, data_sm or submit_multi failed.";
format(?ESME_RINVSRCTON) ->
    "Invalid Source address TON.";
format(?ESME_RINVSRCNPI) ->
    "Invalid Source address NPI.";
format(?ESME_RINVDSTTON) ->
    "Invalid Destination address TON.";
format(?ESME_RINVDSTNPI) ->
    "Invalid Destination address NPI.";
format(?ESME_RINVSYSTYP) ->
    "Invalid system_type field.";
format(?ESME_RINVREPFLAG) ->
    "Invalid replace_if_present flag.";
format(?ESME_RINVNUMMSGS) ->
    "Invalid number of messages.";
format(?ESME_RTHROTTLED) ->
    "Throttling error (ESME has exceeded allowed message limits).";
format(?ESME_RINVSCHED) ->
    "Invalid Scheduled Delivery Time.";
format(?ESME_RINVEXPIRY) ->
    "Invalid message validity period (Expiry time).";
format(?ESME_RINVDFTMSGID) ->
    "Predefined Message ID is Invalid or specified predefined message was not found.";
format(?ESME_RX_T_APPN) ->
    "ESME Receiver Temporary App Error Code.";
format(?ESME_RX_P_APPN) ->
    "ESME Receiver Permanent App Error Code.";
format(?ESME_RX_R_APPN) ->
    "ESME Receiver Reject Message Error Code.";
format(?ESME_RQUERYFAIL) ->
    "query_sm request failed.";
format(?ESME_RINVTLVSTREAM) ->
    "Error in the optional part of the PDU Body.";
format(?ESME_RTLVNOTALLWD) ->
    "TLV not allowed.";
format(?ESME_RINVTLVLEN) ->
    "Invalid Parameter Length.";
format(?ESME_RMISSINGTLV) ->
    "Expected TLV missing.";
format(?ESME_RINVTLVVAL) ->
    "Invalid TLV Value.";
format(?ESME_RDELIVERYFAILURE) ->
    "Transaction Delivery Failure.";
format(?ESME_RUNKNOWNERR) ->
    "Unknown Error.";
format(?ESME_RSERTYPUNAUTH) ->
    "ESME Not authorised to use specified service_type.";
format(?ESME_RPROHIBITED) ->
    "ESME Prohibited from using specified operation.";
format(?ESME_RSERTYPUNAVAIL) ->
    "Specified service_type is unavailable.";
format(?ESME_RSERTYPDENIED) ->
    "Specified service_type is denied.";
format(?ESME_RINVDCS) ->
    "Invalid Data Coding Scheme.";
format(?ESME_RINVSRCADDRSUBUNIT) ->
    "Source Address Sub unit is Invalid.";
format(?ESME_RINVDSTADDRSUBUNIT) ->
    "Destination Address Sub unit is Invalid.";
format(?ESME_RINVBCASTFREQINT) ->
    "Broadcast Frequency Interval is invalid.";
format(?ESME_RINVBCASTALIAS_NAME) ->
    "Broadcast Alias Name is invalid.";
format(?ESME_RINVBCASTAREAFMT) ->
    "Broadcast Area Format is invalid.";
format(?ESME_RINVNUMBCAST_AREAS) ->
    "Number of Broadcast Areas is invalid.";
format(?ESME_RINVBCASTCNTTYPE) ->
    "Broadcast Content Type is invalid.";
format(?ESME_RINVBCASTMSGCLASS) ->
    "Broadcast Message Class is invalid.";
format(?ESME_RBCASTFAIL) ->
    "broadcast_sm operation failed.";
format(?ESME_RBCASTQUERYFAIL) ->
    "query_broadcast_sm operation failed.";
format(?ESME_RBCASTCANCELFAIL) ->
    "cancel_broadcast_sm operation failed.";
format(?ESME_RINVBCAST_REP) ->
    "Number of Repeated Broadcasts is invalid.";
format(?ESME_RINVBCASTSRVGRP) ->
    "Broadcast Service Group is invalid.";
format(?ESME_RINVBCASTCHANIND) ->
    "Broadcast Channel Indicator is invalid.".


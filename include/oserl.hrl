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
-ifndef(oserl).
-define(oserl, true).

%%% INCLUDE FILES
-include_lib("oserl/include/smpp_globals.hrl").

%%% MACROS
-define(MAX_REF_NUM, 255).  % SMS fragment reference number

%% Sequence Number
-define(MAX_SEQUENCE_NUMBER, 16#7FFFFFFF).  % PDU sequence number
-define(INCR_SEQUENCE_NUMBER(SeqNum),
        if SeqNum == ?MAX_SEQUENCE_NUMBER -> 1; true -> SeqNum + 1 end).

%% Default Data Coding (Standardised by IANA)
%%
%% section 4.7.7 on [SMPP 5.0]
-define(DEFAULT_DATA_CODING, 16#00).

%% Default Port Number (Standardised by IANA)
%%
%% section 2.2 on [SMPP 5.0]
-define(DEFAULT_SMPP_PORT, 2775).

%% User Data Header Macros
%%
%% section 9.2.3.24 on [3G TS 23.040]
-define(UDHL_CONCAT, 5).   % UDH Length excluded length for Concatenated SMs
-define(UDHL_PORT_16, 6).  % UDHL for 16 bit Application Port Addressing
-define(UDHL_PORT_8, 4).   % UDHL for 8 bit Application Port Addressing

-define(IEI_CONCAT, 0).    % Information Element Identifier for Concatenated SMs
-define(IEI_PORT_16, 5).   % IEI for 16 bit Application Port Addressing
-define(IEI_PORT_8, 4).    % IEI for 8 bit Application Port Addressing

-define(IEDL_CONCAT, 3).   % IE Data length for Concatenated SMs excluded length
-define(IEDL_PORT_16, 4).  % IEDL for 16 bit Application Port Addressing
-define(IEDL_PORT_8, 2).   % IEDL for 8 bit Application Port Addressing

%% Short Message sizes
-define(SM_MAX_SIZE, 160).
-define(SM_MAX_SEGMENT_SIZE, 153).

%% Timers default values
%%
%% Besides the timers declared on [SMPP 5.0], a rebind timer default is
%% defined.  This timer sets the time lapse in which the ESME should try to
%% rebind once the MC becomes unavailable.</p>
%%
%% section 2.7 on [SMPP 5.0]
-define(SESSION_INIT_TIME, 180000).  % 3 minutes
-define(ENQUIRE_LINK_TIME, 60000).   % 1 minute
-define(INACTIVITY_TIME, infinity).  % No timeout, never drop the session.
-define(RESPONSE_TIME, 60000).       % 1 minute

-define(ASSERT_TIME, 120000).        % Two minutes as in TCP SYN_SENT
-define(CALL_TIME, 5000).


%% Timers record
-define(DEFAULT_TIMERS_SMPP, #timers_smpp{}).

-define(TIMERS(STime, ETime, ITime, RTime),
        #timers_smpp{session_init_time = STime,
                     enquire_link_time = ETime,
                     inactivity_time   = ITime,
                     response_time     = RTime}).

%%% RECORDS
%% {timers_smpp, SessionInitTime, EnquireLinkTime, InactivityTime, ResponseTime}
%%    SessionInitTime = int() | infinity
%%    EnquireLinkTime = int() | infinity
%%    InactivityTime  = int() | infinity
%%    ResponseTime    = int() | infinity
%%
%% The times are expressed in milliseconds.  The atom ``infinity`` disables the
%% timer.
%%
%% - SessionInitTime: Session init timer.  An ESME will close the
%%   MC-initiated connection if the MC fails to issue an outbind within the
%%   defined period of time.
%%
%%   On expiration, close the connection  (default value is ?SESSION_INIT_TIME).
%%
%% - EnquireLinkTime: Enquire link timer. Time lapse allowed
%%   between operations after which a SMPP entity should interrogate whether
%%   its peer still has an active session.
%%
%%   On expiration an enquire_link request should be initiated (default
%%   value is ?ENQUIRE_LINK_TIME).
%%
%% - InactivityTime: Inactivity timer.  Maximum time lapse allowed between
%%   transactions.
%%
%%   On expiration, close the session or issue an unbind request (default
%%   value is ?INACTIVITY_TIME.
%%
%% - ResponseTime: Response timer. Time lapse allowed between a SMPP request
%%   and the corresponding SMPP response.
%%
%%   On expiration assume the operation have failed  (default value is
%%   ?RESPONSE_TIME).
-record(timers_smpp,
        {session_init_time = ?SESSION_INIT_TIME,
         enquire_link_time = ?ENQUIRE_LINK_TIME,
         inactivity_time   = ?INACTIVITY_TIME,
         response_time     = ?RESPONSE_TIME}).

-endif.  % -ifndef(oserl)

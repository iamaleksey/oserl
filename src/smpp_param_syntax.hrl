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
-ifndef(smpp_param_syntax).
-define(smpp_param_syntax, true).

%%% MACROS
% Sections 3.2.1.5 and 3.2.1.6 on [SMPP 5.0]
-define(STANDARD(Name, Domain, Default, Error),
        #standard{name = Name,
                  domain = Domain,
                  default = Default,
                  error = Error}).
-define(TLV(Name, Tag, Domain, Reserved, Multiple, Default, Error),
        #tlv{name = Name,
             tag = Tag,
             domain = Domain,
             reserved = Reserved,
             multiple = Multiple,
             default = Default,
             error = Error}).

% Simplified TLV Macros for Readability.
-define(SIMPLE_TLV(Name, Tag, Domain, Reserved, Default, Error),
        ?TLV(Name, Tag, Domain, Reserved, false, Default, Error)).
-define(MULTIPLE_TLV(Name, Tag, Domain, Reserved, Default, Error),
        ?TLV(Name, Tag, Domain, Reserved, true, Default, Error)).

%%% RECORDS
-record(standard, {name, domain, default, error}).
-record(tlv, {name, tag, domain, reserved, multiple, default, error}).

-endif.  % -ifndef(smpp_param_syntax)

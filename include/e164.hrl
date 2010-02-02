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
-ifndef(e164).
-define(e164, true).

%%% MACROS
% As defined in http://www.numberingplans.com (International Numbering Plans)
-define(COUNTRY_CODE_AFGHANISTAN,                       93).
-define(COUNTRY_CODE_ALBANIA,                          355).
-define(COUNTRY_CODE_ALGERIA,                          213).
-define(COUNTRY_CODE_AMERICAN_SAMOA,                   684).
-define(COUNTRY_CODE_ANDORRA,                          376).
-define(COUNTRY_CODE_ANGOLA,                           244).
-define(COUNTRY_CODE_ANGUILLA,                           1).
-define(COUNTRY_CODE_ANTIGUA_AND_BARBUDA,                1).
-define(COUNTRY_CODE_ARGENTINE_REPUBLIC,                54).
-define(COUNTRY_CODE_ARMENIA,                          374).
-define(COUNTRY_CODE_ARUBA,                            297).
-define(COUNTRY_CODE_ASCENSION,                        247).
-define(COUNTRY_CODE_AUSTRALIA,                         61).
-define(COUNTRY_CODE_AUSTRALIAN_EXTERNAL_TERRITORIES,  672).
-define(COUNTRY_CODE_AUSTRIA,                           43).
-define(COUNTRY_CODE_AZERBAIJANI_REPUBLIC,             994).
-define(COUNTRY_CODE_BAHAMAS,                            1).
-define(COUNTRY_CODE_BAHRAIN,                          973).
-define(COUNTRY_CODE_BANGLADESH,                       880).
-define(COUNTRY_CODE_BARBADOS,                           1).
-define(COUNTRY_CODE_BELARUS,                          375).
-define(COUNTRY_CODE_BELGIUM,                           32).
-define(COUNTRY_CODE_BELIZE,                           501).
-define(COUNTRY_CODE_BENIN,                            229).
-define(COUNTRY_CODE_BERMUDA,                            1).
-define(COUNTRY_CODE_BHUTAN,                           975).
-define(COUNTRY_CODE_BOLIVIA,                          591).
-define(COUNTRY_CODE_BOSNIA_AND_HERZEGOVINA,           387).
-define(COUNTRY_CODE_BOTSWANA,                         267).
-define(COUNTRY_CODE_BRAZIL,                            55).
-define(COUNTRY_CODE_BRITISH_VIRGIN_ISLANDS,             1).
-define(COUNTRY_CODE_BRUNEI_DARUSSALAM,                673).
-define(COUNTRY_CODE_BULGARIA,                         359).
-define(COUNTRY_CODE_BURKINA_FASO,                     226).
-define(COUNTRY_CODE_BURUNDI,                          257).
-define(COUNTRY_CODE_CAMBODIA,                         855).
-define(COUNTRY_CODE_CAMEROON,                         237).
-define(COUNTRY_CODE_CANADA,                             1).
-define(COUNTRY_CODE_CAPE_VERDE,                       238).
-define(COUNTRY_CODE_CAYMAN_ISLANDS,                     1).
-define(COUNTRY_CODE_CENTRAL_AFRICAN_REPUBLIC,         236).
-define(COUNTRY_CODE_CHAD,                             235).
-define(COUNTRY_CODE_CHILE,                             56).
-define(COUNTRY_CODE_CHINA,                             86).
-define(COUNTRY_CODE_CHRISTMAS_ISLAND,                  61).
-define(COUNTRY_CODE_COCOS,                             61).
-define(COUNTRY_CODE_COLOMBIA,                          57).
-define(COUNTRY_CODE_COMOROS,                          269).
-define(COUNTRY_CODE_CONGO_BRAZZAVILLE,                242).
-define(COUNTRY_CODE_CONGO_KINSHASA,                   243).
-define(COUNTRY_CODE_COOK_ISLANDS,                     682).
-define(COUNTRY_CODE_COSTA_RICA,                       506).
-define(COUNTRY_CODE_COTE_D_IVOIRE,                    225).
-define(COUNTRY_CODE_CROATIA,                          385).
-define(COUNTRY_CODE_CUBA,                              53).
-define(COUNTRY_CODE_CYPRUS,                           357).
-define(COUNTRY_CODE_CZECH_REPUBLIC,                   420).
-define(COUNTRY_CODE_DENMARK,                           45).
-define(COUNTRY_CODE_DIEGO_GARCIA,                     246).
-define(COUNTRY_CODE_DJIBOUTI,                         253).
-define(COUNTRY_CODE_DOMINICA,                           1).
-define(COUNTRY_CODE_DOMINICAN_REPUBLIC,                 1).
-define(COUNTRY_CODE_EAST_TIMOR,                       670).
-define(COUNTRY_CODE_ECUADOR,                          593).
-define(COUNTRY_CODE_EGYPT,                             20).
-define(COUNTRY_CODE_EL_SALVADOR,                      503).
-define(COUNTRY_CODE_EQUATORIAL_GUINEA,                240).
-define(COUNTRY_CODE_ERITREA,                          291).
-define(COUNTRY_CODE_ESTONIA,                          372).
-define(COUNTRY_CODE_ETHIOPIA,                         251).
-define(COUNTRY_CODE_EUROPE,                           388).
-define(COUNTRY_CODE_FALKLAND_ISLANDS,                 500).
-define(COUNTRY_CODE_FAROE_ISLANDS,                    298).
-define(COUNTRY_CODE_FIJI,                             679).
-define(COUNTRY_CODE_FINLAND,                          358).
-define(COUNTRY_CODE_FRANCE,                            33).
-define(COUNTRY_CODE_FRENCH_GUIANA,                    594).
-define(COUNTRY_CODE_FRENCH_POLYNESIA,                 689).
-define(COUNTRY_CODE_GABONESE_REPUBLIC,                241).
-define(COUNTRY_CODE_GAMBIA,                           220).
-define(COUNTRY_CODE_GEORGIA,                          995).
-define(COUNTRY_CODE_GERMANY,                           49).
-define(COUNTRY_CODE_GHANA,                            233).
-define(COUNTRY_CODE_GIBRALTAR,                        350).
-define(COUNTRY_CODE_GLOBAL_MOBILE_SATELLITE_SYSTEM,   881).
-define(COUNTRY_CODE_GREECE,                            30).
-define(COUNTRY_CODE_GREENLAND,                        299).
-define(COUNTRY_CODE_GRENADA_AND_CARRIACOU,              1).
-define(COUNTRY_CODE_GUADELOUPE,                       590).
-define(COUNTRY_CODE_GUAM,                               1).
-define(COUNTRY_CODE_GUATEMALA,                        502).
-define(COUNTRY_CODE_GUINEA,                           224).
-define(COUNTRY_CODE_GUINEA_BISSAU,                    245).
-define(COUNTRY_CODE_GUYANA,                           592).
-define(COUNTRY_CODE_HAITI,                            509).
-define(COUNTRY_CODE_HONDURAS,                         504).
-define(COUNTRY_CODE_HONG_KONG,                        852).
-define(COUNTRY_CODE_HUNGARY,                           36).
-define(COUNTRY_CODE_ICELAND,                          354).
-define(COUNTRY_CODE_INDIA,                             91).
-define(COUNTRY_CODE_INDONESIA,                         62).
-define(COUNTRY_CODE_INMARSAT_ATLANTIC_OCEAN_EAST,     871).
-define(COUNTRY_CODE_INMARSAT_ATLANTIC_OCEAN_WEST,     874).
-define(COUNTRY_CODE_INMARSAT_INDIAN_OCEAN,            873).
-define(COUNTRY_CODE_INMARSAT_PACIFIC_OCEAN,           872).
-define(COUNTRY_CODE_INMARSAT_SINGLE_NUMBER_ACCESS,    870).
-define(COUNTRY_CODE_INTERNATIONAL_FREEPHONE_SERVICE,  800).
-define(COUNTRY_CODE_INTERNATIONAL_NETWORKS,           882).
-define(COUNTRY_CODE_INTERNATIONAL_PREMIUM_RATE_SERVI, 979).
-define(COUNTRY_CODE_INTERNATIONAL_PUBLIC_CORRESPONDE, 991).
-define(COUNTRY_CODE_INTERNATIONAL_SHARED_COST_SERVIC, 808).
-define(COUNTRY_CODE_IRAN,                              98).
-define(COUNTRY_CODE_IRAQ,                             964).
-define(COUNTRY_CODE_IRELAND,                          353).
-define(COUNTRY_CODE_ISRAEL,                           972).
-define(COUNTRY_CODE_ITALY,                             39).
-define(COUNTRY_CODE_JAMAICA,                            1).
-define(COUNTRY_CODE_JAPAN,                             81).
-define(COUNTRY_CODE_JORDAN,                           962).
-define(COUNTRY_CODE_KAZAKSTAN,                          7).
-define(COUNTRY_CODE_KENYA,                            254).
-define(COUNTRY_CODE_KIRIBATI,                         686).
-define(COUNTRY_CODE_KOREA,                            850).
-define(COUNTRY_CODE_KOREA_2,                           82).
-define(COUNTRY_CODE_KUWAIT,                           965).
-define(COUNTRY_CODE_KYRGYZ_REPUBLIC,                  996).
-define(COUNTRY_CODE_LAO_PEOPLE_S_DEMOCRATIC_REPUBLIC, 856).
-define(COUNTRY_CODE_LATVIA,                           371).
-define(COUNTRY_CODE_LEBANON,                          961).
-define(COUNTRY_CODE_LESOTHO,                          266).
-define(COUNTRY_CODE_LIBERIA,                          231).
-define(COUNTRY_CODE_LIBYA,                            218).
-define(COUNTRY_CODE_LIECHTENSTEIN,                    423).
-define(COUNTRY_CODE_LITHUANIA,                        370).
-define(COUNTRY_CODE_LUXEMBOURG,                       352).
-define(COUNTRY_CODE_MACAO,                            853).
-define(COUNTRY_CODE_MACEDONIA,                        389).
-define(COUNTRY_CODE_MADAGASCAR,                       261).
-define(COUNTRY_CODE_MALAWI,                           265).
-define(COUNTRY_CODE_MALAYSIA,                          60).
-define(COUNTRY_CODE_MALDIVES,                         960).
-define(COUNTRY_CODE_MALI,                             223).
-define(COUNTRY_CODE_MALTA,                            356).
-define(COUNTRY_CODE_MARSHALL_ISLANDS,                 692).
-define(COUNTRY_CODE_MARTINIQUE,                       596).
-define(COUNTRY_CODE_MAURITANIA,                       222).
-define(COUNTRY_CODE_MAURITIUS,                        230).
-define(COUNTRY_CODE_MAYOTTE,                          269).
-define(COUNTRY_CODE_MEXICO,                            52).
-define(COUNTRY_CODE_MICRONESIA,                       691).
-define(COUNTRY_CODE_MOLDOVA,                          373).
-define(COUNTRY_CODE_MONACO,                           377).
-define(COUNTRY_CODE_MONGOLIA,                         976).
-define(COUNTRY_CODE_MONTSERRAT,                         1).
-define(COUNTRY_CODE_MOROCCO,                          212).
-define(COUNTRY_CODE_MOZAMBIQUE,                       258).
-define(COUNTRY_CODE_MYANMAR,                           95).
-define(COUNTRY_CODE_NAMIBIA,                          264).
-define(COUNTRY_CODE_NAURU,                            674).
-define(COUNTRY_CODE_NEPAL,                            977).
-define(COUNTRY_CODE_NETHERLANDS,                       31).
-define(COUNTRY_CODE_NETHERLANDS_ANTILLES,             599).
-define(COUNTRY_CODE_NEW_CALEDONIA,                    687).
-define(COUNTRY_CODE_NEW_ZEALAND,                       64).
-define(COUNTRY_CODE_NICARAGUA,                        505).
-define(COUNTRY_CODE_NIGER,                            227).
-define(COUNTRY_CODE_NIGERIA,                          234).
-define(COUNTRY_CODE_NIUE,                             683).
-define(COUNTRY_CODE_NORTHERN_MARIANA_ISLANDS,           1).
-define(COUNTRY_CODE_NORWAY,                            47).
-define(COUNTRY_CODE_OMAN,                             968).
-define(COUNTRY_CODE_PAKISTAN,                          92).
-define(COUNTRY_CODE_PALAU,                            680).
-define(COUNTRY_CODE_PALESTINE,                        970).
-define(COUNTRY_CODE_PANAMA,                           507).
-define(COUNTRY_CODE_PAPUA_NEW_GUINEA,                 675).
-define(COUNTRY_CODE_PARAGUAY,                         595).
-define(COUNTRY_CODE_PERU,                              51).
-define(COUNTRY_CODE_PHILIPPINES,                       63).
-define(COUNTRY_CODE_POLAND,                            48).
-define(COUNTRY_CODE_PORTUGAL,                         351).
-define(COUNTRY_CODE_PUERTO_RICO,                        1).
-define(COUNTRY_CODE_QATAR,                            974).
-define(COUNTRY_CODE_REUNION,                          262).
-define(COUNTRY_CODE_ROMANIA,                           40).
-define(COUNTRY_CODE_RUSSIAN_FEDERATION,                 7).
-define(COUNTRY_CODE_RWANDESE_REPUBLIC,                250).
-define(COUNTRY_CODE_SAINT_HELENA,                     290).
-define(COUNTRY_CODE_SAINT_KITTS_AND_NEVIS,              1).
-define(COUNTRY_CODE_SAINT_LUCIA,                        1).
-define(COUNTRY_CODE_SAINT_PIERRE_AND_MIQUELON,        508).
-define(COUNTRY_CODE_SAINT_VINCENT_AND_THE_GRENADINES,   1).
-define(COUNTRY_CODE_SAMOA,                            685).
-define(COUNTRY_CODE_SAN_MARINO,                       378).
-define(COUNTRY_CODE_SAO_TOME_AND_PRINCIPE,            239).
-define(COUNTRY_CODE_SAUDI_ARABIA,                     966).
-define(COUNTRY_CODE_SENEGAL,                          221).
-define(COUNTRY_CODE_SERBIA_AND_MONTENEGRO,            381).
-define(COUNTRY_CODE_SEYCHELLES,                       248).
-define(COUNTRY_CODE_SIERRA_LEONE,                     232).
-define(COUNTRY_CODE_SINGAPORE,                         65).
-define(COUNTRY_CODE_SLOVAK_REPUBLIC,                  421).
-define(COUNTRY_CODE_SLOVENIA,                         386).
-define(COUNTRY_CODE_SOLOMON_ISLANDS,                  677).
-define(COUNTRY_CODE_SOMALI_DEMOCRATIC_REPUBLIC,       252).
-define(COUNTRY_CODE_SOUTH_AFRICA,                      27).
-define(COUNTRY_CODE_SPAIN,                             34).
-define(COUNTRY_CODE_SRI_LANKA,                         94).
-define(COUNTRY_CODE_SUDAN,                            249).
-define(COUNTRY_CODE_SURINAME,                         597).
-define(COUNTRY_CODE_SWAZILAND,                        268).
-define(COUNTRY_CODE_SWEDEN,                            46).
-define(COUNTRY_CODE_SWITZERLAND,                       41).
-define(COUNTRY_CODE_SYRIAN_ARAB_REPUBLIC,             963).
-define(COUNTRY_CODE_TAIWAN,                           886).
-define(COUNTRY_CODE_TAJIKISTAN,                       992).
-define(COUNTRY_CODE_TANZANIA,                         255).
-define(COUNTRY_CODE_THAILAND,                          66).
-define(COUNTRY_CODE_TOGOLESE_REPUBLIC,                228).
-define(COUNTRY_CODE_TOKELAU,                          690).
-define(COUNTRY_CODE_TONGA,                            676).
-define(COUNTRY_CODE_TRINIDAD_AND_TOBAGO,                1).
-define(COUNTRY_CODE_TUNISIA,                          216).
-define(COUNTRY_CODE_TURKEY,                            90).
-define(COUNTRY_CODE_TURKMENISTAN,                     993).
-define(COUNTRY_CODE_TURKS_AND_CAICOS_ISLANDS,           1).
-define(COUNTRY_CODE_TUVALU,                           688).
-define(COUNTRY_CODE_UGANDA,                           256).
-define(COUNTRY_CODE_UKRAINE,                          380).
-define(COUNTRY_CODE_UNITED_ARAB_EMIRATES,             971).
-define(COUNTRY_CODE_UNITED_KINGDOM_OF_GREAT_BRITAIN,   44).
-define(COUNTRY_CODE_UNITED_STATES_OF_AMERICA,           1).
-define(COUNTRY_CODE_UNITED_STATES_VIRGIN_ISLANDS,       1).
-define(COUNTRY_CODE_UNIVERSAL_PERSONAL_TELECOMMUNICA, 878).
-define(COUNTRY_CODE_URUGUAY,                          598).
-define(COUNTRY_CODE_UZBEKISTAN,                       998).
-define(COUNTRY_CODE_VANUATU,                          678).
-define(COUNTRY_CODE_VATICAN_CITY_STATE,                39).
-define(COUNTRY_CODE_VENEZUELA,                         58).
-define(COUNTRY_CODE_VIET_NAM,                          84).
-define(COUNTRY_CODE_WALLIS_AND_FUTUNA,                681).
-define(COUNTRY_CODE_YEMEN,                            967).
-define(COUNTRY_CODE_ZAMBIA,                           260).
-define(COUNTRY_CODE_ZIMBABWE,                         263).

-endif.  % -ifndef(e164)

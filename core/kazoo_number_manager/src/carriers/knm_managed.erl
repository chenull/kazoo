%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_managed).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-export([generate_numbers/3]).
-export([import_numbers/2]).

-include("knm.hrl").

-define(KZ_MANAGED, <<"numbers%2Fmanaged">>).
-define(MANAGED_VIEW_FILE, <<"views/numbers_managed.json">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 15
     }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check with carrier if these numbers are registered with it.
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(ne_binaries()) -> {ok, kz_json:object()} |
                                      {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), knm_search:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(<<"+", _/binary>>=Prefix, Quantity, Options) ->
    AccountId = knm_search:account_id(Options),
    find_numbers_in_account(Prefix, Quantity, AccountId, Options);
find_numbers(Prefix, Quantity, Options) ->
    find_numbers(<<"+",Prefix/binary>>, Quantity, Options).

-spec find_numbers_in_account(ne_binary(), pos_integer(), api_binary(), knm_search:options()) ->
                                     {'ok', knm_number:knm_numbers()} |
                                     {'error', any()}.
find_numbers_in_account(Prefix, Quantity, AccountId, Options) ->
    case do_find_numbers_in_account(Prefix, Quantity, AccountId, Options) of
        {'error', 'not_available'}=Error ->
            ResellerId = knm_search:reseller_id(Options),
            case AccountId =:= 'undefined'
                orelse AccountId =:= ResellerId
            of
                'true' -> Error;
                'false' ->
                    find_numbers_in_account(Prefix, Quantity, ResellerId, Options)
            end;
        Result -> Result
    end.

-spec do_find_numbers_in_account(ne_binary(), pos_integer(), api_binary(), knm_search:options()) ->
                                        {'ok', list()} |
                                        {'error', any()}.
do_find_numbers_in_account(Prefix, Quantity, AccountId, Options) ->
    ViewOptions = [{'startkey', [AccountId, ?NUMBER_STATE_AVAILABLE, Prefix]}
                  ,{'endkey', [AccountId, ?NUMBER_STATE_AVAILABLE, <<Prefix/binary,"\ufff0">>]}
                  ,{'limit', Quantity}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(?KZ_MANAGED, <<"numbers_managed/status">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("found no available managed numbers for account ~s", [AccountId]),
            {'error', 'not_available'};
        {'ok', JObjs} ->
            lager:debug("found ~B available managed numbers for account ~s", [length(JObjs), AccountId]),
            format_numbers_resp(JObjs, Options);
        {'error', _R}=E ->
            lager:debug("failed to lookup available managed numbers: ~p", [_R]),
            E
    end.

-spec format_numbers_resp(kz_json:objects(), knm_search:options()) -> {'ok', list()}.
format_numbers_resp(JObjs, Options) ->
    QID = knm_search:query_id(Options),
    Numbers = [format_number_resp(QID, JObj) || JObj <- JObjs],
    {'ok', Numbers}.

format_number_resp(QID, JObj) ->
    Num = kz_doc:id(kz_json:get_value(<<"doc">>, JObj)),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, kz_json:new()}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignTo = knm_phone_number:assigned_to(PhoneNumber),
    State = knm_phone_number:state(PhoneNumber),
    lager:debug("acquiring number ~s", [knm_phone_number:number(PhoneNumber)]),
    update_doc(Number, [{?PVT_STATE, State}
                       ,{?PVT_ASSIGNED_TO, AssignTo}
                       ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    lager:debug("disconnecting number ~s"
               ,[knm_phone_number:number(knm_number:phone_number(Number))]),
    update_doc(Number, [{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
                       ,{?PVT_ASSIGNED_TO, null}
                        %%                       ,{?PVT_ASSIGNED_TO, <<>>}
                       ]).

-spec generate_numbers(ne_binary(), pos_integer(), non_neg_integer()) -> 'ok'.
generate_numbers(_AccountId, _Number, 0) -> 'ok';
generate_numbers(?MATCH_ACCOUNT_RAW(AccountId), Number, Quantity)
  when Quantity > 0
       andalso is_integer(Number)
       andalso is_integer(Quantity) ->
    {'ok', _JObj} = save_doc(AccountId, <<"+",(kz_term:to_binary(Number))/binary>>),
    generate_numbers(AccountId, Number+1, Quantity-1).

-spec import_numbers(ne_binary(), ne_binaries()) -> kz_json:object().
import_numbers(AccountId, Numbers) ->
    import_numbers(AccountId, Numbers, kz_json:new()).

-spec import_numbers(ne_binary(), ne_binaries(), kz_json:object()) -> kz_json:object().
import_numbers(_AccountId, [], JObj) -> JObj;
import_numbers(AccountId, [Number | Numbers], JObj) ->
    NewJObj =
        case save_doc(AccountId, Number) of
            {'ok', _Doc} ->
                kz_json:set_value([<<"success">>, Number], kz_json:new(), JObj);
            {'error', Reason} ->
                Error = kz_json:from_list([{<<"reason">>, Reason}
                                          ,{<<"message">>, <<"error adding number to DB">>}
                                          ]),
                kz_json:set_value([<<"errors">>, Number], Error, JObj)
        end,
    import_numbers(AccountId, Numbers, NewJObj).

-spec save_doc(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                            {'error', any()}.
save_doc(AccountId, Number) ->
    JObj = kz_json:from_list([{<<"_id">>, knm_converters:normalize(Number)}
                             ,{<<"pvt_account_id">>, AccountId}
                             ,{?PVT_STATE, ?NUMBER_STATE_AVAILABLE}
                             ,{?PVT_TYPE, <<"number">>}
                             ]),
    save_doc(JObj).

-spec save_doc(kz_json:object()) -> {'ok', kz_json:object()} |
                                    {'error', any()}.
save_doc(JObj) ->
    case kz_datamgr:save_doc(?KZ_MANAGED, JObj) of
        {'error', 'not_found'} ->
            'true' = kz_datamgr:db_create(?KZ_MANAGED),
            {'ok', _View} = kz_datamgr:revise_doc_from_file(?KZ_MANAGED, ?APP, ?MANAGED_VIEW_FILE),
            save_doc(JObj);
        Result -> Result
    end.

-spec update_doc(knm_number:knm_number(), kz_proplist()) ->
                        knm_number:knm_number().
update_doc(Number, UpdateProps) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_phone_number:number(PhoneNumber),
    case kz_datamgr:update_doc(?KZ_MANAGED, Num, [{?PVT_MODULE_NAME, kz_term:to_binary(?MODULE)}
                                                  | UpdateProps
                                                 ])
    of
        {'ok', _UpdatedDoc} -> Number;
        {'error', Reason} ->
            knm_errors:database_error(Reason, PhoneNumber)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

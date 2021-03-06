%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_ips).

-include("kazoo_ips.hrl").

-export([available/0
        ,available/1
        ,available/2
        ]).
-export([assigned/1]).
-export([zones/0]).
-export([hosts/0]).
-export([summary/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available() -> {'ok', kz_json:objects()} |
                     {'error', any()}.
-spec available(api_binary()) ->
                       {'ok', kz_json:objects()} |
                       {'error', any()}.

available() -> available('undefined').

available(Zone) -> available(Zone, 1).

-spec available(api_binary(), non_neg_integer()) ->
                       {'ok', kz_json:objects()} |
                       {'error', any()}.
available(Zone, Quantity) ->
    ViewOptions = props:filter_undefined(
                    [{'key', Zone}
                    ,{'limit', Quantity}
                    ]
                   ),
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/available_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> available(Zone, Quantity) end);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
        {'error', _R}=E ->
            lager:debug("unable to get available dedicated ips: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec assigned(ne_binary()) ->
                      {'ok', kz_json:objects()} |
                      {'error', any()}.
assigned(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ViewOptions = [{'key', AccountId}],
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/assigned_to_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> assigned(Account) end);
        {'ok', JObjs} ->
            {'ok', sort_assigned([kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs])};
        {'error', _R}=E ->
            lager:debug("unable to get assigned dedicated ips: ~p", [_R]),
            E
    end.

-spec sort_assigned(kz_json:objects()) -> kz_json:objects().
sort_assigned(IPs) ->
    ZoneName = get_zone_name(),
    sort_assigned(kz_term:shuffle_list(IPs), ZoneName, []).

-spec sort_assigned(kz_json:objects(), ne_binary(), kz_json:objects()) -> kz_json:objects().
sort_assigned([], _, Sorted) -> Sorted;
sort_assigned([IP|IPs], ZoneName, Sorted) ->
    case kz_json:get_value(<<"zone">>, IP) =:= ZoneName of
        'true' -> sort_assigned(IPs, ZoneName, [IP] ++ Sorted);
        'false' -> sort_assigned(IPs, ZoneName, Sorted ++ [IP])
    end.

-spec get_zone_name() -> ne_binary().
get_zone_name() ->
    LocalZone = kz_term:to_binary(kz_nodes:local_zone()),
    NameMap = kapps_config:get_json(?CONFIG_CAT, <<"zone_name_map">>, kz_json:new()),
    kz_json:get_ne_value(LocalZone, NameMap, LocalZone).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec zones() ->
                   {'ok', ne_binaries()} |
                   {'error', any()}.
zones() ->
    ViewOptions = ['group'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/zone_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun zones/0);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"key">>, JObj)
                    || JObj <- JObjs
                   ]};
        {'error', _R}=E ->
            lager:debug("unable to get zones: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec hosts() ->
                   {'ok', ne_binaries()} |
                   {'error', any()}.
hosts() ->
    ViewOptions = ['group'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/host_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun hosts/0);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"key">>, JObj)
                    || JObj <- JObjs
                   ]};
        {'error', _R}=E ->
            lager:debug("unable to get hosts: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec summary(api_binary()) ->
                     {'ok', kz_json:objects()} |
                     {'error', any()}.
summary(Host) ->
    ViewOptions = props:filter_undefined([{'key', Host}]),
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/summary_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> summary(Host) end);
        {'ok', JObjs} -> {'ok', [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
        {'error', _R}=E ->
            lager:debug("unable to get host ips: ~p", [_R]),
            E
    end.

%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(kz_types).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kazoo_stdlib/include/kz_records.hrl").

-type text() :: string() | atom() | binary() | iolist().

-type atoms() :: [atom()].
-type pids() :: [pid()].
-type references() :: [reference()].

-type kz_proplist_key() :: any().
-type kz_proplist_value() :: any().
-type kz_proplist_property() :: atom() | {kz_proplist_key(), kz_proplist_value()}.
-type kz_proplist() :: [kz_proplist_property()].
-type kz_proplists() :: [kz_proplist()].
-type kz_proplist_kv(K, V) :: [{K, V}].

-type pid_ref() :: {pid(), reference()}.
-type pid_refs() :: [pid_ref()].
-type api_pid_ref() :: pid_ref() | 'undefined'.
-type api_pid_refs() :: pid_refs() | 'undefined'.

-type api_terms() :: kz_json:object() | kz_proplist().
-type api_binary() :: binary() | 'undefined'.
-type api_ne_binary() :: ne_binary() | 'undefined'.
-type api_ne_binaries() :: [api_ne_binary()] | 'undefined'.
-type api_binaries() :: [api_binary()] | 'undefined'.
-type api_object() :: kz_json:object() | 'undefined'.
-type api_objects() :: kz_json:objects() | 'undefined'.
-type api_boolean() :: boolean() | 'undefined'.
-type api_atom() :: atom() | 'undefined'.
-type api_atoms() :: atoms() | 'undefined'.
-type api_string() :: string() | 'undefined'.
-type api_reference() :: reference() | 'undefined'.
-type api_pid() :: pid() | 'undefined'.
-type api_list() :: list() | 'undefined'.

-type api_number() :: number() | 'undefined'.
-type api_integer() :: integer() | 'undefined'.
-type api_pos_integer() :: pos_integer() | 'undefined'.
-type api_non_neg_integer() :: non_neg_integer() | 'undefined'.
-type api_float() :: float() | 'undefined'.

-type kz_deeplist() :: iolist(). %[any() | kz_deeplist()].

-type kz_std_return() :: {'ok', any()} | {'error', any()}.

-type kz_jobj_return() :: {'ok', kz_json:object()} | {'error', any()}.
-type kz_jobjs_return() :: {'ok', kz_json:objects()} | {'error', any()}.

-type ne_binary() :: <<_:8,_:_*8>>.
-type ne_binaries() :: [ne_binary()].
-type binaries() :: [binary()].

-type strings() :: [string()].
-type integers() :: [integer()].

-type functions() :: [function()].

%% when using gen_smtp to send emails, it takes a 5-tuple for a message-body part
-type mail_message_body() :: {ne_binary(), ne_binary(), kz_proplist(), kz_proplist(), ne_binary() | iolist()}.

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

-type kz_now() :: erlang:timestamp().
-type kz_year() :: non_neg_integer().
-type kz_month() :: 1..12.
-type kz_day() :: 1..31.
-type kz_hour() :: 0..23.
-type kz_minute() :: 0..59.
-type kz_second() :: 0..59.
-type kz_daynum() :: 1..7.
-type kz_weeknum() :: 1..53.
-type kz_date() :: calendar:date(). %%{kz_year(), kz_month(), kz_day()}.
-type kz_time() :: calendar:time(). %%{kz_hour(), kz_minute(), kz_second()}.
-type kz_datetime() :: calendar:datetime(). %%{kz_date(), kz_time()}.
-type kz_iso_week() :: calendar:yearweeknum(). %%{kz_year(), kz_weeknum()}.
-type gregorian_seconds() :: pos_integer().
-type unix_seconds() :: pos_integer().
-type api_seconds() :: 'undefined' | gregorian_seconds().

-type kz_timeout() :: non_neg_integer() | 'infinity'.

-type kz_ip_list() :: ne_binaries().

%% Recreate the non-exported types defined in the Erlang supervisor source
-type sup_child_spec() :: supervisor:child_spec().
-type sup_child_specs() :: [sup_child_spec()].
-type sup_start_flags() :: supervisor:sup_flags().

-type sup_init_ret() :: {'ok', {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                        'ignore'.

-type sup_child_id() :: api_pid().
-type sup_startchild_err() :: 'already_present' |
                              {'already_started', sup_child_id()} |
                              any().
-type sup_startchild_ret() :: {'ok', sup_child_id()} |
                              {'ok', sup_child_id(), any()} |
                              {'error', sup_startchild_err()}.

%% Recreate the non-exported types defined in the Erlang gen_server source
-type startlink_err() :: {'already_started', pid()} |
                         'shutdown' |
                         any().
-type startlink_ret() :: {'ok', pid()} |
                         'ignore' |
                         {'error', startlink_err()}.
-type startapp_ret() :: {'ok', pid()} |
                        {'ok', pid(), any()} |
                        {'error', startlink_err()}.

-type call_from() :: pid_ref().
-type gen_server_timeout() :: 'hibernate' | non_neg_integer().
-type handle_call_ret() :: {'reply', any(), any()} |
                           {'reply', any(), any(), gen_server_timeout()} |
                           {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()} |
                           {'stop', any(), any(), any()}.

-type handle_call_ret_state(State) :: {'reply', any(), State} |
                                      {'reply', any(), State, gen_server_timeout()} |
                                      {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State} |
                                      {'stop', any(), State, any()}.

-type handle_cast_ret() :: {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()}.
-type handle_cast_ret_state(State) :: {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State}.

-type handle_info_ret() :: {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()}.
-type handle_info_ret_state(State) :: {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State}.

-type handle_fsm_ret(State) :: {'next_state', atom(), State} |
                               {'next_state', atom(), State, timeout() | 'hibernate'} |
                               {'stop', any(), State}.

-type handle_sync_event_ret(State) :: handle_fsm_ret(State) |
                                      {'reply', any(), atom(), State} |
                                      {'reply', any(), atom(), State, timeout() | 'hibernate'} |
                                      {'stop', any(), any(), State}.

-type server_ref() :: atom() |
                      {atom(), atom()} |
                      {'global', any()} |
                      {'via', atom(), any()} |
                      pid().

-type gen_server_name() :: {'local', atom()} |
                           {'global', any()} |
                           {'via', atom(), any()}.
-type gen_server_option() :: {'debug', list()} |
                             {'timeout', non_neg_integer()} |
                             {'spawn_opt', list()}.
-type gen_server_options() :: [gen_server_option()].


%% XML types
-type xml_attrib_name() :: atom().
-type xml_attrib_value() :: ne_binary() | nonempty_string() | iolist() | atom() | number().
-type xml_attrib() :: #xmlAttribute{}.
-type xml_attribs() :: [xml_attrib()].

-type xml_el() :: #xmlElement{}.
-type xml_els() :: [xml_el()].

-type xml_text() :: #xmlText{value :: iolist()}.
-type xml_texts() :: [xml_text()].


-type xml_thing() :: xml_el() | xml_text().
-type xml_things() :: xml_els() | xml_texts().


%% KZ_NODES types

-type whapp_info() :: #whapp_info{}.
-type kapps_info() :: [{binary(), whapp_info()}].

-type media_server() :: {ne_binary(), kz_json:object()}.
-type media_servers() :: [media_server()].

-type kz_node() :: #kz_node{}.
-type kz_nodes() :: [kz_node()].

-export_type([text/0
             ,atoms/0
             ,pids/0
             ,references/0
             ,kz_proplist_key/0
             ,kz_proplist_value/0
             ,kz_proplist_property/0
             ,kz_proplist/0
             ,kz_proplists/0
             ,kz_proplist_kv/2
             ,pid_ref/0
             ,pid_refs/0
             ,api_pid_ref/0
             ,api_pid_refs/0
             ,api_terms/0
             ,api_binary/0
             ,api_ne_binary/0
             ,api_ne_binaries/0
             ,api_binaries/0
             ,api_object/0
             ,api_objects/0
             ,api_boolean/0
             ,api_atom/0
             ,api_atoms/0
             ,api_string/0
             ,api_reference/0
             ,api_pid/0
             ,api_list/0
             ,api_number/0
             ,api_integer/0
             ,api_pos_integer/0
             ,api_non_neg_integer/0
             ,api_float/0
             ,kz_deeplist/0
             ,kz_std_return/0
             ,kz_jobj_return/0
             ,kz_jobjs_return/0
             ,ne_binary/0
             ,ne_binaries/0
             ,binaries/0
             ,strings/0
             ,integers/0
             ,functions/0
             ,mail_message_body/0
             ,dict/2
             ,kz_now/0
             ,kz_year/0
             ,kz_month/0
             ,kz_day/0
             ,kz_hour/0
             ,kz_minute/0
             ,kz_second/0
             ,kz_daynum/0
             ,kz_weeknum/0
             ,kz_date/0
             ,kz_time/0
             ,kz_datetime/0
             ,kz_iso_week/0
             ,gregorian_seconds/0
             ,unix_seconds/0
             ,api_seconds/0
             ,kz_timeout/0
             ,kz_ip_list/0
             ,sup_child_spec/0
             ,sup_child_specs/0
             ,sup_start_flags/0
             ,sup_init_ret/0
             ,sup_child_id/0
             ,sup_startchild_err/0
             ,sup_startchild_ret/0
             ,startlink_err/0
             ,startlink_ret/0
             ,startapp_ret/0
             ,call_from/0
             ,gen_server_timeout/0
             ,handle_call_ret/0
             ,handle_call_ret_state/1
             ,handle_cast_ret/0
             ,handle_cast_ret_state/1
             ,handle_info_ret/0
             ,handle_info_ret_state/1
             ,handle_fsm_ret/1
             ,handle_sync_event_ret/1
             ,server_ref/0
             ,gen_server_name/0
             ,gen_server_option/0
             ,gen_server_options/0
             ,xml_attrib_name/0
             ,xml_attrib_value/0
             ,xml_attrib/0
             ,xml_attribs/0
             ,xml_el/0
             ,xml_els/0
             ,xml_text/0
             ,xml_texts/0
             ,xml_thing/0
             ,xml_things/0
             ,whapp_info/0
             ,kapps_info/0
             ,media_server/0
             ,media_servers/0
             ,kz_node/0
             ,kz_nodes/0
             ]).

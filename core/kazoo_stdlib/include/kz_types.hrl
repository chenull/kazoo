-ifndef(KAZOO_TYPES_INCLUDED).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kazoo_stdlib/include/kz_records.hrl").

-define(MILLISECONDS_IN_SECOND, 1000).
-define(MILLISECONDS_IN_MINUTE, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_MINUTE)).
-define(MILLISECONDS_IN_HOUR, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_HOUR)).
-define(MILLISECONDS_IN_DAY, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_DAY)).

-define(MICROSECONDS_IN_SECOND, (1000 * ?MILLISECONDS_IN_SECOND)).

-define(SECONDS_IN_MINUTE,     60).
-define(SECONDS_IN_HOUR,     3600).
-define(SECONDS_IN_DAY,     86400).
-define(SECONDS_IN_WEEK,   604800).
-define(SECONDS_IN_YEAR, 31540000).

-define(BYTES_K, 1024).
-define(BYTES_M, 1048576).
-define(BYTES_G, 1073741824).
-define(BYTES_T, 1099511627776).

-define(ANY_DIGIT, [<<"1">>, <<"2">>, <<"3">>
                   ,<<"4">>, <<"5">>, <<"6">>
                   ,<<"7">>, <<"8">>, <<"9">>
                   ,<<"*">>, <<"0">>, <<"#">>
                   ]).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-type text() :: kz_types:text().

-type atoms() :: kz_types:atoms().
-type pids() :: kz_types:pids().
-type references() :: kz_types:references().

-type kz_proplist_key() :: kz_types:kz_proplist_key().
-type kz_proplist_value() :: kz_types:kz_proplist_value().
-type kz_proplist_property() :: kz_types:kz_proplist_property().
-type kz_proplist() :: kz_types:kz_proplist().
-type kz_proplists() :: kz_types:kz_proplists().
-type kz_proplist_kv(K, V) :: kz_types:kz_proplist_kv(K, V).

-type pid_ref() :: kz_types:pid_ref().
-type pid_refs() :: kz_types:pid_refs().
-type api_pid_ref() :: kz_types:api_pid_ref().
-type api_pid_refs() :: kz_types:api_pid_refs().

-type api_terms() :: kz_types:api_terms().
-type api_binary() :: kz_types:api_binary().
-type api_ne_binary() :: kz_types:api_ne_binary().
-type api_ne_binaries() :: kz_types:api_ne_binaries().
-type api_binaries() :: kz_types:api_binaries().
-type api_object() :: kz_types:api_object().
-type api_objects() :: kz_types:api_objects().
-type api_boolean() :: kz_types:api_boolean().
-type api_atom() :: kz_types:api_atom().
-type api_atoms() :: kz_types:api_atoms().
-type api_string() :: kz_types:api_string().
-type api_reference() :: kz_types:api_reference().
-type api_pid() :: kz_types:api_pid().
-type api_list() :: kz_types:api_list().

-type api_number() :: kz_types:api_number().
-type api_integer() :: kz_types:api_integer().
-type api_pos_integer() :: kz_types:api_pos_integer().
-type api_non_neg_integer() :: kz_types:api_non_neg_integer().
-type api_float() :: kz_types:api_float().

-type kz_deeplist() :: kz_types:kz_deeplist().

-type kz_std_return() :: kz_types:kz_std_return().

-type kz_jobj_return() :: kz_types:kz_jobj_return().
-type kz_jobjs_return() :: kz_types:kz_jobjs_return().

%% non-empty binary
-define(NE_BINARY, <<_:8,_/binary>>).

-type ne_binary() :: kz_types:ne_binary().
-type ne_binaries() :: kz_types:ne_binaries().
-type binaries() :: kz_types:binaries().

-type strings() :: kz_types:strings().
-type integers() :: kz_types:integers().

-type functions() :: kz_types:functions().

%% when using gen_smtp to send emails, it takes a 5-tuple for a message-body part
-type mail_message_body() :: kz_types:mail_message_body().

%% for setting types on dicts
-type dict(K,V) :: kz_types:dic(K, V).

%% result of calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
%% Subtract this value from a Gregorian seconds version of a date
%% to get the Unix timestamp
%% datetime_to_gregorian_seconds({date(),time()}) - ?UNIX_EPOCH_IN_GREGORIAN.
-define(UNIX_EPOCH_IN_GREGORIAN, 62167219200).

-type kz_now() :: kz_types:kz_now().
-type kz_year() :: kz_types:kz_year().
-type kz_month() :: kz_types:kz_month().
-type kz_day() :: kz_types:kz_day().
-type kz_hour() :: kz_types:kz_hour().
-type kz_minute() :: kz_types:kz_minute().
-type kz_second() :: kz_types:kz_second().
-type kz_daynum() :: kz_types:kz_daynum().
-type kz_weeknum() :: kz_types:kz_weeknum().
-type kz_date() :: kz_types:kz_date().
-type kz_time() :: kz_types:kz_time().
-type kz_datetime() :: kz_types:kz_datetime().
-type kz_iso_week() :: kz_types:kz_iso_week().
-type gregorian_seconds() :: kz_types:gregorian_seconds().
-type unix_seconds() :: kz_types:unix_seconds().
-type api_seconds() :: kz_types:api_seconds().

-type kz_timeout() :: kz_types:kz_timeout().

-type kz_ip_list() :: kz_types:kz_ip_list().

%% Recreate the non-exported types defined in the Erlang supervisor source
-type sup_child_spec() :: kz_types:sup_child_spec().
-type sup_child_specs() :: kz_types:sup_child_specs().
-type sup_start_flags() :: kz_types:sup_flags().

-type sup_init_ret() :: kz_types:sup_init_ret().

-type sup_child_id() :: kz_types:sup_child_id().
-type sup_startchild_err() :: kz_types:sup_startchild_err().
-type sup_startchild_ret() :: kz_types:sup_startchild_ret().

%% Helper macro for declaring children of supervisor
-define(WORKER(I), {I, {I, 'start_link', []}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_ARGS(I, Args), {I, {I, 'start_link', Args}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_TYPE(I, Type), {I, {I, 'start_link', []}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_ARGS_TYPE(I, Args, Type), {I, {I, 'start_link', Args}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_NAME_ARGS(I, N, Args), {N, {I, 'start_link', Args}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_NAME_ARGS_TYPE(N, I, Args, Type), {N, {I, 'start_link', Args}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_APP_INIT(I, T), {I, {I, 'start_link', []}, 'temporary', T * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).

-define(SUPER(I), {I, {I, 'start_link', []}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_TYPE(I, Type), {I, {I, 'start_link', []}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_ARGS(I, Args), {I, {I, 'start_link', Args}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_ARGS_TYPE(I, Args, Type), {I, {I, 'start_link', Args}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_ARGS(I, N, Args), {N, {I, 'start_link', Args}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_ARGS_TYPE(N, I, Args, Type), {N, {I, 'start_link', Args}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_TYPE(N, I, Type), {N, {I, 'start_link', []}, Type, 'infinity', 'supervisor', [I]}).

-define(CACHE(N), {N, {'kz_cache', 'start_link', [N]}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', ['kz_cache']}).
-define(CACHE_ARGS(N, Arg), {N, {'kz_cache', 'start_link', [N, Arg]}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', ['kz_cache']}).

%% Recreate the non-exported types defined in the Erlang gen_server source
-type startlink_err() :: kz_types:startlink_err().
-type startlink_ret() :: kz_types:startlink_ret().
-type startapp_ret() :: kz_types:startapp_ret().

-type call_from() :: kz_types:call_from().
-type gen_server_timeout() :: kz_types:gen_server_timeout().
-type handle_call_ret() :: kz_types:handle_call_ret().

-type handle_call_ret_state(State) :: kz_types:handle_call_ret_state(State).

-type handle_cast_ret() :: kz_types:handle_cast_ret().
-type handle_cast_ret_state(State) :: kz_types:handle_cast_ret_state(State).

-type handle_info_ret() :: kz_types:handle_info_ret().
-type handle_info_ret_state(State) :: kz_types:handle_info_ret_state(State).

-type handle_fsm_ret(State) :: kz_types:handle_fsm_ret(State).

-type handle_sync_event_ret(State) :: kz_types:handle_sync_event_ret(State).

-type server_ref() :: kz_types:server_ref().

-type gen_server_name() :: kz_types:gen_server_name().
-type gen_server_option() :: kz_types:gen_server_option().
-type gen_server_options() :: kz_types:gen_server_options().

%% XML types
-type xml_attrib_name() :: kz_types:xml_attrib_name().
-type xml_attrib_value() :: kz_types:xml_attrib_value().
-type xml_attrib() :: kz_types:xml_attrib().
-type xml_attribs() :: kz_types:xml_attribs().

-type xml_el() :: kz_types:xml_el().
-type xml_els() :: kz_types:xml_els().

-type xml_text() :: kz_types:xml_text().
-type xml_texts() :: kz_types:xml_texts().

-type xml_thing() :: kz_types:xml_thing().
-type xml_things() :: kz_types:xml_things().

%% Used by ecallmgr and kapi_dialplan at least
-define(CALL_EVENTS,
        [<<"CALL_SECURE">>,<<"CALL_UPDATE">>
        ,<<"CHANNEL_ANSWER">>
        ,<<"CHANNEL_CREATE">>, <<"CHANNEL_DESTROY">>
        ,<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>,<<"CHANNEL_EXECUTE_ERROR">>
        ,<<"CHANNEL_FAX_STATUS">>,<<"CHANNEL_INTERCEPTED">>
        ,<<"CHANNEL_PROGRESS_MEDIA">>,<<"CHANNEL_REPLACED">>
        ,<<"CHANNEL_TRANSFEREE">>,<<"CHANNEL_TRANSFEROR">>
        ,<<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
        ,<<"CHANNEL_HOLD">>, <<"CHANNEL_UNHOLD">>
        ,<<"DETECTED_TONE">>,<<"DTMF">>
        ,<<"LEG_CREATED">>, <<"LEG_DESTROYED">>
        ,<<"RECORD_START">>,<<"RECORD_STOP">>
        ,<<"dialplan">> %% errors are sent with this
        ]).

-define(CHANNEL_LOOPBACK_HEADER_PREFIX, "Export-Loopback-").
-define(CALL_INTERACTION_ID, "Call-Interaction-ID").
-define(CALL_INTERACTION_DEFAULT
       ,<<(kz_term:to_binary(kz_time:now_s()))/binary
          ,"-", (kz_binary:rand_hex(4))/binary
        >>).

-define(BRIDGE_DEFAULT_SYSTEM_TIMEOUT_S, 20).


-define(MATCH_ACCOUNT_RAW(Account),
        <<(Account):32/binary>>
       ).
-define(MATCH_ACCOUNT_UNENCODED(Account),
        <<"account/", (Account):34/binary>>
       ).
-define(MATCH_ACCOUNT_ENCODED(Account),
        <<"account%2F", (Account):38/binary>>
       ).
-define(MATCH_ACCOUNT_encoded(Account),
        <<"account%2f", (Account):38/binary>>
       ).

-define(MATCH_ACCOUNT_RAW(A, B, Rest),
        <<(A):2/binary, (B):2/binary, (Rest)/binary>>  %% FIXME: add missing size (Rest:28)
       ).
-define(MATCH_ACCOUNT_UNENCODED(A, B, Rest),
        <<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary>>
       ).
-define(MATCH_ACCOUNT_ENCODED(A, B, Rest),
        <<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary>>
       ).
-define(MATCH_ACCOUNT_encoded(A, B, Rest),
        <<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary>>
       ).

-define(MATCH_PROVISIONER_RAW(Account),
        <<"account/", (Account):34/binary, "-provisioner">>
       ).
-define(MATCH_PROVISIONER_ENCODED(Account),
        <<"account%2F", (Account):38/binary, "-provisioner">>
       ).
-define(MATCH_PROVISIONER_encoded(Account),
        <<"account%2f", (Account):38/binary, "-provisioner">>
       ).

-define(MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month),
        <<(A):2/binary, (B):2/binary, (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month),
        <<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month),
        <<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_encoded(A, B, Rest, Year, Month),
        <<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).

%% FIXME: replace these with the above ones, actually matching: "account..."
%% FIXME: add MATCH_MODB_SUFFIX_encoded/3
-define(MATCH_MODB_SUFFIX_RAW(Account, Year, Month),
        <<(Account):32/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).
-define(MATCH_MODB_SUFFIX_UNENCODED(Account, Year, Month),
        <<(Account):42/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).
-define(MATCH_MODB_SUFFIX_ENCODED(Account, Year, Month),
        <<(Account):48/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).

-define(MATCH_MODB_PREFIX(Year, Month, Account),
        <<(Year):4/binary, (Month):2/binary, "-", (Account)/binary>>  %% FIXME: add missing size
       ).
-define(MATCH_MODB_PREFIX_M1(Year, Month, Account),
        <<(Year):4/binary, (Month):1/binary, "-", (Account)/binary>>  %% FIXME: add missing size
       ).

-define(MATCH_RESOURCE_SELECTORS_RAW(Account),
        <<(Account):32/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_UNENCODED(Account),
        <<"account/", (Account):34/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_ENCODED(Account),
        <<"account%2F", (Account):38/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_encoded(Account),
        <<"account%2f", (Account):38/binary, "-selectors">>
       ).

-define(MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest),
        <<(A):2/binary, (B):2/binary, (Rest):28/binary
          ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest),
        <<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary
          ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest),
        <<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary
          ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_encoded(A, B, Rest),
        <<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary
          ,"-selectors"
        >>
       ).

%% KZ_NODES types

-type whapp_info() :: kz_types:whapp_info().
-type kapps_info() :: kz_types:kapps_info().

-type media_server() :: kz_types:media_server().
-type media_servers() :: kz_types:media_servers().

-type kz_node() :: kz_types:kz_node().
-type kz_nodes() :: kz_types:kz_nodes().

-define(FAKE_CALLID(C), kz_term:to_hex_binary(crypto:hash(md5, C))).

-ifdef(TEST).

-define(LOG_ALERT(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_CRITICAL(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_DEBUG(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_EMERGENCY(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_ERROR(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_INFO(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_NOTICE(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(LOG_WARNING(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).

-define(LOG_ALERT(F), ?LOG_ALERT(F, [])).
-define(LOG_CRITICAL(F), ?LOG_CRITICAL(F, [])).
-define(LOG_DEBUG(F), ?LOG_DEBUG(F, [])).
-define(LOG_EMERGENCY(F), ?LOG_EMERGENCY(F, [])).
-define(LOG_ERROR(F), ?LOG_ERROR(F, [])).
-define(LOG_INFO(F), ?LOG_INFO(F, [])).
-define(LOG_NOTICE(F), ?LOG_NOTICE(F, [])).
-define(LOG_WARNING(F), ?LOG_WARNING(F, [])).

-else.

-define(LOG_ALERT(F, A), lager:alert(F, A)).
-define(LOG_CRITICAL(F, A), lager:critical(F, A)).
-define(LOG_DEBUG(F, A), lager:debug(F, A)).
-define(LOG_EMERGENCY(F, A), lager:emergency(F, A)).
-define(LOG_ERROR(F, A), lager:error(F, A)).
-define(LOG_INFO(F, A), lager:info(F, A)).
-define(LOG_NOTICE(F, A), lager:notice(F, A)).
-define(LOG_WARNING(F, A), lager:warning(F, A)).

-define(LOG_ALERT(F), ?LOG_ALERT(F, [])).
-define(LOG_CRITICAL(F), ?LOG_CRITICAL(F, [])).
-define(LOG_DEBUG(F), ?LOG_DEBUG(F, [])).
-define(LOG_EMERGENCY(F), ?LOG_EMERGENCY(F, [])).
-define(LOG_ERROR(F), ?LOG_ERROR(F, [])).
-define(LOG_INFO(F), ?LOG_INFO(F, [])).
-define(LOG_NOTICE(F), ?LOG_NOTICE(F, [])).
-define(LOG_WARNING(F), ?LOG_WARNING(F, [])).

-endif.

-define(SUP_LOG_DEBUG(F, A),
        begin
            lager:debug(F, A),
            io:format(F ++ "\n", A)
        end
       ).
-define(SUP_LOG_WARNING(F, A),
        begin
            lager:warning(F, A),
            io:format(F ++ "\n", A)
        end
       ).
-define(SUP_LOG_ERROR(F, A),
        begin
            lager:error(F, A),
            io:format(F ++ "\n", A)
        end
       ).

-define(DEV_LOG(F, A), io:format(user, "~s:~p  " ++ F ++ "\n", [?MODULE, ?LINE | A])).
-define(DEV_LOG(F), ?DEV_LOG(F, [])).

-define(SUP_LOG_DEBUG(F), ?SUP_LOG_DEBUG(F, [])).
-define(SUP_LOG_WARNING(F), ?SUP_LOG_WARNING(F, [])).
-define(SUP_LOG_ERROR(F), ?SUP_LOG_ERROR(F, [])).

-define(KAZOO_TYPES_INCLUDED, 'true').
-endif.

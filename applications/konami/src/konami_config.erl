%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% Handles Konami code configs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_config).

-export([numbers/0, numbers/1
        ,patterns/0, patterns/1
        ,binding_digit/0, binding_digit/1
        ,timeout/0, timeout/1
        ,listen_on/0, listen_on/1
        ]).

-include("konami.hrl").

-define(DEFAULT_BINDING_DIGIT, <<"*">>).
-define(DEFAULT_DIGIT_TIMEOUT, 800).

-define(META_SAY_HI, kz_json:from_list([{<<"module">>, <<"say">>}
                                       ,{<<"data">>, kz_json:from_list([{<<"text">>, <<"hi">>}])}
                                       ])).

-define(DEFAULT_NUMBERS, kz_json:from_list([{<<"2">>, ?META_SAY_HI}
                                           ])).
-define(DEFAULT_PATTERNS, kz_json:from_list([{<<"^2$">>, ?META_SAY_HI}
                                            ])).

-define(DEFAULT_LISTEN_ON, 'a').

-type default_fun() :: fun(() -> any()).
-type formatter_fun() :: fun((any()) -> any()).

-spec numbers() -> kz_json:object().
-spec numbers(ne_binary()) -> kz_json:object().
numbers() ->
    kapps_config:get_json(<<"metaflows">>, <<"numbers">>, ?DEFAULT_NUMBERS).

numbers(Account) ->
    case konami_doc(Account) of
        'undefined' -> numbers();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"numbers">>, fun numbers/0)
    end.

-spec patterns() -> kz_json:object().
-spec patterns(ne_binary()) -> kz_json:object().
patterns() ->
    kapps_config:get_json(<<"metaflows">>, <<"patterns">>, ?DEFAULT_PATTERNS).

patterns(Account) ->
    case konami_doc(Account) of
        'undefined' -> patterns();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"patterns">>, fun patterns/0)
    end.

-spec binding_digit() -> <<_:8>>.
-spec binding_digit(ne_binary()) -> <<_:8>>.
binding_digit() ->
    BindingDigit = kapps_config:get_ne_binary(<<"metaflows">>, <<"binding_digit">>, ?DEFAULT_BINDING_DIGIT),
    constrain_binding_digit(BindingDigit).

binding_digit(Account) ->
    case konami_doc(Account) of
        'undefined' -> binding_digit();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"binding_digit">>, fun binding_digit/0, fun constrain_binding_digit/1)
    end.

-spec constrain_binding_digit(ne_binary()) -> <<_:8>>.
constrain_binding_digit(BindingDigit) ->
    case lists:member(BindingDigit, ?ANY_DIGIT) of
        'true' -> BindingDigit;
        'false' -> ?DEFAULT_BINDING_DIGIT
    end.

-spec timeout() -> non_neg_integer().
-spec timeout(ne_binary()) -> non_neg_integer().
timeout() ->
    kapps_config:get_integer(<<"metaflows">>, <<"digit_timeout_ms">>, ?DEFAULT_DIGIT_TIMEOUT).

timeout(Account) ->
    case konami_doc(Account) of
        'undefined' -> timeout();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"digit_timeout_ms">>, fun timeout/0, fun kz_term:to_integer/1)
    end.

-spec listen_on() -> 'a' | 'b' | 'ab'.
-spec listen_on(ne_binary()) -> 'a' | 'b' | 'ab'.
listen_on() ->
    constrain_listen_on(kapps_config:get_ne_binary(<<"metaflows">>, <<"listen_on">>, ?DEFAULT_LISTEN_ON)).
listen_on(Account) ->
    case konami_doc(Account) of
        'undefined' -> listen_on();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"listen_on">>, fun listen_on/0, fun constrain_listen_on/1)
    end.

-spec constrain_listen_on(api_binary()) -> 'a' | 'b' | 'ab'.
constrain_listen_on(<<"a">>) -> 'a';
constrain_listen_on(<<"b">>) -> 'b';
constrain_listen_on(<<"ab">>) -> 'ab';
constrain_listen_on(<<"both">>) -> 'ab';
constrain_listen_on(_) -> ?DEFAULT_LISTEN_ON.

-spec identity(X) -> X.
identity(X) -> X.

-spec get_attribute(kz_json:object(), ne_binary(), default_fun()) -> any().
-spec get_attribute(kz_json:object(), ne_binary(), default_fun(), formatter_fun()) -> any().
get_attribute(JObj, K, DefaultFun) ->
    get_attribute(JObj, K, DefaultFun, fun identity/1).

get_attribute(JObj, K, DefaultFun, FormatterFun) ->
    case kz_json:get_value(K, JObj) of
        'undefined' -> DefaultFun();
        V -> FormatterFun(V)
    end.

-spec konami_doc(ne_binary()) -> api_object().
konami_doc(Account) ->
    case kz_account:fetch(Account) of
        {'ok', JObj} -> kz_json:get_value(<<"metaflows">>, JObj);
        {'error', 'not_found'} -> 'undefined';
        {'error', _E} ->
            AccountId = kz_util:format_account_id(Account, 'raw'),
            lager:debug("failed to open account(~s)'s konami doc: ~p", [AccountId, _E]),
            'undefined'
    end.

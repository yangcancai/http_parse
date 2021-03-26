-module(http_parse).

-define(MAX_INT, trunc(math:pow(2, 31) - 1)).
-define(MIN_INT, math:pow(2, 31) * -1).
-define(MAX_BIG_INT, trunc(math:pow(2, 63) - 1)).
-define(MIN_BIG_INT, trunc(math:pow(2, 63) * -1)).
-define(MAX_POS_BIG_INT, trunc(math:pow(2, 63) - 1)).
-define(MIN_FLOAT, -3.4 * math:pow(10, 38)).
-define(MAX_FLOAT, 3.4 * math:pow(10, 38)).

-include("http_parse.hrl").

%% API
-export([check_para/2, body/1, qs/1, bindings/1, headers/1, to_binary/1, to_list/1,
         combine_binary/2, to_atom/1]).

-type headers() :: body().
-type bindings() :: body().
-type key_value() :: {binary() | atom(), any()}.
-type key_value_list() :: [key_value()].
-type body() :: [key_value()] | [key_value_list()] | [#{binary() | atom() => any()}].
-type qs() :: body().
-type data_format() ::
    #{body => body(),
      qs => qs(),
      headers => headers(),
      bindings => bindings()}.
-type min() :: integer() | float().
-type max() :: integer() | float().
-type required() ::
    pos_integer |
    neg_integer |
    integer |
    bigint |
    {number, min(), max()} |
    {string, pos_integer()} |
    required |
    float |
    {float, min(), max()} |
    {binary, pos_integer()} |
    optional.
-type requireds() :: [required()].
-type para() :: {atom(), requireds()}.
-type paras() :: [para()].
-type opt() :: list | tuple | paras().
-type opts() :: [opt()].
-type cond_format() ::
    #{body => opts(),
      qs => opts(),
      headers => opts(),
      bindings => opts()}.

%% @doc why format is
%% Error agent_id, The body/headers/qs/bindings param agent_id is -100 must be pos_integer, type is
%% required
%% Error order_id, The body/headers/qs/bindings param order_id is undefined must be string(5), type
%% is required
%% Error comment, The body/headers/qs/bindings param comment is abcdef1234 must be string(5), type
%% is optional
-spec check_para(data_format(), cond_format()) ->
                    {ok, data_format()} | {error, Why :: binary()}.
check_para(Data, Cond) ->
    try
        check_param(Data, Cond)
    catch
        _E:ErrorMsg:_Stack ->
            ErrorMsg
    end.

%%      erlang:get_stacktrace()

body(#{body := Body}) ->
    Body.

qs(#{qs := Qs}) ->
    Qs.

headers(#{headers := Headers}) ->
    Headers.

bindings(#{bindings := Bindings}) ->
    Bindings.

change_cond_form([list, {para, CondLs}]) ->
    {list, CondLs};
change_cond_form([tuple, {para, CondLs}]) ->
    {tuple, CondLs};
change_cond_form([list, CondLs]) ->
    {list, CondLs};
change_cond_form([tuple, CondLs]) ->
    {tuple, CondLs};
change_cond_form([{para, CondLs}]) ->
    {para, CondLs};
change_cond_form(CondLs) ->
    {para, CondLs}.

check_param(Data, Cond) ->
    NewData =
        maps:fold(fun(Key, V, InAcc) ->
                     {ParaType, AlterCond} = change_cond_form(V),
                     case mapsfind(Key, Data) of
                         {_, undefined} -> throw({error, ?BODY_DRR(Key)});
                         {NewKey, KeyVal} ->
                             Value = check_param_(ParaType, AlterCond, KeyVal, Key),
                             maps:put(NewKey, Value, InAcc)
                     end
                  end,
                  Data,
                  Cond),
    {ok, NewData}.

check_param_(list, Cond, CheckData, Key) ->
    Data =
        lists:foldl(fun(CheckTerm, Acc) ->
                       Value = check_param_(Cond, CheckTerm, Key),
                       [Value | Acc]
                    end,
                    [],
                    CheckData),
    lists:reverse(Data);
check_param_(_, Cond, CheckData, Key) ->
    check_param_(Cond, CheckData, Key).

check_param_(Cond, CheckData, Key) when is_map(CheckData) ->
    lists:foldl(fun({CheckKey, OptLs}, InAcc) ->
                   {HaveKey, Value} = mapsfind(CheckKey, CheckData),
                   IsKeyExist = maps:is_key(HaveKey, CheckData),
                   case catch get_new_param(IsKeyExist, Value, OptLs) of
                       {ok, NewValue} -> format_data(IsKeyExist, HaveKey, NewValue, InAcc);
                       {error, ErrorMsg} ->
                           ErrorHead = ?HEAD_ERR(CheckKey, Key),
                           throw({error, <<ErrorHead/binary, ErrorMsg/binary>>})
                   end
                end,
                CheckData,
                Cond);
check_param_(Cond, CheckData, Key) when is_list(CheckData) ->
    lists:foldl(fun({CheckKey, OptLs}, InAcc) ->
                   {HaveKey, Value} = keyfind(CheckKey, CheckData),
                   IsKeyExist = lists:keymember(HaveKey, 1, CheckData),
                   case catch get_new_param(IsKeyExist, Value, OptLs) of
                       {ok, NewValue} -> format_data(IsKeyExist, HaveKey, NewValue, InAcc);
                       {error, ErrorMsg} ->
                           ErrorHead = ?HEAD_ERR(CheckKey, Key),
                           throw({error, <<ErrorHead/binary, ErrorMsg/binary>>})
                   end
                end,
                CheckData,
                Cond).

mapsfind(Key, CheckData) ->
    case maps:find(Key, CheckData) of
        {ok, Val} ->
            {Key, Val};
        error ->
            BinKey = to_binary(Key),
            case maps:find(BinKey, CheckData) of
                {ok, Val} ->
                    {BinKey, Val};
                error ->
                    {BinKey, undefined}
            end
    end.

keyfind(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            {Key, Value};
        _ ->
            BinKey = to_binary(Key),
            case lists:keyfind(BinKey, 1, List) of
                {BinKey, Value} ->
                    {BinKey, Value};
                _ ->
                    {BinKey, undefined}
            end
    end.

%% @doc IsMemeberkey,Value, Conditions
get_new_param(false, undefined, [_, optional]) ->
    {ok, undefined};
get_new_param(false, undefined, [_, optional, _Regex]) ->
    {ok, undefined};
get_new_param(false, undefined, [Strain, required]) ->
    throw({error, ?TYPE_ERR(undefined, show_strain(Strain), required)});
get_new_param(false, undefined, [Strain, required, _Regex]) ->
    throw({error, ?TYPE_ERR(undefined, show_strain(Strain), required)});
get_new_param(true, <<"">>, [_Strain, optional]) ->
    {ok, <<"">>};
get_new_param(true, <<"">>, [_Strain, optional, _Regex]) ->
    {ok, <<"">>};
get_new_param(true, NeedCheckVal, [Strain, OptType]) ->
    get_new_param_(NeedCheckVal, Strain, OptType);
get_new_param(true, NeedCheckVal, [Strain, OptType, Regex]) ->
    get_new_param_(NeedCheckVal, Strain, OptType, Regex).

get_new_param_(NeedCheckVal, Strain, OptType, Regex) ->
    case filter(to_list(NeedCheckVal), Regex) of
        [] ->
            throw({error, ?INVALIDE_ERR(NeedCheckVal, show_strain(Strain), OptType)});
        NewCheckVal ->
            get_new_param_(NewCheckVal, Strain, OptType)
    end.

%% 所有非法的参数都返回[]
filter(V, Regex) ->
    case re:run(V, Regex, [{capture, all, list}, global]) of
        {match, L} ->
            to_binary(lists:flatten(L));
        _ ->
            []
    end.

show_strain({Type, N}) ->
    lists:concat(["{", Type, ",", N, "}"]);
show_strain({Type, Min, Max}) ->
    lists:concat(["{", Type, ",", Min, ",", Max, "}"]);
show_strain(Type) ->
    Type.

%% @doc 整型,浮点
get_new_param_(NeedCheckVal, float, OptType) ->
    case catch to_float(NeedCheckVal) of
        Val when is_float(Val) ->
            case ?MAX_FLOAT >= Val andalso ?MIN_FLOAT =< Val of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?SIZE_ERR(Val, ?MIN_FLOAT, ?MAX_FLOAT, float, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, float, OptType),
            {error, ErrMSG}
    end;
%% @doc binary
get_new_param_(NeedCheckVal, binary, OptType) ->
    case catch to_binary(NeedCheckVal) of
        Val when is_binary(Val) ->
            {ok, Val};
        _ ->
            ErrMGS = ?TYPE_ERR(NeedCheckVal, binary, OptType),
            {error, ErrMGS}
    end;
%% @doc 检查 integer/pos_integer/non_neg_integer/bigint/pos_bigint
get_new_param_(NeedCheckVal, IntType, OptType)
    when IntType == integer;
         IntType == pos_integer;
         IntType == non_neg_integer;
         IntType == neg_integer;
         IntType == bigint;
         IntType == pos_bigint ->
    case catch to_integer(NeedCheckVal) of
        Val when is_integer(Val) ->
            {Min, Max} = get_map_int_limit(IntType),
            case Max >= Val andalso Min =< Val of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?SIZE_ERR(Val, Min, Max, IntType, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, IntType, OptType),
            {error, ErrMSG}
    end;
%% @doc 检查string
get_new_param_(NeedCheckVal, {string, Len}, OptType) ->
    case catch to_list(NeedCheckVal) of
        Val when is_list(Val) ->
            case erlang:length(Val) =< Len andalso length(Val) > 0 of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?LENGTH_ERR(Val, Len, string, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, string, OptType),
            {error, ErrMSG}
    end;
%% @doc 检查number
get_new_param_(NeedCheckVal, {number, Min, Max}, OptType) ->
    case catch to_number(NeedCheckVal) of
        Val when is_number(Val) ->
            case Val >= Min andalso Val =< Max of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?SIZE_ERR(Val, Min, Max, number, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, number, OptType),
            {error, ErrMSG}
    end;
%% @doc 检查number
get_new_param_(NeedCheckVal, {number, Min, Max, NotEqual}, OptType) ->
    case catch to_number(NeedCheckVal) of
        Val when is_number(Val) ->
            case Val >= Min andalso Val =< Max andalso Val /= NotEqual of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?SIZE_ERR(Val, Min, Max, NotEqual, number, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, number, OptType),
            {error, ErrMSG}
    end;
%% @doc 检查float
get_new_param_(NeedCheckVal, {float, Min, Max}, OptType) ->
    case catch to_float(NeedCheckVal) of
        Val when is_float(Val) ->
            case Val >= Min andalso Val =< Max of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?SIZE_ERR(Val, Min, Max, float, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, float, OptType),
            {error, ErrMSG}
    end;
%% @doc 检查binary
get_new_param_(NeedCheckVal, {binary, Length}, OptType) ->
    case catch to_binary(NeedCheckVal) of
        Val when is_binary(Val) ->
            case erlang:size(Val) > 0 andalso erlang:size(Val) =< Length of
                true ->
                    {ok, Val};
                _ ->
                    ErrMSG = ?LENGTH_ERR(to_list(Val), Length, binary, OptType),
                    {error, ErrMSG}
            end;
        _ ->
            ErrMSG = ?TYPE_ERR(NeedCheckVal, binary, OptType),
            {error, ErrMSG}
    end;
get_new_param_(NeedCheckVal, Type, _OptType) ->
    case get(Type) of
        undefined ->
            {error, <<"unknown type ", (to_binary(Type))/binary>>};
        Cond ->
            {ok, New} = check_param(#{Type => NeedCheckVal}, Cond),
            {ok, maps:get(Type, New)}
    end.

get_map_int_limit(integer) ->
    {?MIN_INT, ?MAX_INT};
get_map_int_limit(bigint) ->
    {?MIN_BIG_INT, ?MAX_BIG_INT};
get_map_int_limit(pos_bigint) ->
    {1, ?MAX_BIG_INT};
get_map_int_limit(pos_integer) ->
    {1, ?MAX_INT};
get_map_int_limit(neg_integer) ->
    {?MIN_INT, -1};
get_map_int_limit(non_neg_integer) ->
    {0, ?MAX_INT}.

format_data(false, _Key, _Value, InAcc) ->
    InAcc;
format_data(_, Key, Value, InAcc) when is_list(InAcc) ->
    lists:keystore(Key, 1, InAcc, {Key, Value});
format_data(_, Key, Value, InAcc) ->
    maps:put(Key, Value, InAcc).

to_binary(V) when is_integer(V) ->
    erlang:integer_to_binary(V);
to_binary(V) when is_list(V) ->
    erlang:list_to_binary(V);
to_binary(V) when is_atom(V) ->
    erlang:atom_to_binary(V, utf8);
to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_float(V) ->
    to_binary(V, 4).

to_binary(V, N) when is_float(V), is_integer(N) ->
    erlang:float_to_binary(V, [{decimals, N}]).

to_list(V) when is_binary(V) ->
    erlang:binary_to_list(V);
to_list(V) when is_atom(V) ->
    erlang:atom_to_list(V);
to_list(V) when is_integer(V) ->
    erlang:integer_to_list(V);
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_float(V) ->
    to_list(V, 4).

to_list(V, N) when is_float(V) ->
    erlang:float_to_list(V, [{decimals, N}]).

to_integer(V) when is_list(V) ->
    erlang:list_to_integer(V);
to_integer(V) when is_binary(V) ->
    erlang:binary_to_integer(V);
to_integer(V) when is_integer(V) ->
    V.

to_float(V) when is_float(V) ->
    erlang:binary_to_float(to_binary(erlang:trunc(V) * 1.0));
to_float(V) when is_integer(V) ->
    erlang:binary_to_float(to_binary(V * 1.0));
to_float(V) when is_list(V) ->
    case catch erlang:list_to_integer(V) of
        Int when is_integer(Int) ->
            Int * 1.0;
        _ ->
            case catch binary_to_float(erlang:list_to_binary(V)) of
                Float when is_float(Float) ->
                    erlang:trunc(Float) * 1.0;
                _ ->
                    throw(error)
            end
    end;
to_float(V) when is_binary(V) ->
    to_float(binary_to_list(V)).

to_number(V) when is_number(V) ->
    V;
to_number(V) when is_float(V) ->
    V;
to_number(V) when is_integer(V) ->
    V;
to_number(V) when is_list(V) ->
    case catch erlang:list_to_integer(V) of
        Int when is_integer(Int) ->
            Int;
        _ ->
            case catch erlang:list_to_float(V) of
                Float when is_float(Float) ->
                    Float;
                _ ->
                    throw(error)
            end
    end;
to_number(V) when is_binary(V) ->
    to_number(binary_to_list(V)).

to_atom(V) when is_atom(V) ->
    V;
to_atom(V) when is_integer(V) ->
    to_atom(erlang:integer_to_binary(V));
to_atom(V) when is_list(V) ->
    erlang:list_to_atom(V);
to_atom(V) when is_binary(V) ->
    erlang:binary_to_atom(V, utf8);
to_atom(V) when is_float(V) ->
    to_atom(V, 4).

to_atom(V, N) when is_float(V) ->
    to_atom(erlang:float_to_binary(V, [{decimals, N}])).

combine_binary(A, B) ->
    <<A/binary, B/binary>>.

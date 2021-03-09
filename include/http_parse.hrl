%% @doc 值，类型integer/float/number/list，条件:optional/required
-define(TYPE_ERR(Val, Type, Opt),
        http_parse:to_binary(
            lists:concat([http_parse:to_list(Val), " must be ", Type, ", type is ", Opt]))).
-define(LENGTH_ERR(Val, Len, Type, Opt),
        http_parse:to_binary(
            lists:concat([http_parse:to_list(Val),
                          " out of range: (",
                          Len,
                          ")",
                          " must be ",
                          Type,
                          ", type is ",
                          Opt]))).
-define(SIZE_ERR(Val, Min, Max, Type, Opt),
        http_parse:to_binary(
            lists:concat([http_parse:to_list(Val),
                          " out of range: (",
                          Min,
                          ", ",
                          Max,
                          ")",
                          " must be ",
                          Type,
                          ", type is ",
                          Opt]))).
-define(SIZE_ERR(Val, Min, Max, NotEqual, Type, Opt),
        http_parse:to_binary(
            lists:concat([http_parse:to_list(Val),
                          " out of range: (",
                          Min,
                          ", ",
                          Max,
                          ")",
                          " can not equal ",
                          NotEqual,
                          " must be ",
                          Type,
                          ", type is ",
                          Opt]))).
-define(HEAD_ERR(Key, Body),
        http_parse:to_binary(
            lists:concat(["Error ",
                          http_parse:to_list(Key),
                          ", The ",
                          Body,
                          " param ",
                          http_parse:to_list(Key),
                          " is "]))).
-define(BODY_DRR(Body),
        http_parse:to_binary(
            lists:concat(["Error", ",", "no ", Body]))).
-define(COMBINE_ERR(BIN1, BIN2), http_parse:combine_binary(BIN1, BIN2)).
-define(INVALIDE_ERR(Val, Type, Opt),
        http_parse:to_binary(
            lists:concat([http_parse:to_list(Val),
                          " is invalid, must be ",
                          Type,
                          ", type is ",
                          Opt]))).

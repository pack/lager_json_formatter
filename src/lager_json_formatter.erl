-module(lager_json_formatter).

-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([format/2, format/3]).

-spec format(lager_msg:lager_msg(), list()) -> iolist().
format(Msg, Config, _Colors) -> format(Msg, Config).
format(Msg, Config) ->
  Message = lager_msg:message(Msg),
  handle_format(Msg, Config, Message, is_binary(Message)).

%% Internal
handle_format(Msg, Config, Message, true) ->
  handle_binary(Msg, Config, Message, jsx:is_json(Message));
handle_format(Msg, Config, Message, false) ->
  handle_list(Msg, Config, Message, jsx:is_json(Message)).

%% Binary
handle_binary(Msg, Config, Message, true) ->
  handle_json(Msg, Config, Message);
handle_binary(Msg, Config, Message, false) ->
  handle_raw(Msg, Config, Message).

%% List
handle_list(Msg, Config, Message, true) ->
  handle_json(Msg, Config, Message);
handle_list(Msg, Config, Message, false) ->
  [First|Tail] = Message,
  case is_number(First) of
    true  -> handle_raw(Msg, Config, list_to_binary(Message));
    false -> handle_metadata(Msg, Config, prepare_json_message(Message))
  end.

prepare_json_message([ {K, [V]} | Tail ]) ->
  [{prepare_json_item(K), prepare_json_message(V)}] ++
    prepare_json_message(Tail);
prepare_json_message([ {K, V} | Tail ]) ->
  [{prepare_json_item(K), prepare_json_item(V)}] ++
    prepare_json_message(Tail);
prepare_json_message([]) -> [].

prepare_json_item(Item) when is_binary(Item), is_number(Item) -> Item;
prepare_json_item(Item) when is_atom(Item) -> atom_to_binary(Item, utf8);
prepare_json_item(Item) when is_list(Item) -> list_to_binary(Item);
prepare_json_item(Item) -> Item.

%% Raw
handle_raw(Msg, _Config, Message) ->
  Severity  = lager_msg:severity(Msg),
  Timestamp = lager_msg:timestamp(Msg),
  IsoTS     = iso8601:format(Timestamp),
  jsx:encode([{s, Severity}, {t, IsoTS}, {m, Message}]).

%% Metadata
handle_metadata(Msg, _Config, Message) ->
  Severity = proplists:get_value(
               <<"s">>, Message, lager_msg:severity(Msg)),
  IsoTS    = proplists:get_value(
               <<"t">>, Message, iso8601:format(lager_msg:timestamp(Msg))),
  JMessage = proplists:get_value(
               <<"m">>, Message, Message),
  MergeMeta = [{<<"s">>, Severity}, {<<"t">>, IsoTS}, {<<"m">>, JMessage}],
  M1 = lists:keydelete(<<"s">>, 1, Message),
  M2 = lists:keydelete(<<"t">>, 1, M1),
  M3 = lists:keydelete(<<"m">>, 1, M2),
  jsx:encode(MergeMeta ++ M3).

%% Json
handle_json(Msg, Config, Message) ->
  handle_metadata(Msg, Config, jsx:decode(Message)).

-ifdef(TEST).

-define(setup(F), {setup, fun setup/0, fun cleanup/1, F}).
setup() -> [].
cleanup(Config) -> Config.

format_test_() ->
  [{"Format a standard string.",
    ?setup(fun format_standard_string/0)},
   {"Format a standard binary.",
    ?setup(fun format_standard_binary/0)},
   {"Format a message with metadata.",
    ?setup(fun format_metadata_message/0)},
   {"Format a message built from json.",
    ?setup(fun format_json_message/0)}
  ].

format_standard_string() ->
  Msg = lager_msg:new("test", info, [], []),
  IsoTS = iso8601:format(lager_msg:timestamp(Msg)),
  MStr = "{\"s\":\"info\",\"t\":\"~s\",\"m\":\"~s\"}",
  TestStr = lists:flatten(io_lib:format(MStr, [IsoTS, "test"])),
  ?assertEqual(list_to_binary(TestStr), format(Msg, [])).

format_standard_binary() ->
  Msg = lager_msg:new(<<"test">>, info, [], []),
  IsoTS = iso8601:format(lager_msg:timestamp(Msg)),
  MStr = "{\"s\":\"info\",\"t\":\"~s\",\"m\":\"~s\"}",
  TestStr = lists:flatten(io_lib:format(MStr, [IsoTS, "test"])),
  ?assertEqual(list_to_binary(TestStr), format(Msg, [])).

format_metadata_message() ->
  Msg = lager_msg:new([{m, "awesome"}, {age, 25}], info, [], []),
  IsoTS = iso8601:format(lager_msg:timestamp(Msg)),
  MStr = "{\"s\":\"info\",\"t\":\"~s\",\"m\":\"~s\",\"age\":~w}",
  TestStr = lists:flatten(io_lib:format(MStr, [IsoTS, "awesome", 25])),
  ?assertEqual(list_to_binary(TestStr), format(Msg, [])).

format_json_message() ->
  Msg = lager_msg:new(<<"{\"name\":\"bob\",\"m\":\"mission success\"}">>, info, [], []),
  IsoTS = iso8601:format(lager_msg:timestamp(Msg)),
  MStr = "{\"s\":\"info\",\"t\":\"~s\",\"m\":\"~s\",\"name\":\"~s\"}",
  TestStr = lists:flatten(io_lib:format(MStr, [IsoTS, "mission success", "bob"])),
  ?assertEqual(list_to_binary(TestStr), format(Msg, [])).

-endif.

-module(lager_json_formatter).

-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([format/2]).

-spec format(lager_msg:lager_msg(), list()) -> iolist().
format(Msg, Config) ->
  Message = lager_msg:message(Msg),
  handle_format(Msg, Config, Message).

handle_format(_Msg, _Config, _Message) ->
  "ok".

-ifdef(TEST).

-define(setup(F), {setup, fun setup/0, fun cleanup/1, F}).

setup() -> [].
cleanup(Config) -> Config.

format_test_() ->
  [{"Format a standard message.",
    ?setup(fun format_standard_message/0)},
   {"Format a message with metadata.",
    ?setup(fun format_metadata_message/0)}
  ].

format_standard_message() ->
  ?_assertEqual(true, true).

format_metadata_message() ->
  ?_assertEqual(true, true).

-endif.

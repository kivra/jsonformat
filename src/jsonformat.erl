%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2020, Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Custom formatter for the Erlang OTP logger application which
%%%      outputs single-line JSON formatted data
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(jsonformat).

%%%_* Exports ==========================================================
-export([format/2]).

%%%_* Macros ===========================================================

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, Config) when is_map(Msg) ->
  encode(Msg, maps:put(level, Level, Meta), Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
  format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
  format(Map#{msg := {report, #{text => unicode:characters_to_binary(String)}}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
  format(Map#{msg := {string, io_lib:format(Format, Terms)}}, Config).

%%%_* Private functions ================================================
encode(Msg, Meta, Config) ->
  jsx:encode(maps:merge(Msg, Meta), maps:to_list(Config)).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hash_32_test() ->
  ?assertEqual( <<"{\"level\":\"alert\",\"text\":\"derp\"}">>
              , format(#{level => alert, msg => {string, "derp"}, meta => #{}}, #{}) ),
  ?assertEqual( <<"{\"herp\":\"derp\",\"level\":\"alert\"}">>
              , format(#{level => alert, msg => {report, #{herp => derp}}, meta => #{}}, #{}) ).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:

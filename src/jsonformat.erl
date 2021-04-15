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

%%%_* Types ============================================================
-type config() :: #{ new_line => boolean()
                   , text_output_key => atom()
                   , format_funs => #{atom() => fun((_) -> _)}}.

-export_type([config/0]).

%%%_* Macros ===========================================================
%%%_* Options ----------------------------------------------------------
-define(NEW_LINE, false).
-define(TEXT_OUTPUT_KEY, text).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec format(logger:log_event(), config()) -> unicode:chardata().
format(#{msg:={report, #{format:=Format, args:=Args, label:={error_logger, _}}}} = Map, Config) ->
  Report = #{text_output_key(Config) => io_lib:format(Format, Args)},
  format(Map#{msg := {report, Report}}, Config);
format(#{level:=Level, msg:={report, Msg}, meta:=Meta0}, Config) when is_map(Msg) ->
  Meta1 = maps:put(level, Level, Meta0),
  Data0 = maps:merge(Msg, Meta1),
  Data1 = format_data(Data0, Config),
  encode(pre_encode(Data1, Config), Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
  format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
  Report = #{text_output_key(Config) => unicode:characters_to_binary(String)},
  format(Map#{msg := {report, Report}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
  format(Map#{msg := {string, io_lib:format(Format, Terms)}}, Config).

%%%_* Private functions ================================================
pre_encode(Data, Config) ->
  maps:fold(
    fun(K, V, Acc) when is_map(V) ->
      maps:put(jsonify(K), pre_encode(V, Config), Acc);
       (K, V, Acc) ->
      maps:put(jsonify(K), jsonify(V), Acc)
    end,
    maps:new(),
    Data).

encode(Data, Config) ->
  Json = jsx:encode(Data),
  case new_line(Config) of
    true -> [Json, <<"\n">>];
    false -> Json
  end.

jsonify(A) when is_atom(A)     -> A;
jsonify(B) when is_binary(B)   -> B;
jsonify(I) when is_integer(I)  -> I;
jsonify(F) when is_float(F)    -> F;
jsonify(B) when is_boolean(B)  -> B;
jsonify(P) when is_pid(P)      -> jsonify(pid_to_list(P));
jsonify(P) when is_port(P)     -> jsonify(port_to_list(P));
jsonify(F) when is_function(F) -> jsonify(erlang:fun_to_list(F));
jsonify(L) when is_list(L)     ->
  try list_to_binary(L) of
    S -> S
  catch error:badarg ->
    unicode:characters_to_binary(io_lib:format("~w", [L]))
  end;
jsonify({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
  <<(a2b(M))/binary,$:,(a2b(F))/binary,$/,(integer_to_binary(A))/binary>>;
jsonify(Any)                   -> unicode:characters_to_binary(io_lib:format("~w", [Any])).

a2b(A) -> atom_to_binary(A, utf8).

format_data(Data, #{format_funs := Callbacks}) ->
  maps:fold(fun(K, Fun, Acc) when is_map_key(K, Data) -> maps:update_with(K, Fun, Acc);
               (_, _, Acc)                            -> Acc
            end, Data, Callbacks);
format_data(Data, _) ->
  Data.

new_line(Config) -> maps:get(new_line, Config, ?NEW_LINE).

text_output_key(Config) -> maps:get(text_output_key, Config, ?TEXT_OUTPUT_KEY).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_test() ->
  ?assertEqual( <<"{\"level\":\"alert\",\"text\":\"derp\"}">>
              , format(#{level => alert, msg => {string, "derp"}, meta => #{}}, #{}) ),
  ?assertEqual( <<"{\"herp\":\"derp\",\"level\":\"alert\"}">>
              , format(#{level => alert, msg => {report, #{herp => derp}}, meta => #{}}, #{}) ),
  ?assertEqual( <<"{\"level\":\"alert\",\"message\":\"derp\"}">>
              , format(#{level => alert, msg => {string, "derp"}, meta => #{}}, #{text_output_key => message}) ).

format_funs_test() ->
  Config1 = #{format_funs => #{ time  => fun(Epoch) -> Epoch + 1 end
                              , level => fun(alert) -> info      end}},
  ?assertEqual( <<"{\"level\":\"info\",\"text\":\"derp\",\"time\":2}">>
              , format(#{level => alert, msg => {string, "derp"}, meta => #{time => 1}}, Config1)).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:

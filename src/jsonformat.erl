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

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, Config) when is_map(Msg) ->
  encode(pre_encode(maps:merge(Msg, maps:put(level, Level, Meta)), Config));
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
  format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
  format(Map#{msg := {report, #{text => unicode:characters_to_binary(String)}}}, Config);
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

encode(Data) ->
  jsx:encode(Data).

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
    term_to_binary(L)
  end;
jsonify({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
  <<(a2b(M))/binary,$:,(a2b(F))/binary,$/,(integer_to_binary(A))/binary>>;
jsonify(Any)                   -> term_to_binary(Any).

a2b(A) -> atom_to_binary(A, utf8).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_test() ->
  ?assertEqual( <<"{\"level\":\"alert\",\"text\":\"derp\"}">>
              , format(#{ level => info
                        , msg   => {report,#{label => {supervisor,prog}}
                        , meta => #{}
                        }, #{}) ),
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

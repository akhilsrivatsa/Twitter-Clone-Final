%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <UF>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2022 8:17 PM
%%%-------------------------------------------------------------------
-module(client).
-author("akhil").
-export([start/0, init_all_services/0]).

%% API

start() ->
  PID = spawn(?MODULE, init_all_services, []),
  register(init_services, PID),
  do_nothing.

init_all_services() ->
  receive
    {tcp,Socket, Result} ->

      %io:format("Printing Timeline ~n"),
      Socket,
      Msg = lists:droplast(string:split(Result, "//", all)),
      lists:foreach(fun(Ind) ->
        Ind
      %io:format("~p ~n", [Ind])
                    end, Msg),

      init_all_services();
    {"register_user", Username, Password} ->
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["register_user", ",", Username, ",", Password]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("Result is ~p ~n", [Result]),
          do_nothing
      end,
      init_all_services();
    {"login_user", Username, Password} ->
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["login_user", ",", Username, ",", Password]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("Result is ~p ~n", [Result]),
          do_nothing
      % io:format("The result received is ~p ~n", [Result])
      end,
      init_all_services();
    {"logoff_user", Username} ->
      io:format("LogOff User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["logoff_user", ",", Username]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("Result is ~p ~n", [Result]),
          do_nothing
      % io:format("The result received is ~p ~n", [Result])
      end,
      init_all_services();
    {"user_follow", Username1, Username2} ->
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["user_follow", ",", Username1, ",", Username2]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("Result is ~p ~n", [Result]),
          do_nothing
      % io:format("The result received is ~p ~n", [Result])
      end,
      init_all_services();
    {"send_tweet", Username, Tweet} ->
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["send_tweet", ",", Username, ",", Tweet]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("Result is ~p ~n", [Result]),
          true
      end,
      init_all_services();
    {"get_all_mentions", Mention_String} ->

      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["search_for_mentions", ",", Mention_String]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("Mention Result is ~p ~n", [Result]),
          true
      end,
      init_all_services();
    {"get_all_hashtags", Mention_String} ->
      Start_Time = erlang:system_time(millisecond),
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["search_for_hashtags", ",", Mention_String]),
      receive
        {tcp,Socket, Result} ->
          Result,
          End_Time = erlang:system_time(millisecond),
          io:format("Hashtag result received is ~p ~n", [Result]),
          Total_Diff = (End_Time - Start_Time),
          io:format("***** HashTag Query Search ***** - ~pms ~n", [Total_Diff])

      end,
      init_all_services();
    {"retweet", Username, Tweet_Id} ->
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["retweet", ",", Username, ",", Tweet_Id]),
      receive
        {tcp,Socket, Result} ->
          Result,
          do_nothing,
          io:format("The result received is ~p ~n", [Result])
      end,
      init_all_services();
    {"getMyTweets", Username} ->
      Start_Time = erlang:system_time(millisecond),
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["get_user_tweets", ",", Username]),
      receive
        {tcp,Socket, Result} ->
          End_Time = erlang:system_time(millisecond),
          Total_Diff = (End_Time - Start_Time),
          io:format("***** Get Query search ***** - ~pms ~n", [Total_Diff]),
          Msg = lists:droplast(string:split(Result, "//", all)),
          lists:foreach(fun(Ind) ->
            io:format("~p ~n", [Ind])
                        end, Msg)

      end,
      init_all_services();
    {"print_stats"} ->
      io:format("Fetching Stats ~n"),
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["print_stats", ",","system"]),
      receive
        {tcp,Socket, Result} ->
          Result,
          do_nothing,
          io:format("~p", [Result])

      end,
      init_all_services();
    {"bulk_subscribe", PID} ->
      {ok, ParsedAddress} = inet:parse_address("10.3.0.24"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["bulk_user_subscribe"]),
      receive
        {tcp,Socket, Result} ->
          Result,
          io:format("bulk call result received ~p ~n", [Result]),
          do_nothing,
          List = binary_to_term(Result),
          PID ! {"success", List}
      end,
      init_all_services()
  end.






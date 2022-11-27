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


-define(IP_ADDRESS,"10.20.0.124").

%% API

start() ->

  PID = spawn(?MODULE, init_all_services, []),
  register(init_services, PID),
  do_nothing.

init_all_services() ->
  receive
    {tcp,Socket, Result} ->
      Socket,
      io:format("Timeline -  ~p ~n", [Result]),
      init_all_services();
    {"register_user", Username, Password} ->
      io:format("Registering User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["register_user", ",", Username, ",", Password]),
      % my_api_caller! {"register_user", Username, Password},
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      init_all_services();
    {"login_user", Username, Password} ->
      io:format("Login User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["login_user", ",", Username, ",", Password]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      %my_api_caller ! {"login_user", Username, Password},
      init_all_services();
    {"logoff_user", Username, Password} ->
      io:format("LogOff User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["logoff_user", ",", Username, ",", Password]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      %my_api_caller ! {"logoff_user", Username, Password},
      init_all_services();
    {"user_follow", Username1, Username2} ->
      io:format("User Follow ~p ~p ~n", [Username1, Username2]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["user_follow", ",", Username1, ",", Username2]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      % my_api_caller ! {"user_follow", Username1, Username2},
      init_all_services();
    {"send_tweet", Username, Tweet} ->
      io:format("User ~p Tweet ~p ~n", [Username, Tweet]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["send_tweet", ",", Username, ",", Tweet]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,

      % my_api_caller ! {"send_tweet", Username, Tweet},
      init_all_services();
    {"get_all_mentions", Mention_String} ->
      io:format("Searching for String ~p ~n", [Mention_String]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["search_for_mentions", ",", Mention_String]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,

      %my_api_caller ! {"search_for_mentions", Mention_String},
      init_all_services();
    {"get_all_hashtags", Mention_String} ->
      io:format("Searching for String ~p ~n", [Mention_String]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["search_for_hashtags", ",", Mention_String]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      %my_api_caller ! {"search_for_hashtags", Mention_String},
      init_all_services();
    {"retweet", Username, Tweet_Id} ->
      io:format("Username ~p has Retweeted ~p ~n", [Username, Tweet_Id]),
      {ok, ParsedAddress} = inet:parse_address("10.20.0.119"),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["retweet", ",", Username, ",", Tweet_Id]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      %my_api_caller ! {"search_for_hashtags", Mention_String},
      init_all_services()
  end.






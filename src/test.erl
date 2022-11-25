%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2022 4:43 PM
%%%-------------------------------------------------------------------
-module(test).
-author("akhil").

%% API
-export([test/0, construct_list/3, init_all_services/0]).
-define(IP_ADDRESS,"").


construct_list(0, Mentions, Array) ->
  io:format("Mentions ~p ~n", [Mentions]),
  Array;


construct_list(Len, Mentions, Array) ->
  Ele = lists:nth(Len, Mentions),
  Inner_Ele = lists:nth(1, Ele),
  construct_list(Len - 1, Mentions, lists:append(Array, [Inner_Ele])).



test() ->
  Tweet = "asdds @a, sdjjkld asjdksd @Rohit,@Virat, @het asdad13134234 @sdsd, @sdsdf , add, @12",
  Mentions = re:run(Tweet, "(?<=@)\\w+", [global, {capture,all, list}]),
  {match, F} = Mentions,
  Mention_List = construct_list(length(F), F, []),

  io:format("Mention List is ~p ~n", [Mention_List]).



init_all_services() ->
  receive
    {"register_user", Username, Password} ->
      io:format("Registering User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["register_user", ",", Username, ",", Password]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      init_all_services();
    {"login_user", Username, Password} ->
      io:format("Login User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["login_user", ",", Username, ",", Password]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      % my_api_caller ! {"login_user", Username, Password},
      init_all_services();
    {"logoff_user", Username, Password} ->
      io:format("LogOff User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
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
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
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
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["send_tweet", ",", Username, ",", Tweet]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      %my_api_caller ! {"send_tweet", Username, Tweet},
      init_all_services();
    {"get_all_mentions", Mention_String} ->
      io:format("Searching for String ~p ~n", [Mention_String]),
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["search_for_mentions", ",", Mention_String]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      % my_api_caller ! {"search_for_mentions", Mention_String},
      init_all_services();
    {"get_all_hashtags", Mention_String} ->
      io:format("Searching for String ~p ~n", [Mention_String]),
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(Socket, ["search_for_hashtags", ",", Mention_String]),
      receive
        {tcp,Socket, Result} ->
          io:format("The result received is ~p ~n", [Result])
      end,
      % my_api_caller ! {"search_for_hashtags", Mention_String},
      init_all_services()
  end.


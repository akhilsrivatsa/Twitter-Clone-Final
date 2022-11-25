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


-define(IP_ADDRESS,"").

%% API

start() ->

  PID = spawn(?MODULE, init_all_services, []),
  register(init_services, PID),
  do_nothing.

init_all_services() ->
  receive
    {"register_user", Username, Password} ->
      io:format("Registering User ~p ~n", [Username]),
      {ok, ParsedAddress} = inet:parse_address(?IP_ADDRESS),
      {ok, Socket} = gen_tcp:connect(ParsedAddress, 9000, [binary,{active, true}]),
      gen_tcp:send(gen_tcp:send(Socket, {"register_user", Username, Password})),
      % my_api_caller! {"register_user", Username, Password},
      init_all_services();
    {"login_user", Username, Password} ->
      io:format("Login User ~p ~n", [Username]),
      my_api_caller ! {"login_user", Username, Password},
      init_all_services();
    {"logoff_user", Username, Password} ->
      io:format("LogOff User ~p ~n", [Username]),
      my_api_caller ! {"logoff_user", Username, Password},
      init_all_services();
    {"user_follow", Username1, Username2} ->
      io:format("User Follow ~p ~p ~n", [Username1, Username2]),
      my_api_caller ! {"user_follow", Username1, Username2},
      init_all_services();
    {"send_tweet", Username, Tweet} ->
      io:format("User ~p Tweet ~p ~n", [Username, Tweet]),
      my_api_caller ! {"send_tweet", Username, Tweet},
      init_all_services();
    {"get_all_mentions", Mention_String} ->
      io:format("Searching for String ~p ~n", [Mention_String]),
      my_api_caller ! {"search_for_mentions", Mention_String},
      init_all_services();
    {"get_all_hashtags", Mention_String} ->
      io:format("Searching for String ~p ~n", [Mention_String]),
      my_api_caller ! {"search_for_hashtags", Mention_String},
      init_all_services()
  end.






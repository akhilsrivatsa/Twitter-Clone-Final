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
    {"register_user", Username, Password} ->
      io:format("Registering User ~p ~n", [Username]),
      my_api_caller! {"register_user", Username, Password},
      init_all_services();
    {"login_user", Username, Password} ->
      io:format("Login User ~p ~n", [Username]),
      my_api_caller ! {"login_user", Username, Password},
      init_all_services();
    {"logoff_user", Username, Password} ->
      io:format("LogOff User ~p ~n", [Username]),
      my_api_caller ! {"logoff_user", Username, Password},
      init_all_services()
  end.






%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2022 6:54 PM
%%%-------------------------------------------------------------------
-module(main_server).
-author("akhil").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, api_handler/0, init_all_tables/ 0, user_registration_service / 2, fell/0]).

-define(SERVER, ?MODULE).

-record(main_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

fell()->
  io:format("fck").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  init_all_tables(),
  PID = spawn(?MODULE, api_handler, []),
  register(my_api_caller, PID),
  {ok, #main_server_state{}}.

init_all_tables() ->
  ets:new(user_accounts, [set, named_table, public]),
  ets:new(user_tweets, [bag, named_table, public]),
  ets:new(mentions_hashtags, [bag, named_table, public]),
  ets:new(follower_store, [bag, named_table, public]).

api_handler() ->
  receive
    {"register_user", Username, Password} ->
      Res = user_registration_service(Username, Password),
      io:format("User - ~p ~n", [Res]),
      api_handler();
    {"login_user", Username, Password} ->
      io:format("User Account Table is ~p ~n", [ets:lookup(user_accounts, Username)]),
      Res = user_login_service(Username, Password),
      io:format("User Account Table is ~p ~n", [ets:lookup(user_accounts, Username)]),
      io:format("User ~p - ~p ~n", [Username,Res]),
      api_handler();
    {"logoff_user", Username, Password} ->
      io:format("User Account Table is ~p ~n", [ets:lookup(user_accounts, Username)]),
      Res = user_logoff_service(Username, Password),
      io:format("User Account Table is ~p ~n", [ets:lookup(user_accounts, Username)]),
      io:format("User ~p - ~p ~n", [Username,Res]),
      api_handler()
  end.


user_registration_service(Username, Password)->
  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    user_already_exists;
  true ->
    ets:insert(user_accounts, {Username, Password, "offline"}),
    user_registered
  end.

user_login_service(Username, Password) ->

  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    {Us, Pw, St} = lists:nth(1, Res),
    if Pw == Password ->
      ets:insert(user_accounts, {Username, Password, "online"}),
      user_logged_in;
      true ->
        Us,
        St,
        invalid_credentials
    end;
    true ->
      io:format("User Doesn't exist ~n"),
      user_does_not_exist
  end.

user_logoff_service(Username, Password) ->
  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    ets:insert(user_accounts, {Username, Password, "offline"}),
    user_logged_off;
    true ->
      io:format("User Doesn't exist ~n"),
      user_does_not_exist
  end.

handle_call(_Request, _From, State = #main_server_state{}) ->
  io:format("Called...."),
  {reply, ok, State}.

handle_cast(_Request, State = #main_server_state{}) ->
  {noreply, State}.


handle_info(_Info, State = #main_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #main_server_state{}) ->
  ok.

code_change(_OldVsn, State = #main_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

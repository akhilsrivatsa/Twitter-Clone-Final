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

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, api_handler/0, init_all_tables/ 0, user_registration_service / 2]).

-define(SERVER, ?MODULE).
-define(PORT, 9000).

-record(main_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
handler(ASocket) ->
  inet:setopts(ASocket, [{active, once}] ),
  receive
    {tcp,ASocket,BinaryMsg} ->
      io:format("Here ~p ~n", [BinaryMsg]),
      Msg = string:split(BinaryMsg, ",", all),
      B=  [binary_to_list(Item) || Item <- Msg],
      BList = lists:append(B, [ASocket]),
      Tuple = list_to_tuple(BList),
      my_api_caller ! Tuple,
      handler(ASocket)
  end.


accept_state(LSocket) ->
  {ok, ASocket} = gen_tcp:accept(LSocket),
  spawn(fun() -> accept_state(LSocket) end),
  handler(ASocket).


init_tcp() ->
  PID =  spawn_link(fun() ->
    {ok, LSocket} = gen_tcp:listen(?PORT, [binary, {active, false}]),
    spawn( fun() -> accept_state(LSocket) end),
    timer:sleep(infinity)
                    end),
  {ok, PID}.


init([]) ->

  init_all_tables(),
  PID = spawn(?MODULE, api_handler, []),
  register(my_api_caller, PID),
  io:format("Inited"),
  init_tcp(),

  {ok, #main_server_state{}}.

init_all_tables() ->
  ets:new(user_accounts, [set, named_table, public]),
  ets:new(user_tweets, [bag, named_table, public]),
  ets:new(mentions_hashtags, [bag, named_table, public]),
  ets:new(follower_store, [bag, named_table, public]).

api_handler() ->
  receive
    {"register_user", Username, Password, ASocket} ->
      Res = user_registration_service(Username, Password),
      %Response = Username ++ " " ++ Res ,
      gen_tcp:send(ASocket, atom_to_list(Res) ),
      api_handler();
    {"login_user", Username, Password, ASocket} ->
      Res = user_login_service(Username, Password),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler();
    {"logoff_user", Username, Password, ASocket} ->
      io:format("User Account Table is ~p ~n", [ets:lookup(user_accounts, Username)]),
      Res = user_logoff_service(Username, Password),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler();
    {"user_follow", Username1, Username2, ASocket} ->
      io:format("User Follow Table is ~p ~n", [ets:lookup(follower_store, Username2)]),
      Res = user_follow_service(Username1, Username2),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler();
    {"send_tweet", Username, Tweet, ASocket} ->
      io:format("Username ~p tweeted ~p ~n", [Username, Tweet]),
      Res = user_sending_tweet(Username, Tweet),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler();
    {"search_for_mentions", Search_String, ASocket} ->
      io:format("Search for String ~p ~n", [Search_String]),
      Res = query_tweets_with_mentions(Search_String),
      io:format("Res ~p ~n", [Res]),
      gen_tcp:send(ASocket, lists:flatten(io_lib:format("~p",[Res]))),
      api_handler();
    {"search_for_hashtags", Search_String, ASocket} ->
      io:format("Search for String ~p ~n", [Search_String]),
      Res = query_tweets_with_hashtags(Search_String),
      gen_tcp:send(ASocket, lists:flatten(io_lib:format("~p",[Res]))),
      api_handler()
  end.


query_tweets_with_mentions(Search_String) ->
    ets:lookup(mentions_hashtags, {Search_String, "MENTION"}).

query_tweets_with_hashtags(Search_String) ->
  ets:lookup(mentions_hashtags, {Search_String, "HASHTAG"}).



is_user_logged_in(Username) ->
  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    {Us, Pw, St} = lists:nth(1, Res),
    Us, Pw,
    if St == "offline" -> user_offline;
    true -> true
    end;
    true -> io:format("Username ~p not found ~n", [Username]),
            user_not_found
  end.

construct_list(0, Mentions, Array) ->
  io:format("Mentions ~p ~n", [Mentions]),
  Array;


construct_list(Len, Mentions, Array) ->
  Ele = lists:nth(Len, Mentions),
  Inner_Ele = lists:nth(1, Ele),
  construct_list(Len - 1, Mentions, lists:append(Array, [Inner_Ele])).



get_all_mentions(Tweet) ->
  F = re:run(Tweet, "(?<=@)\\w+", [global, {capture,all, list}]),
  io:format("rep ~p ~n", [F]),
  if F /= nomatch ->
    {match, Mentions} = F,
    Mention_List = construct_list(length(Mentions), Mentions, []),
    Mention_List;
    true -> Mention_List = [],
            Mention_List
  end.


get_all_hashtags(Tweet) ->
  F = re:run(Tweet, "(?<=#)\\w+", [global, {capture,all, list}]),
  if F /= nomatch ->
    {match, HashTags} = F,
    HashTag_List = construct_list(length(HashTags), HashTags, []),
    HashTag_List;
  true -> HashTag_List = [],
          HashTag_List
  end.




update_mentions_table(Username, Tweet, TweetMentions) ->
  io:format("UPDATING MENTIONS TABLE ~n"),
  lists:foreach(
    fun(Mention) ->
      io:format("Mention ~p ~n", [Mention]),
     ets:insert(mentions_hashtags, {{Mention, "MENTION"}, Tweet, Username})
    end, TweetMentions).

update_hashtags_table(Username, Tweet, TweetHashTags) ->
  io:format("UPDATING HASHTAGS TABLE ~n"),
  lists:foreach(
    fun(Mention) ->
      ets:insert(mentions_hashtags, {{Mention, "HASHTAG"}, Tweet, Username})
    end, TweetHashTags).

user_sending_tweet(Username, Tweet) ->
   Res = is_user_logged_in(Username),
  if Res == true ->
    io:format("Username ~p and Tweet ~p ~n", [Username, Tweet]),
    Tweet_Mentions = get_all_mentions(Tweet),
    Tweet_Hashtags = get_all_hashtags(Tweet),
    update_mentions_table(Username, Tweet, Tweet_Mentions),
    update_hashtags_table(Username, Tweet, Tweet_Hashtags),

    io:format("Tweet Mention ~p ~n", [Tweet_Mentions]),
    io:format("Tweet Hashtags ~p ~n", [Tweet_Hashtags]);

    true -> user_offline_or_invalid_tweet_cant_be_delivered
  end.


user_follow_service(Username1, Username2) ->
  Res2 = ets:lookup(user_accounts, Username1),
  Res1 = ets:lookup(user_accounts, Username2),
  Res2,
  if ( length(Res1) /= 0  andalso length(Res2) /= 0 ) ->
    io:format("Both Users are valid ~n"),
    ets:insert(follower_store, {Username2, Username1}),
    user1_follows_user2;
  true->
    io:format("Unable to make follow connection in follower_store table ~n"),
    user1_or_user2_not_found
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

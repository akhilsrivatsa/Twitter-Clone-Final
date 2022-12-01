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
  code_change/3, api_handler/7, init_all_tables/ 0, user_registration_service / 2]).

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
      Msg = string:split(BinaryMsg, ",", all),
      io:format("Msg Received ~p ~n", [Msg]),
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
  PID = spawn(?MODULE, api_handler, [0, 0, 0, 0, 0, 0,0]),
  register(my_api_caller, PID),
  init_tcp(),

  {ok, #main_server_state{}}.

init_all_tables() ->
  ets:new(user_accounts, [set, named_table, public]),
  ets:new(user_tweets, [bag, named_table, public]),
  ets:new(mentions_hashtags, [bag, named_table, public]),
  ets:new(follower_store, [bag, named_table, public]),
  ets:new(user_feed_store, [bag, named_table, public]),
  ets:new(user_follow_count, [set, named_table, public]),
  ets:new(user_list, [set, named_table, public]),

  ets:new(tweet_counter, [set, named_table, public]),
  ets:new(user_counter, [set, named_table, public]),

  ets:insert(tweet_counter, {"last_updated_counter", 0}),
  ets:insert(user_counter, {"last_updated_counter", 0}).

api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register, First_Tweet, Last_Tweet) ->
  receive
    {"register_user", Username, Password, ASocket} ->

      if First_Api_Call == 0 ->
        T = erlang:system_time(millisecond);
      true ->
        T = First_Api_Call
     end,
      Res = user_registration_service(Username, Password),
        gen_tcp:send(ASocket, atom_to_list(Res)),
        api_handler(Users + 1, Tweets, Retweets, T, erlang:system_time(millisecond),First_Tweet, Last_Tweet);
    {"login_user", Username, Password, ASocket} ->
      Res = user_login_service(Username, Password, ASocket),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet, Last_Tweet);
    {"logoff_user", Username,ASocket} ->
      io:format("User Account Table is ~p ~n", [ets:lookup(user_accounts, Username)]),
      Res = user_logoff_service(Username),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet,  Last_Tweet);
    {"user_follow", Username1, Username2, ASocket} ->
      Res = user_follow_service(Username1, Username2),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register, First_Tweet,Last_Tweet);
    {"send_tweet", Username, Tweet, ASocket} ->
      if First_Tweet == 0 ->
        T = erlang:system_time(millisecond);
        true ->
          T = First_Tweet
      end,
      Res = user_sending_tweet(Username, Tweet),
      gen_tcp:send(ASocket, atom_to_list(Res)),
      api_handler(Users, Tweets + 1, Retweets, First_Api_Call, Last_Register,T,erlang:system_time(millisecond));
    {"search_for_mentions", Search_String, ASocket} ->
      StartTime = erlang:system_time(millisecond),
      Res = query_tweets_with_mentions(Search_String),
      EndTime = erlang:system_time(millisecond),
      io:format("*******Query Time for Mentions***** => ~p ~n", [EndTime - StartTime]),
      gen_tcp:send(ASocket, lists:flatten(io_lib:format("~p",[Res]))),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet, Last_Tweet);
    {"search_for_hashtags", Search_String, ASocket} ->
      StartTime = erlang:system_time(millisecond),
      Res = query_tweets_with_hashtags(Search_String),
      EndTime = erlang:system_time(millisecond),
      io:format("*******Query Time for Hashtags***** => ~p ~n", [EndTime - StartTime]),
      gen_tcp:send(ASocket, lists:flatten(io_lib:format("~p",[Res]))),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet, Last_Tweet);
    {"retweet", Username, Tweet_Id, ASocket} ->
      {_, Tweet, _} = lists:nth(1,ets:lookup(user_tweets, list_to_integer(Tweet_Id))),
      %io:format("Tweet is ~p ~n", [Tweet]),
      send_tweet_to_all_online_followers(Username,Username ++ "retweeted- " ++ Tweet),
      gen_tcp:send(ASocket, lists:flatten("Retweet operation sucessful ~n")),
      api_handler(Users, Tweets, Retweets + 1, First_Api_Call, Last_Register,First_Tweet, Last_Tweet);
    {"get_user_tweets", Username, ASocket} ->
      Res = query_user_tweets(Username),
      io:format("Res ~p ~n", [Res]),
      gen_tcp:send(ASocket, Res),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet, Last_Tweet);
    {"bulk_user_subscribe", ASocket} ->
      io:format("Subscribing Users in bulk ~n"),
      subscribe_users_in_bulk(),
      {"last_updated_counter", Users} = lists:nth(1, ets:lookup(user_counter, "last_updated_counter")),
      Tup = populate_tup(Users, []),
      Sorted_tup = lists:keysort(2 ,Tup),
      io:format("Sorted Tuple list ~p ~n", [Sorted_tup]),
      gen_tcp:send(ASocket, term_to_binary(Sorted_tup)),
      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet, Last_Tweet);
    {"print_stats", "system",ASocket } ->
      io:format("*******Total Number of Registered Users***** => ~p ~n", [Users]),
      io:format("*******Total Number of Tweets******** => ~p ~n ", [Tweets]),
      io:format("*******Total Number of Retweets******** => ~p ~n ", [Retweets]),
      io:format("*****Total Time To Register******* => ~p ms ~n", [Last_Register - First_Api_Call]),
      io:format("******Total Time To Tweet********* => ~p ms  ~n", [Last_Tweet - First_Tweet]),
      io:format("******* Total Execution Time******** => ~p ms ~n", [erlang:system_time(millisecond) - First_Api_Call]),

      gen_tcp:send(ASocket, "Completed Printing Stats"),

      api_handler(Users, Tweets, Retweets, First_Api_Call, Last_Register,First_Tweet, Last_Tweet)
  end.


populate_tup(0, List) ->
  List;

populate_tup(Index, List) ->
  {K, V} = lists:nth(1,ets:lookup(user_follow_count, Index)),
  populate_tup(Index - 1,  lists:append(List, [{K, V}])).


subscribe_users_in_bulk() ->
  io:format("SUBSCRIBING USERS IN BULK ~n"),

  {"last_updated_counter", Users} = lists:nth(1, ets:lookup(user_counter, "last_updated_counter")),
  io:format("User count  ~p ~n", [Users]),
  lists:foreach(
    fun(Followed_User_ID) ->
      {_ , User2} = lists:nth(1,ets:lookup(user_list, Followed_User_ID)),
      Rand_number_of_followers = round(rand:uniform(Users - 1)),
      ets:insert(user_follow_count, {Followed_User_ID,Rand_number_of_followers}),
      lists:foreach(fun(_) ->
        Rand_Follower_Id = round(rand:uniform(Users)),
        {_ , User1} = lists:nth(1,ets:lookup(user_list, Rand_Follower_Id)),
        if User1 /= User2  ->
          ets:insert(follower_store, {User2, User1});
          true -> do_nothing
        end
                    end, lists:seq(1, Rand_number_of_followers))

    end, lists:seq(1, Users)).


feed_all_tweets(0, AllTweets, Res) ->
  AllTweets,
  Res;

feed_all_tweets(Index, AllTweets, Res) ->
  {_, Tweet} = lists:nth(Index, AllTweets),
  feed_all_tweets(Index - 1, AllTweets, lists:append(Res, [Tweet])).


query_user_tweets(Username) ->
  All_Tweets = ets:lookup(user_feed_store, Username),
  feed_all_tweets(length(All_Tweets), All_Tweets, []).


query_tweets_with_mentions(Search_String) ->
  io:format("Im here, Search string is ~p ~n", [Search_String]),
  Res =   ets:lookup(mentions_hashtags, {Search_String, "MENTION"}),
  io:format("Result is ~p", [Res]),
  Res.

query_tweets_with_hashtags(Search_String) ->
  ets:lookup(mentions_hashtags, {Search_String, "HASHTAG"}).


is_user_logged_in(Username) ->
  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    {Us, Pw, St, _} = lists:nth(1, Res),
    Us, Pw,
    if St == "offline" -> user_offline;
    true -> true
    end;
    true -> io:format("Username ~p not found ~n", [Username]),
            user_not_found
  end.

construct_list(0, Mentions, Array) ->
  Mentions,
  Array;


construct_list(Len, Mentions, Array) ->
  Ele = lists:nth(Len, Mentions),
  Inner_Ele = lists:nth(1, Ele),
  construct_list(Len - 1, Mentions, lists:append(Array, [Inner_Ele])).

get_all_mentions(Tweet) ->
  F = re:run(Tweet, "(?<=@)\\w+", [global, {capture,all, list}]),
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
  lists:foreach(
    fun(Mention) ->
      ets:insert(mentions_hashtags, {{Mention, "MENTION"}, Tweet, Username}),
      ets:insert(user_feed_store, {Mention, Tweet})
    end, TweetMentions).

update_hashtags_table(Username, Tweet, TweetHashTags) ->
  lists:foreach(
    fun(Mention) ->
      ets:insert(mentions_hashtags, {{Mention, "HASHTAG"}, Tweet, Username})
    end, TweetHashTags).

fetch_all_followers_list(0, Followers, OnlineUserList) ->
  Followers,
  OnlineUserList;

fetch_all_followers_list(Index, Followers, OnlineUserList) ->
  {_,Follower} = lists:nth(Index, Followers),
  fetch_all_followers_list(Index - 1, Followers, lists:append(OnlineUserList, [Follower])).

fetch_all_Online_Users_With_Port(0, Followers_List, Online_Users) ->
  Followers_List,
  Online_Users;

fetch_all_Online_Users_With_Port(Index, Followers_List, Online_Users) ->
  Follower  = lists:nth(Index, Followers_List),
  Res = ets:lookup(user_accounts, Follower),
  {User, Password, Status, Port}= lists:nth(1, Res),
  Password,
  if Status == "online" ->
    fetch_all_Online_Users_With_Port(Index - 1, Followers_List, lists:append(Online_Users, [{User, Port}]));
  true ->
    fetch_all_Online_Users_With_Port(Index - 1, Followers_List, Online_Users)
  end.

get_all_followers_of_user(Username) ->
  Followers_Tuple_List = ets:lookup(follower_store, Username),
  Followers_List = fetch_all_followers_list(length(Followers_Tuple_List), Followers_Tuple_List, []),
  Online_Followers_List = fetch_all_Online_Users_With_Port(length(Followers_List), Followers_List, []),
  Online_Followers_List.


send_tweet(0, Tweet, Followers, Username) ->
  Tweet, Followers, Username,
  end_recursive_loop;


send_tweet(Index, Tweet, Followers, Username) ->
  {Follower, Port} = lists:nth(Index, Followers),
  Follower,
  String = Username ++ " tweeted =>" ++ Tweet ++ "//",
  gen_tcp:send(Port, String),
  send_tweet(Index - 1, Tweet, Followers, Username).


populate_tweet_store(0, Tweet, All_Followers) ->
  Tweet, All_Followers,
  do_nothing;

populate_tweet_store(Index, Tweet, All_Followers) ->

  Follower = lists:nth(Index, All_Followers),
  ets:insert(user_feed_store, {Follower, Tweet}),
  populate_tweet_store(Index - 1, Tweet, All_Followers).


send_tweet_to_all_online_followers(Username, Tweet) ->

  Followers_Tuple_List = ets:lookup(follower_store, Username),
  All_Followers = fetch_all_followers_list(length(Followers_Tuple_List), Followers_Tuple_List, []),
  String = Username ++ " tweeted =>" ++ Tweet ++ "//",
  populate_tweet_store(length(All_Followers), String, All_Followers),
  Online_Followers = get_all_followers_of_user(Username),
  send_tweet(length(Online_Followers), Tweet, Online_Followers, Username).


send_tweet_to_all_mentions(0, Username, Tweet, TweetMentions) ->
  Username, Tweet, TweetMentions,
  end_of_mentions_loop;

send_tweet_to_all_mentions(Index,Username, Tweet, TweetMentions) ->
  Mention = lists:nth(Index, TweetMentions),
  Res = ets:lookup(user_accounts, Mention),
  if length(Res) > 0 ->
      {User, Password, Status, Port} = lists:nth(1, Res),
      if Status == "online" ->

        String = Username ++ " tweeted =>" ++ Tweet ++ "//",
        Password,
        gen_tcp:send(Port, String),
        send_tweet_to_all_mentions(Index - 1, User, Tweet, TweetMentions);

        true -> true
      end;
    true -> io:format("Mention ~p is not present in the user table ~n", [Mention]),
           send_tweet_to_all_mentions(Index - 1, Username, Tweet, TweetMentions)
  end.

user_sending_tweet(Username, Tweet) ->
   Res = is_user_logged_in(Username),
  if Res == true ->
    Tweet_Mentions = get_all_mentions(Tweet),
    Tweet_Hashtags = get_all_hashtags(Tweet),

    {"last_updated_counter", Last_Counter} = lists:nth(1, ets:lookup(tweet_counter, "last_updated_counter")),

    Current_Counter = Last_Counter + 1,
    %io:format("Current tweet ~p ~n", [Current_Counter]),

    ets:insert(user_tweets, {Current_Counter, Tweet, Username}),
    ets:insert(tweet_counter, {"last_updated_counter",Current_Counter}),
    %io:format("last tweet ~p ~n", [Last_Counter_]),
    ets:lookup(user_tweets, Last_Counter + 1),
    String = Username ++ " tweeted =>" ++ Tweet ++ "//",
    update_mentions_table(Username, String, Tweet_Mentions),
    update_hashtags_table(Username, Tweet, Tweet_Hashtags),
    send_tweet_to_all_online_followers(Username, Tweet),
    send_tweet_to_all_mentions(length(Tweet_Mentions), Username, Tweet, Tweet_Mentions),
    sent_tweet;
    true -> user_offline_or_invalid_tweet_cant_be_delivered
  end.


user_follow_service(Username1, Username2) ->
  Res2 = ets:lookup(user_accounts, Username1),
  Res1 = ets:lookup(user_accounts, Username2),
  Res2,
  if ( length(Res1) /= 0  andalso length(Res2) /= 0 ) ->
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
    {"last_updated_counter", Last_Counter} = lists:nth(1, ets:lookup(user_counter, "last_updated_counter")),
    ets:insert(user_list, {Last_Counter + 1, Username}),
    ets:insert(user_counter, {"last_updated_counter",Last_Counter + 1}),
    ets:insert(user_accounts, {Username, Password, "offline", ""}),
    user_registered
  end.

user_login_service(Username, Password, ASocket) ->
  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    {Us, Pw, St, _} = lists:nth(1, Res),
    if Pw == Password ->
      ets:insert(user_accounts, {Username, Password, "online", ASocket}),
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

user_logoff_service(Username) ->
  Res = ets:lookup(user_accounts, Username),
  if length(Res) > 0 ->
    {_,Password,_, _} = lists:nth(1, Res),
    ets:insert(user_accounts, {Username, Password, "offline", ""}),
    user_logged_off;
    true ->
      io:format("User Doesn't exist ~n"),
      user_does_not_exist
  end.

handle_call(_Request, _From, State = #main_server_state{}) ->
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

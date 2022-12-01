%%%-------------------------------------------------------------------
%%% @author akhil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(simulator).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, perform_zip_f_tweet_distribution/4]).

-define(SERVER, ?MODULE).

-record(simulator_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


perform_zip_f_tweet_distribution(0, Zip_f, Sorted_tup, Users_To_Disconnect) ->
  Zip_f, Sorted_tup, Users_To_Disconnect,
  do_nothing;


perform_zip_f_tweet_distribution(Index, Zip_f, Sorted_tup, Users_To_Disconnect) ->
  Hash = "#COP5615",
  {_,AT,_} = lists:nth(1, ets:lookup(user_accounts, 1)),
  Mention = "@" ++ AT,

  io:format("Hash is ~p and Mention is ~p ~n", [Hash, Mention]),

  {K,_} = lists:nth(Index, Sorted_tup),
  {_,User1,_} = lists:nth(1, ets:lookup(user_accounts, K)),
  Size = round(Zip_f / (length(Sorted_tup)- Index + 1)),
  io:format("Sending Tweet for User ~p and no.of tweets are ~p ~n ", [Index, Size]),
  io:format("User is ~p ~n", [User1]),
  lists:foreach(
    fun(Counter) ->
      if Counter rem 10 == 0 ->
        init_services ! {"retweet", User1, "1"};
        true ->
          if Counter rem 3 == 0 ->
            init_services ! {"send_tweet", User1, (base64:encode_to_string(crypto:strong_rand_bytes(100))) ++ (Hash)};
            true ->
              if Counter rem 5 == 0 andalso Counter /= 1 ->
                Tweet = (base64:encode_to_string(crypto:strong_rand_bytes(100))) ++ (Mention),
                io:format("The tweet is ~p ~n", [Tweet]),
                init_services ! {"send_tweet", User1, Tweet};
                true ->
                  init_services ! {"send_tweet", User1, (base64:encode_to_string(crypto:strong_rand_bytes(100)))}
              end
          end
      end
    end, lists:seq(1, Size)),
  init_services ! {"logoff_user", User1},
  perform_zip_f_tweet_distribution(Index - 1, Zip_f, Sorted_tup, Users_To_Disconnect).




init([]) ->
  {ok, Users} = io:read("Enter no.of users to simulate ~n"),
  {ok, Zip_F_constant} = io:read("Enter max no.of tweets ~n"),Zip_F_constant,
  {ok, Percentage_to_Disconnect} = io:read("Enter max no.of users to disconnect ~n"),
  Users_To_Disconnect = (Percentage_to_Disconnect / 100) * Users,
  io:format("No.of users are ~p ~n", [Users]),
  io:format("No.of users to disconnect ~p ~n", [Users_To_Disconnect]),

  init_all_tables(),
  Registration_Start_time  = erlang:system_time(millisecond),
  perform_registration_and_login(Users),
  Registration_End_time = erlang:system_time(millisecond),
  Total_Registration_Diff = (Registration_End_time - Registration_Start_time),

  io:format("*** USER REGISTRATION AND LOGIN COMPLETE in ~p seconds *** ~n", [Total_Registration_Diff]),

  io:format("*** PERFORM USER BULK FOLLOW *** ~n"),
  init_services ! {"bulk_subscribe", self()},
  receive
    {"success", List} -> perform_zip_f_tweet_distribution(length(List),Zip_F_constant, List, Percentage_to_Disconnect)
  end,
  init_services ! {"get_all_hashtags", "COP5615"},
  {_,AT,_} = lists:nth(1, ets:lookup(user_accounts, 1)),
  io:format("at is ~p ~n", [AT]),
  init_services ! {"getMyTweets", AT},


  generate_stats(),

  {ok, #simulator_state{}}.


generate_stats() -> init_services ! {"print_stats"}.


generate_random_string() -> base64:encode_to_string(crypto:strong_rand_bytes(32)).

init_all_tables() ->
  ets:new(user_accounts, [set, named_table, public]),
  ets:new(user_follow_count, [set, named_table, public]).

perform_registration_and_login(Users) ->
  io:format("Performing Registration for ~p Users ~n", [Users]),
  lists:foreach(
    fun(Id) ->
      Username = generate_random_string(),
      Password = generate_random_string(),
      init_services ! {"register_user", Username, Password},
      ets:insert(user_accounts, {Id, Username, Password}),
      init_services ! {"login_user", Username, Password},
      io:format("Random username ~p and password ~p  ~n", [Username, Password])

    end, lists:seq(1, Users)).

handle_call(_Request, _From, State = #simulator_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #simulator_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #simulator_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #simulator_state{}) ->
  ok.

code_change(_OldVsn, State = #simulator_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

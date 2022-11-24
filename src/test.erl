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
-export([test/0, construct_list/3]).


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



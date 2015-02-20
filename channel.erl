-module(channel).
-include_lib("./defs.hrl").
-export([main/1, initial_state/1]).

main(State) ->
  receive
    {request, From, Ref, Request} ->
      {Response, NextState} = loop(State, Request),
      From ! {result, Ref, Response},
      main(NextState)
  end.

initial_state(ChannelName) ->
  #ch_st{name = ChannelName}.

loop(St, {member, {Nick, _}}) ->
  {lists:keymember(Nick, 1, St#ch_st.users), St}; 

loop(St, {join, User}) ->
  {Nick, _} = User,
  case lists:keymember(Nick, 1, St#ch_st.users) of
    false ->
      NewState = St#ch_st{users = [ User | St#ch_st.users ]},
      {ok, NewState};
    true ->
      {{error, user_already_joined}, St}
  end;

loop(St, {leave, User}) ->
  {Nick, _} = User,
  case lists:keymember(Nick, 1, St#ch_st.users) of
    true ->
      NewState = St#ch_st{users = lists:delete(User, St#ch_st.users)},
      {ok, NewState};
    false ->
      {{error, user_not_joined}, St}
  end;

loop(St, {send_msg, Sender, Msg}) ->
  {Nick, _} = Sender,
  case lists:keymember(Nick, 1, St#ch_st.users) of
    true ->
      Receivers = lists:delete(Sender, St#ch_st.users),
      sendToUsers(Receivers, {incoming_msg, St#ch_st.name, Nick, Msg}),
      {ok, St};
    false ->
      {{error, user_not_joined}, St}
  end.

%% Sends a message to all the users in the list.
sendToUsers([], _) ->
  ok;

sendToUsers([ {_, UserPid} | T ], Msg) ->
  UserPid ! {request, self(), async, Msg},
  sendToUsers(T, Msg).

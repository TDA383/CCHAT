-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

main(State) ->
  receive
    {request, From, Ref, Request} ->
      {Response, NextState} = loop(State, Request),
      From ! {result, Ref, Response},
      main(NextState)
  end.

initial_state(ServerName) ->
  #server_st{name = ServerName}.

loop(St, {connect, User}) ->
  {UserName, _} = User,
  case lists:keymember(UserName, 1, St#server_st.users) of
    false  ->
      NewState = St#server_st{users = [ User | St#server_st.users]},
      {ok, NewState};
    true ->
      {{error, user_already_connected}, St}
  end;

%% TODO: Fix a bug where the user can disconnect before leaving chat rooms.
loop(St, {disconnect, User}) ->
  {UserName, _} = User,
  case lists:keymember(UserName, 1, St#server_st.users) of
    true  ->
      case isInAChannel(User, St#server_st.channels) of
        true ->
          {{error, leave_channels_first}, St};
        false ->
          NewState = #server_st{users = lists:delete(User,
            St#server_st.users)},
          {ok, NewState}
      end;
    false ->
      {{error, user_not_connected}, St}
  end;

loop(St, {join, User, Channel}) ->
  case lists:member(User, St#server_st.users) of
    true ->
      {UserName, _} = User,
      case lists:keytake(Channel, 1, St#server_st.channels) of
        {value, {_, UsersFound}, NewChannelList} ->
          case lists:keymember(UserName, 1, UsersFound) of
            false ->
              NewState = St#server_st{channels
                = [ {Channel, [ User | UsersFound ]} | NewChannelList]},
              {ok, NewState};
            true ->
              {{error, user_already_joined}, St}
          end;
        false ->
          NewState = St#server_st{channels
            = [ {Channel, [User]} | St#server_st.channels ]},
          {ok, NewState}
      end;
    false ->
      {{error, user_not_connected}, St}
  end;

loop(St, {leave, User, Channel}) ->
  {UserName, _} = User,
  case lists:keytake(Channel, 1, St#server_st.channels) of
    {value, {_, UsersFound}, NewList} ->
      case lists:keymember(UserName, 1, UsersFound) of
        true ->
          NewState = St#server_st{channels
            = [ {Channel, lists:delete(User, UsersFound) } | NewList]},
          {ok, NewState};
        false ->
          {{error, user_not_joined}, St}
      end;
    false ->
      {{error, user_not_joined}, St}
  end;

loop(St, {send_msg, User, Channel, Msg}) ->
  {UserName, _} = User,
  case lists:keyfind(Channel, 1, St#server_st.channels) of
    {_, UsersFound} ->
      case lists:keytake(UserName, 1, UsersFound) of
        {_, _, Receivers} ->
          sendToUsers(Receivers,
            {incoming_msg, Channel, UserName, Msg}),
          {ok, St};
        false ->
          io:format("User ~p not joined~n", [UserName]),
          io:format("~p~n", [UsersFound]),
          {{error, user_not_joined}, St}
      end;
    false ->
      {{error, user_not_joined}, St}
  end.

%% Looks if the user is in any channel in the list.
isInAChannel(_, []) ->
  false;

isInAChannel(User, [ {_, FirstUsers} | T]) ->
  case lists:member(User, FirstUsers) of
    true ->
      true;
    false ->
      isInAChannel(User, T)
  end.

%% Sends a message to all the users in the list.
sendToUsers([], _) ->
  ok;

sendToUsers([ {_, UserPid} | T ], Msg) ->
  UserPid ! {request, self(), async, Msg},
  sendToUsers(T, Msg).

-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

main(State) ->
  receive
    {request, From, Request} ->
      {Response, NextState} = loop(State, Request),
      From ! Response,
      main(NextState)
  end.

initial_state(ServerName) ->
  #server_st{name = ServerName}.

%% TODO: Fix error when trying to join chat rooms before connecting.
loop(St, {connect, User}) ->  
  case lists:member(User, St#server_st.users) of
    false  ->
      NewState = St#server_st{users = [ User | St#server_st.users]},
      printUsers(NewState#server_st.users),
      {ok, NewState};
    true ->
      {{error, user_already_connected}, St}
  end;

%% TODO: Fix a bug where the user can disconnect before leaving chat rooms.
loop(St, {disconnect, User}) ->
  case lists:member(User, St#server_st.users) of
    true  ->
      case isInAChannel(User, St#server_st.channels) of
        true -> 
          {error, leave_channels_first};
        false ->            
          NewState = #server_st{users = lists:delete(User,
            St#server_st.users)},
          printUsers(NewState#server_st.users),
          {ok, NewState}
      end;
    false ->
      {{error, user_not_connected}, St}
  end;

loop(St, {join, User, Channel}) ->
  {UserName, _} = User,    
  case findAndRemoveChannel(Channel, St#server_st.channels) of
    {{_, UsersFound}, NewList} ->
      case lists:member(User, UsersFound) of
        false ->         
          sendToUsers(UsersFound,
            {incoming_msg, Channel, UserName,
            "*"++UserName++" joined the room*"}),            
          NewState = St#server_st{channels
            = [ {Channel, [ User | UsersFound ]} | NewList]},
          {ok, NewState};
        true ->
          {{error, user_already_joined}, St}
      end;
    false ->
      NewState = St#server_st{channels
        = [ {Channel, [User]} | St#server_st.channels ]},
      {ok, NewState}
  end;

loop(St, {leave, User, Channel}) ->
  {UserName, _} = User,    
  case findAndRemoveChannel(Channel, St#server_st.channels) of
    {{_, UsersFound}, NewList} ->
      case lists:member(User, UsersFound) of
        true ->
          sendToUsers(UsersFound,
            {incoming_msg, Channel, UserName,
            "*"++UserName++" left the room*"}),           
          NewState = St#server_st{channels
            = [ {Channel, lists:delete(User, UsersFound) } | NewList]},
          {ok, NewState};
        false ->
          {{error, user_not_joined}, St}
      end;
    false ->
      {{error, user_not_joined}, St}
  end;

loop(St, {send, User, Channel, Msg}) ->
  {UserName, _} = User,  
  case findAndRemoveChannel(Channel, St#server_st.channels) of
    {{_, UsersFound}, _} ->
      case lists:member(User, UsersFound) of
        true ->
          sendToUsers(lists:delete(User, UsersFound),
            {incoming_msg, Channel, UserName, Msg}),
          {ok, St};
        false ->
          {{error, user_not_joined}, St}
      end;
    false ->
      {{error, user_not_joined}, St}
  end.

%% Looks for a channel in a list of tuples, where the tuples contains name of a
%% channel and its connected users. If it fails, false is returned. If it
%% succeeds, then a tuple consisting the found tuple and the new list is
%% returned.
findAndRemoveChannel(_, []) ->
  false;

findAndRemoveChannel(ChannelSearch, [{FirstChannel, FirstUsers} | T]) ->
  case FirstChannel =:= ChannelSearch of
    true ->
      {{FirstChannel, FirstUsers}, T};
    false ->
      Result = findAndRemoveChannel(ChannelSearch, T),
      case Result of
        {{ChannelFound, UsersFound}, NewList} ->
          {{ChannelFound, UsersFound},
            [ {FirstChannel, FirstUsers} | NewList ]};
        false ->
          false
      end
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
  UserPid ! {request, self(), ref, Msg},
  sendToUsers(T, Msg).

%% Prints the list of users in the console.
printUsers([]) -> 
  ok;printUsers([User | T]) -> 
  io:format("User: ~p~n", [User]),
  printUsers(T).

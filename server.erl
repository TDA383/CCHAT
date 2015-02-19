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

loop(St, {connect, User}) ->
  case lists:member(User, St#server_st.users) of
    false  ->
      NewState = St#server_st{users = [ User | St#server_st.users]},
      {ok, NewState};
    true ->
      {{error, user_already_connected}, St}
  end;

loop(St, {disconnect, User}) ->
  case lists:member(User, St#server_st.users) of
    true  ->
      NewState = #server_st{users = lists:delete(User, St#server_st.users)},
      {ok, NewState};
    false ->
      {{error, user_not_connected}, St}
  end;

loop(St, {join, User, Channel}) ->
  case findAndRemoveChannel(Channel, St#server_st.channels) of
    {{_, UsersFound}, NewList} ->
      case lists:member(User, UsersFound) of
        false ->
          NewState = St#server_st{channels
            = [ {Channel, [ User | UsersFound ]} | NewList]},
          {ok, NewState};
        true ->
          {error, user_already_joined}
      end;
    false ->
      NewState = St#server_st{channels
        = [ {Channel, [User]} | St#server_st.channels ]},
      {ok, NewState}
  end;

loop(St, {leave, User, Channel}) ->
  case findAndRemoveChannel(Channel, St#server_st.channels) of
    {{_, UsersFound}, NewList} ->
      case lists:member(User, UsersFound) of
        true ->
          NewState = St#server_st{channels
            = [ {Channel, lists:delete(User, UsersFound) } | NewList]},
          {ok, NewState};
        false ->
          {error, user_not_joined}
      end;
    false ->
      {error, user_not_joined}
  end;

loop(St, {msg_from_GUI, User, Channel, Msg}) ->
  case findAndRemoveChannel(Channel, St#server_st.channels) of
    {{_, UsersFound}, NewList} ->
      case lists:member(User, UsersFound) of
        true ->
          NewState = St#server_st{channels
            = [ {Channel, lists:delete(User, UsersFound) } | NewList]},
          {ok, NewState};
        false ->
          {error, user_not_joined}
      end;
    false ->
      {error, user_not_joined}
  end;

loop(St, {has_joined, User, Channel}) ->
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

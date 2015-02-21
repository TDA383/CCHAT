-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

%% Receives messages from the client and handles them accordingly.
main(State) ->
  receive
    {request, From, Ref, Request} ->
      case Request of
        {connect,_} ->
          {Response, NextState} = loop(State, Request),
          From ! {result, Ref, Response};
        {disconnect,_} ->
          {Response, NextState} = loop(State, Request),
          From ! {result, Ref, Response};
        _ ->
          {_, NextState} = loop(State, Ref, Request)
        end,
      main(NextState)
  end.
%% Produces the initial state.
initial_state(ServerName) ->
  #server_st{name = ServerName}.

%% ---------------------------------------------------------------------------

%% Loop handles each kind of request from a client.

%% Connects the client and updates the server state, if not the client is
%% already a member.
loop(St, {connect, User}) ->
  case lists:member(User, St#server_st.users) of
    false  ->
      NewState = St#server_st{users = [ User | St#server_st.users]},
      {ok, NewState};
    true ->
      {{error, user_already_connected}, St}
  end;

%% Disconnects the client and updates the server state, if the client
%% is disconnected from all channels.
loop(St, {disconnect, User}) ->
  case lists:member(User, St#server_st.users) of
    true ->
      case isInAChannel(User, St#server_st.channels) of
        false ->
          NewState = St#server_st{users = lists:delete(User,
            St#server_st.users)},
          {ok, NewState};
        true ->
          {{error, leave_channels_first}, St}
      end;
    false ->
      {{error, user_not_connected}, St}
  end.

%% Connects the client to the specified Channel by sending request to the
%% channel and updates the server state. A new channel is created if the
%%% specified channel doesn't exist.
loop(St, Ref, {join, User, Channel}) ->
  {_, Pid} = User,
  case lists:member(User, St#server_st.users) of
    true ->
      ChannelAtom = list_to_atom(Channel),
      case lists:member(ChannelAtom, registered()) of
        false ->
          helper:start(ChannelAtom, channel:initial_state(Channel),
            fun channel:main/1),
          ChannelAtom ! {request, Pid, Ref, {join, User}},
          NewState = St#server_st{channels =
            [ChannelAtom | St#server_st.channels]},
          {ok, NewState};
        true ->
          ChannelAtom ! {request, Pid, Ref, {join, User}},
          {ok, St}
      end;
    false ->
      Pid ! {result, Ref, {error, user_not_connected}},
      {error, St}
  end;

%% Disconnects the client from the specified Channel by sending a request to
%% the channel and updates the server state, if and only if the client is
%% already connected to the channel.
loop(St, Ref, {leave, User, Channel}) ->
  {_, Pid} = User,
  case lists:member(User, St#server_st.users) of
    true ->
      ChannelAtom = list_to_atom(Channel),
      case lists:member(ChannelAtom, registered()) of
        true ->
          ChannelAtom ! {request, Pid, Ref, {leave, User}},
          {ok, St};
        false ->
          Pid ! {result, Ref, {error, user_not_joined}},
          {error, St}
      end;
    false ->
      Pid ! {result, Ref, {error, user_not_connected}},
      {error, St}
  end;

%% Sends a request to the channel to send a message to all other members of
%% the channel.
loop(St, Ref, {send_msg, User, Channel, Msg}) ->
  {_, Pid} = User,
  case lists:member(User, St#server_st.users) of
    true ->
      ChannelAtom = list_to_atom(Channel),
      case lists:member(ChannelAtom, registered()) of
        true ->
          ChannelAtom ! {request, Pid, Ref, {send_msg, User, Msg}},
          {ok, St};
        false ->
          Pid ! {result, Ref, {error, user_not_joined}},
          {error, St}
      end;
    false ->
      Pid ! {result, Ref, {error, user_not_connected}},
      {error, St}
  end.

%% Determines whether a user is connected to one or more channels, or not.
isInAChannel(_, []) ->
  false;

isInAChannel(User, [ChannelAtom | T]) ->
  Result = helper:request(ChannelAtom, {member, User}),
  case Result of
    true ->
      true;
    false ->
      isInAChannel(User, T)
  end.

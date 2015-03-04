-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receives messages from GUI and the server, and handles them accordingly.
main(State) ->
  receive
    {request, From, Ref, Request} ->
      {Response, NextState} = loop(State, Request),
      From ! {result, Ref, Response},
      main(NextState)
  end.

%% Produce initial state.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick }.

%% ---------------------------------------------------------------------------

%% Loop handles each kind of request from GUI and server.

%% Connect to server
loop(St, {connect, Server}) ->
  NewState = St#cl_st{server = list_to_atom(Server)},
  serverRequest(NewState, {connect, user(St)});

%% Disconnect from server.
loop(St, disconnect) ->
  case St#cl_st.server of
    disconnected ->
      errorMessage({error, user_not_connected}, St);
    _ ->
      Response = serverRequest(St, {disconnect, user(St)}),
      case Response of
        {ok, _} ->
          {ok, St#cl_st{server = disconnected}};
        Error ->
          Error
      end
  end;

% Join channel.
loop(St, {join, Channel}) ->
  serverRequest(St, {join, user(St), Channel});

%% Leaves channel by sending request directly to the channel, to prevent
%% bottle-necking in the server.
loop(St, {leave, Channel}) ->
  channelRequest(St, Channel, {leave, user(St)});

% Sending messages.
loop(St, {msg_from_GUI, Channel, Msg}) ->
  channelRequest(St, Channel, {send_msg, user(St), Msg});

% Receives ping from user.
loop(St, {ping_from_user, From, TimeStamp}) ->
  {Pid, Nick} = From,
  helper:requestAsync(Pid, {pong, Nick, TimeStamp}),
  {ok, St};

% Pings another user.
loop(St, {ping, Nick}) ->
  serverRequest(St, {ping, Nick, now()});

% Receives pong from the pinged user.
loop(St, {pong, Nick, TimeStamp}) ->
  Diff = helper:timeSince(TimeStamp),
  gen_server:call(list_to_atom(St#cl_st.gui),
    {msg_to_SYSTEM, io_lib:format("Pong ~s: ~pms", [Nick,Diff])}),
  {ok, St};

%% Get current nick.
loop(St, whoami) ->
  {St#cl_st.nick, St};

%% Change nick.
loop(St, {nick, Nick}) ->
  case St#cl_st.server of
    disconnected ->
      NewState = St#cl_st{nick = Nick},
      {ok, NewState};
    _ ->
      errorMessage({error, user_already_connected}, St)
  end;

%% Incoming message from server.
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = MsgFromClient,
    gen_server:call(list_to_atom(GUIName),
      {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

%% Returns a tuple of the clients user name and process id.
user(St) ->
  {St#cl_st.nick, self()}.

%% Sends a request to the current server.
serverRequest(St, Request) ->
  ServerAtom = St#cl_st.server,
  case lists:member(ServerAtom, registered()) of
    true ->
      try helper:request(ServerAtom, Request) of
        ok ->
          {ok, St};
        Error ->
          errorMessage(Error, St)
      catch
        exit:"Timeout" ->
          errorMessage({error, server_not_reached}, St)
      end;
    false ->
      errorMessage({error, server_not_reached}, St)
  end.

%% Sends a request to a given channel, if the user is already connected to the
%% server.
channelRequest(St, Channel, Request) ->
  case St#cl_st.server of
    disconnected ->
      errorMessage({error, user_not_connected}, St);
    _ ->
      ChannelAtom = list_to_atom(Channel),
      case lists:member(ChannelAtom, registered()) of
        true ->
          try helper:request(ChannelAtom, Request) of
            ok ->
              {ok, St};
            Error ->
              errorMessage(Error, St)
          catch
            exit:"Timeout" ->
              errorMessage({error, server_not_reached}, St)
          end;
        false ->
          errorMessage({error, user_not_joined}, St)
      end
  end.

%% Returns the error message corresponding to the error atom.
errorMessage(Error, St) ->
  case Error of
    {error, user_not_connected} ->
      {{error, user_not_connected,
        "You are not connected to the server!"},
        St#cl_st{server = disconnected}};
    {error, user_already_connected} ->
      {{error, user_already_connected,
        "You are already connected!"}, St};
    {error, user_not_found} ->
      {{error, user_not_found,
        "The user is not connected to the server!"}, St};
    {error, leave_channels_first} ->
      {{error, leave_channels_first,
        "Leave all channels before disconnecting!"}, St};
    {error, user_not_joined} ->
      {{error, user_not_joined, "You haven't joined the chat room!"}, St};
    {error, user_already_joined} ->
      {{error, user_already_joined, "You have already joined!"}, St};
    {error, server_not_reached} ->
      {{error, server_not_reached,
        "The server could not be reached!"},
        St#cl_st{server = disconnected}}
  end.

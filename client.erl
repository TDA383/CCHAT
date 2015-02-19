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

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->
  NewState = St#cl_st{server = Server},
  serverRequest(NewState, {connect, user(St)});

%% Disconnect from server
loop(St, disconnect) ->
  case St#cl_st.server of
    undefined ->
      errorMessage({error, user_not_connected}, St);
    _ ->
      Response = serverRequest(St, {disconnect, user(St)}),
      case Response of
        {ok, _} ->
          {ok, St#cl_st{server = undefined}};
        Error ->
          Error
      end
  end;

% Join channel
loop(St, {join, Channel}) ->
  serverRequest(St, {join, user(St), Channel});

%% Leave channel
loop(St, {leave, Channel}) ->
  serverRequest(St, {leave, user(St), Channel});

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
  serverRequest(St, {send_msg, user(St), Channel, Msg});

%% Get current nick
loop(St, whoami) ->
  {St#cl_st.nick, St};

%% Change nick
loop(St, {nick, Nick}) ->
  case St#cl_st.server of
    undefined ->
      NewState = St#cl_st{nick = Nick},
      {ok, NewState};
    _ ->
      errorMessage({error, user_already_connected}, St)
  end;

%% Incoming message
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = MsgFromClient,
    gen_server:call(list_to_atom(GUIName),
      {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

%% Returns the user tuple for the server.
user(St) ->
  {St#cl_st.nick, self()}.

%% Sends a request to the current server. If it fails to connect, it returns
%% {error, user_not_connected}. If it succeeds, it will either return 'ok' or
%% the error message received from the server. If no message is received after
%% 3 seconds, error 'server_not_reached' will be returned.
serverRequest(St, Request) ->
  Server = St#cl_st.server,
  ServerAtom = list_to_atom(Server),
  case lists:member(ServerAtom, registered()) of
    true ->
      Ref = make_ref(),
      ServerAtom ! {request, self(), Ref, Request},
      receive
        {result, Ref, ok} ->
          {ok, St};
        {result, Ref, {error, Error}} ->
          errorMessage({error, Error}, St)
      after 3000 ->
        errorMessage({error, server_not_reached}, St)
      end;
    false ->
      errorMessage({error, server_not_reached}, St)
  end.

%% Returns the error message corresponding to the error atom.
errorMessage(Error, St) ->
  case Error of
    {error, user_not_connected} ->
      {{error, user_not_connected,
        "You are not connected to the server!"},
        St#cl_st{server = undefined}};
    {error, user_already_connected} ->
      {{error, user_already_connected,
        "You are already connected!"}, St};
    {error, leave_channels_first} ->
      {{error, leave_channels_first,
        "Leave all channels before disconnecting!"}, St};
    {error, user_not_joined} ->
      {{error, user_not_joined, "You haven't joined the chat room!"}, St};
    {error, user_already_joined} ->
      {{error, user_already_joined, "You have already joined!"}, St};
    {error, server_not_reached} ->
      {{error, server_not_reached,
        "The server could not be reached, you have been disconnected!"},
        St#cl_st{server = undefined}}
  end.

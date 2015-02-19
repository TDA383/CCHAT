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
    serverRequest(NewState, {connect, {St#cl_st.nick, self()}}),
  receive
    ok ->
      {ok, NewState};
    {error, user_already_connected} ->
      {{error, user_already_connected, "You are already connected!"}, St}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

%% Disconnect from server
loop(St, disconnect) ->
  NewState = St#cl_st{server = undefined},
  serverRequest(St, {disconnect, {St#cl_st.nick, self()}}),
  receive
    ok ->
      {ok, NewState};
    {error, user_not_connected} ->
      {{error, user_not_connected, "You are not connected to the server!"}, St};
    {error, leave_channels_first} ->
      {{error, leave_channels_first}, "Leave all channels before disconnecting!"}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

% Join channel
loop(St, {join, Channel}) ->
  serverRequest(St, {join, {St#cl_st.nick, self()}, Channel}),
  receive
    ok ->
      {ok, St};
    {error, user_already_joined} ->
      {{error, user_already_joined, "You have already joined!"}, St}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

%% Leave channel
loop(St, {leave, Channel}) ->
  serverRequest(St, {leave, {St#cl_st.nick, self()}, Channel}),
  receive
    ok ->
      {ok, St};
    {error, user_not_joined} ->
      {{error, user_not_joined, "You haven't joined the chat room!"}, St}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
  serverRequest(St, {send,{St#cl_st.nick, self()}, Channel, Msg}),
  receive
    ok ->
      {ok, St};
    {error, user_not_joined} ->
      {{error, user_not_joined, "You haven't joined the chat room!"}, St}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

%% Get current nick
loop(St, whoami) ->
  {St#cl_st.nick, St};

%% Change nick
loop(St, {nick, Nick}) ->
  NewState = St#cl_st{nick = Nick},
  {ok, NewState};   

%% Incoming message
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = MsgFromClient,
    gen_server:call(list_to_atom(GUIName),
      {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

%% Sends a request to the current server. If it fails, it returns false.
serverRequest(#cl_st{server=Server}, Request) ->
  ServerAtom = list_to_atom(Server),
  case lists:member(ServerAtom, registered()) of
    true -> 
      ServerAtom ! {request, self(), Request};
    false ->
      false
  end;

serverRequest(_, _) ->
  false.

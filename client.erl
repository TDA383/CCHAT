-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receive messages from GUI and handle them accordingly
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
  request(NewState, {connect, St#cl_st.nick}),
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
  request(St, {disconnect, St#cl_st.nick}),
  receive
    ok ->
      {ok, NewState};
    {error, user_not_connected} ->
      {{error, user_not_connected, "You are not connected to the server!"}, St}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

% Join channel
loop(St, {join, Channel}) ->
  request(St, {join, St#cl_st.nick, Channel}),
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
  request(St, {leave, St#cl_st.nick, Channel}),
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
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St, whoami) ->
    % {"nick", St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Change nick
loop(St, {nick, Nick}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
loop(St = #cl_st { gui = GUIName }, MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = MsgFromClient,
    gen_server:call(list_to_atom(GUIName),
      {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

%% Sends a request to the current server. If it fails, it returns false.
request(#cl_st{server=Server}, Request) ->
  ServerAtom = list_to_atom(Server),
  ServerAtom ! {request, self(), Request};

request(_, _) ->
  false.

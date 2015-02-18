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
  ServerAtom = list_to_atom(Server),  
  ServerAtom ! {request, self(), {connect, St#cl_st.nick}},
  receive
    ok ->
      NewState = St#cl_st{server = Server},
      {ok, NewState};
    {error, user_already_connected} ->
      {{error, user_already_connected, "You are already connected!"}, St}
  after 3000 ->
    {{error, server_not_reached, "The server could not be reached!"}, St}
  end;

%% Disconnect from server
loop(St, disconnect) ->
  case St#cl_st.connected_channels of
    [] ->
      ServerAtom = list_to_atom(St#cl_st.server),  
      ServerAtom ! {request, self(), {disconnect, St#cl_st.nick}},
      receive
        ok ->
          {ok, St};
        {error, user_not_connected} ->
          {error, user_not_connected, "You are not connected to the server!"}
      after 3000 -> 
        {{error, server_not_reached, "The server could not be reached!"}, St}
      end;
    _ ->
      {{error, leave_channels_first,
        "You must leave all chat rooms before disconnecting!"}, St}
  end;

% Join channel
loop(St, {join, Channel}) ->
  case lists:member(Channel, connected_channels) of
    false ->       
      ServerAtom = list_to_atom(St#cl_st.server),  
      ServerAtom ! {request, self(), {join, Channel}},
      receive
        ok ->
          NewState = St#cl_st{connected_channels
                                = [ Channel | St#cl_st.connected_channels] },
          {ok, NewState}
      after 3000 -> 
        {{error, server_not_reached, "The server could not be reached!"}, St}
      end;
    true -> 
      {{error, user_already_joined,
        "You have already joined this chat room!"}, St};

%% Leave channel
loop(St, {leave, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

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
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.

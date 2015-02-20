-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

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

initial_state(ServerName) ->
  #server_st{name = ServerName}.

loop(St, {connect, User}) ->
  {Nick, _} = User,
  case lists:keymember(Nick, 1, St#server_st.users) of
    false  ->
      NewState = St#server_st{users = [ User | St#server_st.users]},
      {ok, NewState};
    true ->
      {{error, user_already_connected}, St}
  end;

loop(St, {disconnect, User}) ->
  {Nick, _} = User,
  case lists:keymember(Nick, 1, St#server_st.users) of
    true  ->      
      case isInAChannel(User, St#server_st.channels) of
        false ->
          NewState = #server_st{users = lists:delete(User,
            St#server_st.users)},
          {ok, NewState};
        true ->
          {{error, leave_channels_first}, St}
      end;
    false ->
      {{error, user_not_connected}, St}
  end.

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

isInAChannel(_, []) -> 
  false;

isInAChannel(User, [ChannelAtom | T]) -> 
  Ref = make_ref(),
  ChannelAtom ! {request, self(), Ref, {member, User}},
  receive
    true ->
      true;
    false -> 
      isInAChannel(User, T)
  end.
    

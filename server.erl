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
  end.

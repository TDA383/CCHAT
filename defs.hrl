% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(cl_st, {gui, nick, server, connected_channels = []}).

% This record defines the structure of the server process.
% It contains the following fields:
%
-record(server_st, {name, users = []}).

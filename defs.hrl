% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
-record(cl_st, {gui, nick, server}).

% This record defines the structure of the server process.
% It contains the following fields:
%
-record(server_st, {name, users = [], channels = []}).

% This record defines the structure of the channel process.
% It contains the following fields:
%
-record(ch_st, {name, users = []}).


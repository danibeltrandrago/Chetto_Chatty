-module(client).

-import(lists,[last/1]).

%% Exported Functions
-export([start/2, init_client/2]).

%% API Functions
start(ServerPid, MyName) ->
	ClientPid = spawn(client, init_client, [ServerPid, MyName]),
	process_commands(ServerPid, MyName, ClientPid, "Global").

init_client(ServerPid, MyName) ->
	ServerPid ! {client_join_req, MyName, self()},
	process_requests(MyName).

%% Local Functions
%% This is the background task logic
process_requests(MyName) ->
	receive
		{join, Name} ->
			io:format("[JOIN] ~s joined~n", [Name]),
			process_requests(MyName);
		{leave, Name} ->
			io:format("[EXIT] ~s left~n", [Name]),
			process_requests(MyName);
		{message, Name, Text} ->
			io:format("[~s] ~s", [Name, Text]),
			process_requests(MyName);
		{message_private, Name, Text} ->
			io:format("<~s> ~s", [Name, Text]),
			process_requests(MyName);
		{message_group, Name, Text} ->
			io:format("(~s) ~s", [Name, Text]),
			process_requests(MyName);


		{change_server, Servers} ->
			if length(Servers) > 0 ->
					io:format("El servidor se ha desconectado.\nReconectando con otro servidor...\n"),
					NewServer = last(Servers),
					start(NewServer, MyName),
					ok;
				true ->
					io:format("El servidor se ha desconectado y no existen mas servidores.\nEscribe 'exit' para salir.\n")
			end;
		exit ->
			io:format("El servidor te ha expulsado del canal, Introduce 'exit' para salir.\n"),
			ok
	end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid, Group) ->
	%% Read from the standard input and send to server
	Text = io:get_line("-> "),
	if  Text == "exit\n" ->
			ServerPid ! {client_leave_req, MyName, ClientPid, Group},
			ok;

		Text == "All\n" ->
			Message = io:get_line("Mensaje general\n-> "),
			ServerPid ! {send, MyName, Message},
			process_commands(ServerPid, MyName, ClientPid, Group);
		
		Text == "private\n" ->
			Aux = io:get_line("Nombre del usuario: "),
			Name = re:replace(Aux, "\\s+", "", [global,{return,list}]),
			Message = io:get_line("Mensaje privado\n-> "),
			ServerPid ! {set_private_send, MyName, ClientPid, Name, Message},
			process_commands(ServerPid, MyName, ClientPid, Group);

		Text == "Change_group\n" ->
			Aux = io:get_line("Introduce el nombre del grupo: "),
			NameGroup = re:replace(Aux, "\\s+", "", [global,{return,list}]),
			ServerPid ! {change_group, MyName, ClientPid, Group, NameGroup},
			process_commands(ServerPid, MyName, ClientPid, NameGroup);

		true ->
			ServerPid ! {send_group, MyName, Group, Text},
			process_commands(ServerPid, MyName, ClientPid, Group)
	end.

-module(server2).

%% Exported Functions
-export([start/0, start/1, init_server/0, init_server/1, process_requests/2]).

%% API Functions
start() ->
	ServerPid = spawn(server2, init_server, []),
	register(myserver, ServerPid),
	process_commands(ServerPid).

start(BootServer) ->
	ServerPid = spawn(server2, init_server, [BootServer]),
	register(myserver, ServerPid),
	process_commands(ServerPid).

init_server() ->
	process_requests([], [self()]).

init_server(BootServer) ->
	BootServer ! {server_join_req, self()},
	process_requests([], []).

process_requests(Clients, Servers) ->
	receive
		%% Messages between client and server
		% A new client joins
		{client_join_req, Name, From} ->
			io:format("[JOIN] ~s~n", [Name]),
			NewClients = [{From, Name}|Clients],
			broadcast_server(Servers, {join, Name}),
			process_requests(NewClients, Servers);
		% A client leaves
		{client_leave_req, Name, From} ->
			io:format("[EXIT] ~s~n", [Name]),
			NewClients = lists:delete({From, Name}, Clients),
			broadcast_server(Servers, {leave, Name}),
			From ! exit,
			process_requests(NewClients, Servers);
		% A client sends a message
		{send, Name, Text} ->
			io:format("~p: ~s", [Name, Text]),
			broadcast_server(Servers, {message, Name, Text}),
			process_requests(Clients, Servers);


		{request_list_users, From} ->
			broadcast_server(Servers, {send_list_of_users, From}),
			process_requests(Clients, Servers);

		{send_list_of_users, From} ->
			From ! {print_users, Clients},
			process_requests(Clients, Servers);

		{print_users, List_of_clients} ->
			print_name(List_of_clients),
			process_requests(Clients, Servers);

		remove_users ->
			remove_user(Clients),
			io:format("All Clients removed~n"),
			process_requests([], Servers);

		%% Messages between servers
		% Stop the server
		disconnect ->
			NewServers = lists:delete(self(), Servers),
			io:format("Redireccionant clients!~n"),
			broadcast(Clients, {change_server, NewServers}),
			broadcast_server(NewServers, {update_servers, NewServers}),
			io:format("Bye!~n"),
			unregister(myserver);
		% Server joins
		{server_join_req, From} ->
			io:format("New server joins!~n"),
			NewServers = [From|Servers],
			broadcast_server(NewServers, {update_servers, NewServers}),
			process_requests(Clients, NewServers);
		% Server list update
		{update_servers, NewServers} ->
			io:format("Server list update! ~w~n", [NewServers]),
			process_requests(Clients, NewServers);
		% Other messages are relayed to clients
		RelayMessage ->
			io:format("Relaying message...~n"),
			broadcast(Clients, RelayMessage),
			process_requests(Clients, Servers)

	end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> 
		X = element(1, Peer),
		X ! Message 
	end,
	lists:map(Fun, PeerList).

%% Local Functions
broadcast_server(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).


%% This is the main task logic
process_commands(ServerPid) ->
	%% Read from the standard input and send to server
	Text = io:get_line("-> "),
    if Text == "List_clients\n" ->
        ServerPid ! {request_list_users, ServerPid},
        process_commands(ServerPid);

       Text == "Remove_clients\n" ->
       	ServerPid ! remove_users,
       	process_commands(ServerPid);
       
       Text == "exit\n" ->
       	ServerPid ! disconnect;
       
       true ->
        io:format("Este comando no existe, Introduce 'help' para ver las opciones.~n"),
        process_commands(ServerPid)
    end.
	

print_name(Clients) ->
	P = fun(Client) ->
			Name = element(2, Client),
			io:format("~s~n", [Name])
		end,
	lists:map(P, Clients).

remove_user(Clients) ->
	io:format("Removing clients...~n"),
	P = fun(Client) ->
			Direction = element(1, Client),
			Direction ! exit
		end,
	lists:map(P, Clients).

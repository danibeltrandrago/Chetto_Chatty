-module(server2).

%% Exported Functions
-export([start/0, start/1, init_server/0, init_server/1, process_requests/3]).

%% API Functions
start() ->
	ServerPid = spawn(server2, init_server, []),
	register(myserver, ServerPid),
	process_commands(ServerPid).

start(BootServer) ->
	ServerPid = spawn(server2, init_server, [BootServer]),
	register(myserver, ServerPid).

init_server() ->
	process_requests([], [self()], []).

init_server(BootServer) ->
	BootServer ! {server_join_req, self()},
	process_requests([], [], []).

process_requests(Clients, Servers, NameClients) ->
	receive
		%% Messages between client and server
		% A new client joins
		{client_join_req, Name, From} ->
			io:format("[JOIN] ~s~n", [Name]),
			NewClients = [From|Clients],
			NewNameClients = [Name|NameClients],
			broadcast(Servers, {join, Name}),
			process_requests(NewClients, Servers, NewNameClients);
		% A client leaves
		{client_leave_req, Name, From} ->
			io:format("[EXIT] ~s~n", [Name]),
			NewClients = lists:delete(From, Clients),
			NewNameClients = lists:delete(Name, NameClients),
			broadcast(Servers, {leave, Name}),
			From ! exit,
			process_requests(NewClients, Servers, NewNameClients);
		% A client sends a message
		{send, Name, Text} ->
			io:format("~p: ~s", [Name, Text]),
			broadcast(Servers, {message, Name, Text}),
			process_requests(Clients, Servers, NameClients);

		print_users ->
			even_print(NameClients),
			process_requests(Clients, Servers, NameClients);

		remove_users ->
			remove_user(Clients),
			io:format("All Clients removed~n"),
			process_requests([], Servers, []);

		%% Messages between servers
		% Stop the server
		disconnect ->
			io:format("Bye!~n"),
			NewServers = lists:delete(self(), Servers),
			broadcast(NewServers, {update_servers, NewServers}),
			unregister(myserver);
		% Server joins
		{server_join_req, From} ->
			io:format("New server joins!~n"),
			NewServers = [From|Servers],
			broadcast(NewServers, {update_servers, NewServers}),
			process_requests(Clients, NewServers, NameClients);
		% Server list update
		{update_servers, NewServers} ->
			io:format("Server list update! ~w~n", [NewServers]),
			process_requests(Clients, NewServers, NameClients);
		% Other messages are relayed to clients
		RelayMessage ->
			io:format("Relaying message...~n"),
			broadcast(Clients, RelayMessage),
			process_requests(Clients, Servers, NameClients)

	end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).


%% This is the main task logic
process_commands(ServerPid) ->
	%% Read from the standard input and send to server
	Text = io:get_line("-> "),
    if Text == "List_clients\n" ->
        ServerPid ! print_users;

       Text == "Remove_all\n" ->
       	io:format("Removing clients...~n"),
       	ServerPid ! remove_users;
       
       Text == "exit\n" ->
       	io:format("Exiting...~n"),
       	ServerPid ! disconnect;
       
       true ->
        io:format("Este comando no existe, Introduce 'help' para ver las opciones.~n")
    end,
	process_commands(ServerPid).


even_print([])-> [];
even_print([H|T]) when H rem 2 /= 0 ->
    even_print(T);
even_print([H|T]) ->
    io:format("~s~n", [H]),
    [H|even_print(T)].

remove_user(Clients) ->
	P = fun(Client) ->
			io:format("[Removed]~n"),
			Client ! exit
		end,
	lists:map(P, Clients).

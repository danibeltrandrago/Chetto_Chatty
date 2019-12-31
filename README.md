# Chetto_Chatty

## utilización

Iniliciación de un nodo de erlang:
> erl -name node@127.0.0.1 -setcookie secret


Compilación y arranque del servidor:
```erlang
c(server2).
server2:start().
```
Arrancar un servidor unido al primero:
```erlang
c(server2).
server2:start({myserver, 'server_node@127.0.0.1'}).
```

Compilación y arranque de un clients:
```erlang
c(client).
client:start({myserver, 'server_node@127.0.0.1'}, "Name").
```

## Funcionalidades del servidor
##### - List_clients
Imprime una lista de todos los clienes conectados entre los distintos servidores.
##### - List_groups
imprime todos los grupos creados y cada uno de los usuarios que hay dentro de ellos.
##### - Remove_all_clients
Elimina todos los clientes del servidor en concreto.
##### - Remove_client
Elimina a un cliente en concreto.
##### - exit
Redirige a los usuarios a otro servidor disponible.

## Funcionalidades del cliente
##### - All
Envia el mensaje deseado a todos los clientes independiente-mente del gripo en el que esten.
##### - private
Envia un mensaje privado a un solo cliente.
##### - Change_group
Cambia el cliente de grupo o crea un nuevo grupo en el caso de que el grupo no este creado.

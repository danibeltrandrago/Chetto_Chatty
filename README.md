# Chetto_Chatty

## Utilicación

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


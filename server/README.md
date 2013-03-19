# Genetica

Genetica is probably one of the most overengineered genetic algorithm systems
today: It's more of a service/server than an application you start. You connect
to it with the Genetica client, [Ninshubur][], and request runs from it from there
by specifying hostname and the port Genetica is setup on.

## Internal design

The internal design of Genetica looks like this:

    genetica_app
    -> genetica_sup
       -> genetica_tcp_handler
       -> genetica_client_sup
          -> genetica_single_sup (multiple)
             -> genetica_tcp_worker
             -> genetica_compute_worker

### `genetica_app`

`genetica_app` uses the application behaviour, and is the entry point for
everyone which wants to use this program. `genetica_app` starts up the Genetica
supervisor, `genetica_sup`, which is the root supervisor in Genetica. Note that
Genetica can only setup a single root supervisor, hence it can only listen to
one port as of right now.

### `genetica_sup`

`genetica_sup` supervises two processes: the request handler, which handles and
forwards client hook-ins, and the client supervisor. As the client supervisor
can work by itself, this will not be restarted when the request handler crashes.
However, the request handler has the Pid to the client supervisor, and whenever
the client supervisor is restarted, it will be given a new Pid. Henceforth, the
request handler will be restarted to get the correct Pid.

### `genetica_request_handler`

`genetica_request_handler` handles and forwards connections to Genetica on the
specified port. Whenever a request comes in, it will pass the connection to the
Genetica client supervisor, which will create a new `genetica_single_sup` which
handles the logic and work this request is given.

[Ninshubur]: https://github.com/hyPiRion/ninshubur

# About

cl-sse is an partial implementation of the Server Side Events protocol as per
https://www.w3.org/TR/eventsource/. It is a toolkit for Common Lisp service
implementations that want to send or receive events according to the protocol.
There is a small demo you can try with a browser (instructions below).

The protocol originated for use in browsers, where most browsers support a
javascript EventSource implementation that can be used to receive pushed event
dispatches from a web server.

With appreciation to https://gist.github.com/jareware/aae9748a1873ef8a91e5
for a browser eventsource example.

# Modules

## server.lisp

The server.lisp module is for the push piece of the protocol.
You could probably make do with `format`, but this piece ensures that
protocols are observed if you aren't familiar with SSE, though it does differ
in that it requires an event name/type where the spec consider this optional.

## client.lisp

The client.lisp module is for CL services that want to receive SSE pushes. SSE
clients outside of javascript don't seem to be very common, but it definitely
has uses. For example a server might use SSE to receive job dispatches in a
microservice architecture.  In fact, if you were to use SSE for job
dispatches, browsers could easily be workers.

I was tempted to have the event properties be nil, instead of empty strings,
when they weren't in the event, however I opted instead to follow the spec a
bit more closely, for no particularly good reason. As there are no users of this
code it's something of a moot point, it was mostly for example purposes.

## demo.lisp

Shows SSE in action in a browser.
Use as follows:

1. (ql:quickload :sse-demo)
2. (sse:demo/start-server) ; starts a hunchentoot server (in demo::*http-server*)
3. Connect a browser to localhost:8080/sse and watch the demo unfold.

By connecting to the SSE page, the browser will execute some javascript code
that connects to SSE service on the /events endpoint, and responds to a loop
emitting sse events coming from the server.  After receiving 10 events the
server hangs up, and the browser re-acquires the /events endpoing and it
starts over gain.

4. When you're done, kill your browser tab, and (sse:demo/stop-server) to kill
the web service.

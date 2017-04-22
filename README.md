# CounterDB

## Intro

CounterDB is a distributed database of counters. It's written in Erlang.

## How to Build

To build this project you should have erlang 19.x installed.

Note: It was tested only with Erlang 19.3.

There are two options to build CounterDB.

### Escriptize Bundle

```
counterdb$ ./rebar3 escriptize
```

This command produces `_build/default/bin/counterdb`. It's an executable
zip-bundle that contains all pre-compiled modules. The bundle expects `escript`
to be in `$PATH` on execution.

### Erlang Release

```
counterdb$ ./rebar3 release
```

The result of this command is a directory `_build/default/rel/counterdb`.
It contains Erlang release of the project with BEAM runtime environment.

## Usage

```
counterdb$ _build/default/bin/counterdb <id>
counterdb$ COUNTER_ID=<id> _build/default/rel/counterdb/bin/counterdb console
```

CounterDB creates `data` directory in current working directory and
in release directory for bundle and release respectively. The directory `data`
contains subdirectories for each `<id>` with information about cluster and
storage of counters persistent data. The storage backend is `dets`.

The project doesn't use Erlang's distribution protocol, works without
cookies, and doesn't require anything additional. It uses only HTTP REST API
on port 7777.

CounterDB expects each node in CounterDB's cluster to have an unique positive
number provided as `<id>`. If `<id>`s are not unique, database behavior
is undefined.

You should send list of nodes to each node at the start to initialize
the cluster. This information can be set up only once. When nodes receive
different lists of nodes, behavior is not defined.

### REST API

* Cluster bootstrapping

```
> curl http://1.2.3.4:7777/config -H 'Content-Type: application/json' \
    -X POST -d '{"actors":["1.2.3.4", "1.2.3.5", "1.2.3.6"]}'
< 204
< 400
< 409
```

You should send this request with `Content-Type` header, it's required.

* Get counter value

```
> curl http://1.2.3.4:7777/counter/:name:/value
< 200
:non-negative integer as text:
```

This handle returns the current value of a counter on this server. This value
is not consistent.

* Get counter consistent value

```
> curl http://1.2.3.4:7777/counter/:name:/consistent_value
< 200
:non-negative number as text:
< 408
:non-negative number as text:
```

This handle returns the consistent value of a counter.

The call can take up to 5 minutes. Receiving status code 408 means that
CounterDB can't reach some nodes in the cluster, and the returned value is not
consistent but the best that it has at this moment.

Please read below about how it works and what consistency is.

* Increment counter

```
> curl http://1.2.3.4:7777/counter/:name: \
    -X POST -d ':non-negative integer as text:'
> 200
<non-negative integer as text>
```

By calling this method you increment the current value of a counter
on this server.

## Consistency

Before explaining how it works we should define consistency in CounterDB.

Consistent value of a counter guarantees that:

1. Each request acknowledged by a node in the cluster is taken into account.

2. The consistent value has no guarantee to include any requests sent after
the consistent value request.

## How It Works

To provide the described consistency model CounterDB uses grow-only CRDT
counter a.k.a. g-counter where `<id>` is used as the actor identifier. Each
node can change only its part of each counter. CounterDB has active
anti-entropy mechanism a.k.a. AAE for spreading new changes through
the cluster. AAE processes work on every node and are very naive; they send
whole snapshots of counters' data once in several seconds.

## Consistent Value

When client sends a consistent value request to node `A`, node `A` generates
an unique key `$AAE$-N-M` and saves it to internal storage. AAE process starts
every 30 seconds and tries to send the snapshot to all reachable nodes. When
node `B` receives the snapshot from node `A`, it merges all counters and then
sets its part of counter `$AAEWAIT$-N-M` to 1. Node `A` would be sure that
node `B` processed snapshot `A` when node `B` sends its snapshot `B` via its
AAE process. Node `A` checks `$AAEWAIT$-N-M` up to 5 minutes. If its value
is equal to the number of nodes, it returns consistent value of the counter
to client.

Why CounterDB uses two counters instead of requesting each node to update its
part of counter `$AAEWAIT$-N-M` via increment REST API? Two counters method
works for clusters where some nodes don't have direct connections to others.
The cost of this method is that CounterDB has to wait for the ending of AAE
process longer if all nodes are reachable from node `A`.

## Acknowledgement Method

CounterDB also has another method for consistency. For this method client must
specify numbers of nodes that acknowledge write and read requests.

```
> curl http://1.2.3.4:7777/counter/:name:/value?wait=<R>
< 200
:non-negative integer as text:
< 408
:non-negative integer as text:

> curl http://1.2.3.4:7777/counter/:name:?wait=<W> \
    -X POST -d ':non-negative integer as text:'
> 200
:non-negative integer as text:
< 408
:non-negative integer as text:
```

This method works only if `<R> + <W>` is bigger than the number of nodes. Often
quorum would be a good choice, so CounterDB supports string `quorum` as
a value of `wait` param.

## Another Possible Solution

To solve problem of distributed database of counters it's also possible
to use a consensus protocol, such as Raft, Multi-Paxos, or EPaxos.
Implementation of a consensus protocol would take weeks. Usage of existing
libraries (such as `riak_ensemble`) would not provide evaluation of
development skills.

## How to Test

Writing a distributed system is a complicated task with a lot of edge cases.
CounterDB uses property-based testing to cover them. Naive tests for
both consistency methods are available in `test` directory. The execution
of these tests is very time-consuming.

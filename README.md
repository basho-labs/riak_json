# RiakJson

Riak Json is a JSON based document and query interface built on Riak and indexed by Solr

### Setup

#### Download

```
git clone https://github.com/basho/riak.git
cd riak && git checkout ack-riak-json
```

#### Build

```
make rel
```

#### Configure

Verify that Yokozuna and RiakJson are both turned `on` in `rel/riak/etc/riak.conf`

```
...
yokozuna = on
...
riak_json = on
...
```

#### Start the server

```
./rel/riak/bin/riak start
```

### Test

#### Download

```
git clone https://github.com/basho/riak_json.git
cd riak_json
```

#### Build

```
make
```

#### Unit test

```
make test
```

#### Integration test

```
make itest
```

### Architecture

Please refer to [docs/architecture.md](https://github.com/basho/riak_json/blob/master/docs/architecture.md) for more information.

### API Reference

Please refer to [docs/demo.md](https://github.com/basho/riak_json/blob/master/docs/demo.md) and [docs/query.md](https://github.com/basho/riak_json/blob/master/docs/query.md) for information about how tu use RiakJson and for examples of queries.


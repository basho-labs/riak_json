# RiakJson

Riak Json is a JSON based document and query interface built on Riak and indexed by Solr

### Setup

#### Download Riak Source
See the [Installing Riak From Source](http://docs.basho.com/riak/2.0.0pre5/ops/building/installing/from-source/) 
discussion for Riak pre-requisites, and the [Yokozuna Install Docs](https://github.com/basho/yokozuna/blob/develop/docs/INSTALL.md)
for Yokozuna/Search pre-requisites (specifically, the part about 'Java 1.6 or later, Oracle 7u25 is recommended')

```
git clone https://github.com/basho/riak.git
cd riak && git checkout ack-riak-json
```

#### Build
While in ```riak/``` and on the ```ack-riak-json``` branch:

```
make rel
```

#### Configure

Verify that Search and RiakJson are both turned `on` in `rel/riak/etc/riak.conf`

```
...
search = on
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
git clone https://github.com/basho-labs/riak_json.git
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

Please refer to [docs/architecture.md](https://github.com/basho-labs/riak_json/blob/master/docs/architecture.md) for more information.

### API Reference

Please refer to [docs/demo.md](https://github.com/basho-labs/riak_json/blob/master/docs/demo.md) and [docs/query.md](https://github.com/basho-labs/riak_json/blob/master/docs/query.md) for information about how tu use RiakJson and for examples of queries.


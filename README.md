# RiakJson

Riak Json is a JSON based document and query interface built on Riak and indexed by Solr

### Setup

### Pre-Built Ubuntu Package
If you have access to an Ubuntu 12.04 64-bit machine, with Oracle Java 7 installed on it,
you can use a pre-built Riak+RiakJson package, based on Riak 2.0.0pre15 and RiakJson 0.0.1:

```bash
wget http://ps-tools.data.riakcs.net:8080/riak_2.0.0pre15-9d332123-riak_json_0.0.1_amd64.deb
dpkg -i riak_2.0.0pre15-9d332123-riak_json_0.0.1_amd64.deb
```

If you're curious about where the various Riak components go, you can list the files installed by the package:

```
dpkg -L riak_2.0.0pre15-9d332123-riak_json_0.0.1_amd64.deb
```

(You still have to make sure Search/Yokozuna is enabled, by editing ```/etc/riak/riak.conf``` and ensuring that
```search = on```)

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

Verify that Search is enabled in `rel/riak/etc/riak.conf`

```
...
search = on
...
```

#### Start the server

```
./rel/riak/bin/riak start
```

### Test

#### Http Interface

```
# Download
git clone https://github.com/basho-labs/riak_json_http.git
cd riak_json_http

# Build
make

# Unit Test
make test

# Integration Test
make itest
```

#### Wire Interface (for MongoDB clients) (WORK IN PROGRESS)

```
# Download
# git clone https://github.com/basho-labs/riak_json_wire.git
# cd riak_json_wire

# Build
# make

# Unit Test
# make test

# Integration Test
# make itest
```

### Architecture

Please refer to [docs/architecture.md](https://github.com/basho-labs/riak_json/blob/master/docs/architecture.md) for more information.

### Http API Reference

Please refer to [docs/demo.md](https://github.com/basho-labs/riak_json/blob/master/docs/demo.md) and [docs/query.md](https://github.com/basho-labs/riak_json/blob/master/docs/query.md) for information about how tu use RiakJson and for examples of queries.

### RiakJson Clients and Projects
 - [riak_json_python_client](https://github.com/basho-labs/riak_json_python_client) - Python client
 - [riak_json_ruby_client](https://github.com/basho-labs/riak_json_ruby_client) - Ruby client
 - [riak_json_java_client](https://github.com/basho-labs/riak_json_java_client) - Java client
 - [nrj](https://github.com/dmitrizagidulin/nrj) - Node.js client
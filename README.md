# RiakJson

Riak Json is a JSON based document and query interface built on Riak and indexed by Solr.
See [docs/RELEASE_NOTES.md](https://github.com/basho-labs/riak_json/blob/master/docs/RELEASE_NOTES.md) for versions/feature log.

### Installation

#### From a Vagrant box
If you're familiar with Vagrant, you can download a pre-built Ubuntu 12.04 LTS VM,
which has Oracle Java 7u25 and RiakJson pre-installed and running.

1. [Install VirtualBox](https://www.virtualbox.org/wiki/Downloads)
2. [Install Vagrant](https://docs.vagrantup.com/v2/installation/)
3. Download the ```riak_json-0.0.1.box``` file (880 Mb), add it to your local boxes list, and initialize a Vagrant VM:

    ```
    wget http://ps-tools.data.riakcs.net:8080/riak_json-0.0.1.box
    vagrant box add --name riak_json --provider virtualbox riak_json-0.0.1.box
    mkdir riak_json_vm
    cd riak_json_vm
    vagrant init riak_json
    ```
4. Now you can bring up the VM, ssh to it, and test out Riak and RiakJson

    ```
    vagrant up
    vagrant ssh
    riak ping
    curl http://localhost:8098/ping
    ```

In addition, you may want to set up a pass-through port into the VM, so that you can make HTTP requests 
to RiakJson from your development machine. (In Vagrant terminology, the riak_json VM is the guest, and your dev machine would be the host).

Edit the ```Vagrantfile```, and add or uncomment the line:

```ruby
  config.vm.network :forwarded_port, guest: 8098, host: 10098
```

Inside the VM, change the Riak config file to listen to requests from the outside.
Stop Riak first (```sudo riak stop```), then edit ```/etc/riak/riak.conf```, and change the http listener to:
```
listener.http.internal = 0.0.0.0:8098
```
Restart Riak (```sudo riak start```), exit the VM, and you should be able to make HTTP requests to Riak from the outside
(accessing port 10098 gets routed to the VM's port 8098).
```
curl http://localhost:10098/ping
OK
```

#### From a Pre-Built Ubuntu Package
If you have access to an Ubuntu 12.04 LTS 64-bit machine, with Oracle Java 7u25 installed on it, you can use a pre-built Riak+RiakJson package:

```bash
wget http://ps-tools.s3.amazonaws.com/riak_2.0.5-riak_json_0.0.4-aa73abc6-1_amd64.deb
dpkg -i riak_2.0.5-riak_json_0.0.4-aa73abc6-1_amd64.deb
```

If you need to uninstall a previous version of riak, you can run the dpkg 'purge' command:

```
dpkg -P riak
```

If you're curious about where the various Riak components go, you can list the files installed by the package:

```
dpkg -L riak_2.0.5-riak_json_0.0.4-aa73abc6-1_amd64.deb
```

(You still have to make sure Search/Yokozuna is enabled, by editing ```/etc/riak/riak.conf``` and ensuring that
```search = on```)

#### From Source
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

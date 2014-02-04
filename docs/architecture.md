# RiakJson Architecture

![RiakJson Overview](https://raw.github.com/basho-labs/riak_json/master/docs/riak_json_arch.jpg)

##Riak Integration
RiakJson is a library of modules whose core responsibilities are:

##### Riak K/V Integration
* CRUD Operations given a "Collection", "Key", and "Document" using Riak K/V

##### Riak Search Query Integration
* Translation of Json queries to Solr queries
* Execution of those queries on Riak Search
* Building a Json result set from the Solr query results

##### Riak Search Schema Integration
* Translation of Json schemas to Solr XML schemas and back
* CRUD Operations given a "Collection", "SchemaName", and "Schema" using Riak Search
* BucketType and Index management / abstraction for searchable "Collections" / buckets

##Inferred Schemas

Schemas can be inferred based on the contents of the first document stored in a RiakJson collection, although manually creating a Json schema is the preferred approach for actual applications.

![RiakJson Schema Flow](https://raw.github.com/basho-labs/riak_json/master/docs/riak_json_schema.jpg)

##### Example Schema

```
    [
      {
        "name": "name", 
        "type": "string"
      },
      {
        "name": "full_name", 
        "type": "text"
      },
      {
        "name": "metric", 
        "type": "number"
      }
    ]
```
##### Schema Endpoints

A schema for a collection can befound at the /schema endpoint:

```
http://localhost:8098/document/collection/{collection}/schema
```

This maps directly to an XML generated schema that Yokozuna understands

```
http://localhost:8098/yz/schema/{collection}DefaultSchema
```
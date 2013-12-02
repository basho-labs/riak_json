#RiakJson Demo

###Architecture

![RiakJson Flow](http://f.cl.ly/items/410v0a0B3w2G1L354034/Image%202013.10.25%201%3A36%3A43%20PM.jpeg)

####Inferred Schema Example

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

This can be found at [Link](http://localhost:8098/document/collection/names/schema/the_demoDefaultSchema)

```
http://localhost:8098/document/collection/names/schema/the_demoDefaultSchema
```

This maps directly to an XML generated schema that Yokozuna understands, found here [Link](http://localhost:8098/yz/schema/the_demoDefaultSchema)

```
http://localhost:8098/yz/schema/the_demoDefaultSchema
```

####Riak Integration Strategy: Proxy

A set of proxy classes are used to make calls to Riak. This was decided because we knew initially that we wanted to quickly make a proof of concept to show off functionality, but knew it would take time to actually decide and implement the final integration with Yokozuna.

The proxies allow us to easily swap out different ways of interacting with Yokozuna while keeping the majority of the logic the same.

#####Previously

When RiakJson development started, Protobuf support was not available in the riak-erlang-client for yokozuna calls, so the first several integration points were implemented as erlang generated HTTP queries to Riak.

#####Current

When Protobuf support was added to Yokozuna and the riak-erlang-client, we began writing new proxies that deal only with the simpler and more readable riak-erlang-client calls.

#####Future Plans

The end goal is to make RiakJson an app that sits directly on top of or beside Riak, much like Yokozuna does. Some work has started on this approach, but it is not finished yet.

###Currently supported endpoints

####Documents

#####Store documents

```
COL=the_demo4

curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/$COL/casey" -d \
    '{"name": "Casey", "metric": 9000}'
    
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/$COL/drew" -d \
    '{"name": "Drew", "metric": 1}'

curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/$COL/dan" -d \
    '{"name": "Dan", "metric": 2}'

curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/$COL/felix" -d \
    '{"name": "Felix", "metric": 3}'
    
```

#####Get a document by key

```
curl -v -XGET -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/felix" \
    | python -mjson.tool

```

#####Delete a document

```
curl -v -XDELETE -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/felix"

#put it back
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/$COL/felix" -d \
    '{"name": "Felix", "metric": 3}'
```

####Querying

#####Get one document back

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/one" -d \
    '{"name": "Drew"}' \
    | python -mjson.tool

curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/one" -d \
    '{"name": {"$regex": "/.*/"}}' \
    | python -mjson.tool
```

#####Get multiple documents back that start with "D"

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"name": {"$regex": "/D.*/"}}' \
    | python -mjson.tool
```

#####More operations
```
# Less than or equal
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"metric": {"$lte": 5}}' \
    | python -mjson.tool

# Greater than
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"metric": {"$gt": 2}}' \
    | python -mjson.tool

# Boolean OR
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"$or": [{"name": {"$regex": "/D.*/"}},{"name": {"$regex": "/F.*/"}}]}' \
    | python -mjson.tool

# Pagination
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$per_page": 1}' \
    | python -mjson.tool

curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$per_page": 1, "$page": 2}' \
    | python -mjson.tool

# Sorting
# Descending
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$sort": {"metric": -1}}' \
    | python -mjson.tool

#Ascending
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$sort": {"metric": 1}}' \
    | python -mjson.tool
```

#### Schemas

##### Get the default / generated Schema

```
curl -v -XGET -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/schema/"$COL"DefaultSchema" \
    | python -mjson.tool
```

##### Put a Schema

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/schema/daschema" -d \
    '[{"name": "single_string", "type": "string"},{"name": "string_with_spaces", "type": "text"},{"name": "some_number", "type": "number"}]'
```

##### Get a Schema

```
curl -v -XGET -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/schema/daschema" \
    | python -mjson.tool
```

#####Example Input

```
{
    "name": {"$regex": "/.*/"}, 
    "$sort": {"metric": 1}
}
```

#####Example Output

```
{
    "data": [
        {
            "_id": "drew", 
            "metric": 1, 
            "name": "Drew"
        }, 
        {
            "_id": "dan", 
            "metric": 2, 
            "name": "Dan"
        }, 
        {
            "_id": "felix", 
            "metric": 3, 
            "name": "Felix"
        }
    ], 
    "num_pages": 1, 
    "page": 1, 
    "per_page": 10, 
    "stats": "undefined", 
    "total": 3
}
```

#####Stats*

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/$COL/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$stats": ["metric"]}' \
    | python -mjson.tool
```

\* This was just added so we haven't decided if we're going to do something like this interface or attempt to merge this solr functionality with the $group aggregation framework in mongo

###Next Steps

1. Facet / aggregation integration
2. Authentication
3. Direct Riak integration
4. Additional operators like TO, $geoWithin, $near
5. Schemaless Support
6. Field list and other solr feature integrations
7. General refactoring and cleanup tasks
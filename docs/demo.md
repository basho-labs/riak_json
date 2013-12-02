
# RiakJson Demo

Riak Json is a JSON based document and query interface built on Riak and indexed by Solr

## Getting Started

### Collections

Collections are a group of documents, they map 1:1 with Riak buckets
Choose a name for a collection

```
test
```

All of the Riak Json URIs are based on this collection

```
http://localhost:8098/document/collection/test
```

Collection Resources:

```
.../{document_key}
.../schema
.../query
```

### Documents
Build a JSON document

```
{
  "name": "Casey", 
  "metric": 9000
}
```

Choose a key

```
casey
```

Put the document in the collection

```
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/demo_collection/casey" -d \
    '{"name": "Casey", "metric": 9000}'
```

Without an explicit schema, this results in an inferred schema

```
    [
      {
        "name": "name", 
        "type": "string"
      },
      {
        "name": "metric", 
        "type": "number"
      }
    ]
```

Load a few more documents into the collection so they can be queried

```
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/demo_collection/drew" -d \
    '{"name": "Drew", "metric": 1}'
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/demo_collection/dan" -d \
    '{"name": "Dan", "metric": 2}'
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/demo_collection/felix" -d \
    '{"name": "Felix", "metric": 3}'
```

Load even more documents into the collection for more compelling reuslts

```
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Petunia -d "{\"name\": \"Petunia\", \"metric\": 31}"
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Max -d "{\"name\": \"Max\", \"metric\": 2}"
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Carrie -d "{\"name\": \"Carrie\", \"metric\": 28}"
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Wilt -d "{\"name\": \"Wilt\", \"metric\": 28}"
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Roberta -d "{\"name\": \"Roberta\", \"metric\": 2}"
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Rowena -d "{\"name\": \"Rowena\", \"metric\": 2}"
curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Robert -d "{\"name\": \"Robert\", \"metric\": 40}"
```

Get a document by key

```
curl -v -XPUT -H 'Content-Type: application/json' \
    "http://localhost:8098/document/collection/demo_collection/deleteme" -d \
    '{"name": "DeleteMe", "metric": 1}'
    
curl -v -XGET -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/deleteme" \
    | python -mjson.tool

```

Delete a document

```
curl -v -XDELETE -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/deleteme"
```

### Queries (One)

Find one document with `name` equal to "Drew"

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/one" -d \
    '{"name": "Drew"}' \
    | python -mjson.tool
```

Find one document with `name` that matches a regular expression

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/one" -d \
    '{"name": {"$regex": "/C.*/"}}' \
    | python -mjson.tool
```

This is an example of an `operator`, `{"$regex": "/C.*/"}` is a query for any one document with a name that starts with "C"

### Queries (All)

Find all documents with `name` that matches a regular expression

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"name": {"$regex": "/D.*/"}}' \
    | python -mjson.tool
```

Here, `{"$regex": "/D.*/"}}` is a query for all documents with a name that starts with "D"

#### Comparison Operators

Find all documents with `metric` less than or equal to 5

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"metric": {"$lte": 5}}' \
    | python -mjson.tool
```

Find all documents with `metric` greater than 2

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"metric": {"$gt": 2}}' \
    | python -mjson.tool
```

#### Boolean Operators

Find all documents with `name` that begins with "D" or "F"

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"$or": [{"name": {"$regex": "/D.*/"}},{"name": {"$regex": "/F.*/"}}]}' \
    | python -mjson.tool
```

#### Pagination

Use the `$per_page` parameter to specify how many documents to return in a page of results

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$per_page": 1}' \
    | python -mjson.tool
```

Use the `$page` parameter to specify which result page to show

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$per_page": 1, "$page": 2}' \
    | python -mjson.tool
```

#### Sorting

Use the `$sort` parameter, a field name, and a -1 value to return the results in descending order based on the value of that field

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$sort": {"metric": -1}}' \
    | python -mjson.tool
```

Change the value to 1 to return the results in ascending order

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/demo_collection/query/all" -d \
    '{"name": {"$regex": "/.*/"}, "$sort": {"metric": 1}}' \
    | python -mjson.tool
```

#### Group Records

Group by distinct field value

```
curl -v -H"content-type: application/json" -H"accept: application/json" \
    -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all \
    -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$group\": [{\"field\": \"metric\", \"limit\": 10, \"start\": 1}]}" \
    | python -m json.tool
```

Group by field queries

```
curl -v -H"content-type: application/json" -XPUT \
    http://127.0.0.1:8098/document/collection/demo_collection/query/all \
    -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$group\": [{\"queries\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"name\": {\"\$regex\": \"/.*a/\"}}], \"start\": 1}]}" \
    | python -m json.tool
```

#### Categorize Records

Categorize by distinct field value

```
curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT \
    http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"field\": \"metric\"}]}" | python -m json.tool
```

Categorize by field queries

```
curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT \
    http://127.0.0.1:8098/document/collection/demo_collection/query/all \
    -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"queries\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"name\": {\"\$regex\": \"/.*a/\"}}]}]}" \
    | python -m json.tool
```

Categorize by range and increments

```
curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT \
    http://127.0.0.1:8098/document/collection/demo_collection/query/all \
    -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"range\": {\"field\": \"metric\", \"start\": 1, \"end\": 50, \"increment\": 10}}]}" \
    | python -m json.tool
```

## Real World Applications
### Schemas

Schema inferral is great for learning the endpoints and developing simple proofs of concept, but for real world applications there is a better way to manage document indexing.

Riak Json provides access to the following schema endpoint:

```
GET, PUT, DELETE
http://localhost:8098/document/collection/blog/schema
```
#### Design

First, decide on the schema fields and types for your application
A blog entry collection might look like this:

```
[
  {
    "name": "author", 
    "type": "string"
  },{
    "name": "post", 
    "type": "text"
  },{
    "name": "timestamp", 
    "type": "number"
  }
]
```

#### Field Types

All currently supported field types:

* "text" -> A string with spaces
* "string" -> A string with no spaces
* "multi_string" -> Array of strings
* "number" -> Integers or Floats

#### Put

Create the schema by executing a `PUT` on the `/schema` endpoint

```
curl -v -XPUT -H 'Content-Type: application/json' -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/blog/schema" -d \
    '[{"name": "author", "type": "string"},{"name": "post", "type": "text"},{"name": "timestamp", "type": "number"}]'
```

This same endpoint and method can be used to update a schema as well, however there are some caveats to keep in mind which will be discussed later

#### Get

Verify the schema was saved properly by performing a `GET` on schema

```
curl -v -GET -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/blog/schema" \
    | python -mjson.tool
```

#### Delete

A schema and the underlying indexes can be deleted as well if a collection should no longer be indexed

```
curl -v -XDELETE -H 'Accept: application/json' \
    "http://localhost:8098/document/collection/blog/schema"
```

#### Concerns

* Index Size: Riak replication + solr index data can become expensive
* Index Speed: numbers < string < text
* Fields not in schema: will not be indexed, and will not be queryable
    
#### Edge Cases and Questions

* What happens when indexed fields aren’t in an uploaded document? Currently there is no schema enforcement, so nothing will happen. An optional "required" attribute may be added to schema fields in the future
* Can pre-existing data be indexed? Yes, an existing bucket can be used as a collection. AAE will retro-actively index all of the data in that bucket but it is not deterministic (no status, time remaining, etc)
* What are the caveats for Schema Update / Migration?
    * In the Riak 2.0 release, reindex functionality will be exposed by Yokozuna which Riak Json will use it
    * Until that is released however, indexes will be removed and past data may not be queryable
    * There are also currently undocumented manual ways to deal with removing stale indexes from old data so that AAE will reindex the data
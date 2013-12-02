# RiakJson Architecture

![RiakJson Flow](http://f.cl.ly/items/410v0a0B3w2G1L354034/Image%202013.10.25%201%3A36%3A43%20PM.jpeg)

####Inferred Schemas

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

A schema for a collection can befound at the /schema endpoint:

```
http://localhost:8098/document/collection/{collection}/schema
```

This maps directly to an XML generated schema that Yokozuna understands

```
http://localhost:8098/yz/schema/{collection}DefaultSchema
```

####Riak Integration

RiakJson is implemented as an app directly on top of Riak, the only integration points it has are some internal api calls to Yokozuna.
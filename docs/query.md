# RiakJson Queries

### Setting a schema:

    [
        {
            "name": "name",
            "type": "string"
        },
        {
            "name": "metric",
            "type": "integer"
        }
    ]

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/schema -d "[{\"name\": \"name\", \"type\": \"string\"}, {\"name\": \"metric\", \"type\": \"integer\"}]"

### Getting a schema:

    curl -v -XGET http://127.0.0.1:8098/document/collection/demo_collection/schema

### Insert a Record:

    {
        "name": "Harold",
        "metric": 33
    }

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Harold -d "{\"name\": \"Harold\", \"metric\": 33}"

#### Result, the key:
    
    Harold


### Retrieve a Record for key, Harold:

    curl -v -H"accept: application/json" -XGET http://127.0.0.1:8098/document/collection/demo_collection/Harold | python -m json.tool

#### Results:

    {
        "metric": 33,
        "name": "Harold"
    }

### Delete a Record for key, Harold

    curl -v -XDELETE http://127.0.0.1:8098/document/collection/demo_collection/Harold

### Insert a few records

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Harold -d "{\"name\": \"Harold\", \"metric\": 33}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Petunia -d "{\"name\": \"Petunia\", \"metric\": 31}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Max -d "{\"name\": \"Max\", \"metric\": 2}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Carrie -d "{\"name\": \"Carrie\", \"metric\": 28}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Wilt -d "{\"name\": \"Wilt\", \"metric\": 28}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Roberta -d "{\"name\": \"Roberta\", \"metric\": 2}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Rowena -d "{\"name\": \"Rowena\", \"metric\": 2}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Robert -d "{\"name\": \"Robert\", \"metric\": 40}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Casey -d "{\"name\": \"Casey\", \"metric\": 9000}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Drew -d "{\"name\": \"Drew\", \"metric\": 1}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Dan -d "{\"name\": \"Dan\", \"metric\": 2}"

    curl -v -H"content-type: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/Felix -d "{\"name\": \"Felix\", \"metric\": 3}"


### Query with regex, R.*

    {
        "name": {
            "$regex": "/R.*/"
        }
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/R.*/\"}}" | python -m json.tool

#### Results:

    {
        "per_page": 100,
        "total": 3,
        "num_pages": 1,
        "page": 0,
        "data": [
            {
                "metric": 2,
                "_id": "Roberta",
                "name": "Roberta"
            },
            {
                "metric": 2,
                "_id": "Rowena",
                "name": "Rowena"
            },
            {
                "metric": 40,
                "_id": "Robert",
                "name": "Robert"
            }
        ]
    }

### Equality Query:

    {
        "name": "Harold"
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": \"Harold\"}" | python -m json.tool

#### Results:

    {
        "per_page": 100,
        "total": 1,
        "num_pages": 1,
        "page": 0,
        "data": [
            {
                "metric": 33,
                "_id": "Harold",
                "name": "Harold"
            }
        ]
    }

### Find one record, Equality Query:

    {
        "name": "Harold"
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/one -d "{\"name\": \"Harold\"}" | python -m json.tool

#### Results:

    {
        "metric": 33,
        "_id": "Harold",
        "name": "Harold"
    }

### Query with boolean and

    {
        "$and": [
            {
                "name": {
                    "$regex": "/R.*/"
                }
            },
            {
                "metric": {
                    "$gte": 5
                }
            },
            {
                "metric": {
                    "$lte": 100
                }
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"\$and\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"metric\": {\"\$gte\": 5}}, {\"metric\": {\"\$lte\": 100}}]}" | python -m json.tool

#### Results:

    {
        "per_page": 100,
        "total": 1,
        "num_pages": 1,
        "page": 0,
        "data": [
            {
                "metric": 40,
                "_id": "Robert",
                "name": "Robert"
            }
        ]
    }

### Query with boolean or

    {
        "$or": [
            {
                "metric": 2
            },
            {
                "metric": 28
            },
            {
                "metric": 40
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"\$or\": [{\"metric\": 2}, {\"metric\": 28}, {\"metric\": 40}]}" | python -m json.tool

#### Results:

    {
        "per_page": 100,
        "total": 7,
        "num_pages": 1,
        "page": 0,
        "data": [
            {
                "metric": 40,
                "_id": "Robert",
                "name": "Robert"
            },
            {
                "metric": 28,
                "_id": "Carrie",
                "name": "Carrie"
            },
            {
                "metric": 28,
                "_id": "Wilt",
                "name": "Wilt"
            },
            {
                "metric": 2,
                "_id": "Max",
                "name": "Max"
            },
            {
                "metric": 2,
                "_id": "Roberta",
                "name": "Roberta"
            },
            {
                "metric": 2,
                "_id": "Rowena",
                "name": "Rowena"
            },
            {
                "metric": 2,
                "_id": "Dan",
                "name": "Dan"
            }
        ]
    }

### Group by value for field, metric

    {
        "name": {
            "$regex": "/.*/"
        },
        "$group": [
            {
                "field": "metric",
                "limit": 10,
                "sort": {
                    "metric": "asc"
                }
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$group\": [{\"field\": \"metric\", \"limit\": 10, \"sort\": {\"metric\": \"asc\"}}]}" | python -m json.tool

#### Results:   

    {
        "groups": {
            "metric": {
                "33": [
                    {
                        "metric": 33,
                        "_id": "Harold",
                        "name": "Harold"
                    }
                ],
                "31": [
                    {
                        "metric": 31,
                        "_id": "Petunia",
                        "name": "Petunia"
                    }
                ],
                "28": [
                    {
                        "metric": 28,
                        "_id": "Carrie",
                        "name": "Carrie"
                    },
                    {
                        "metric": 28,
                        "_id": "Wilt",
                        "name": "Wilt"
                    }
                ],
                "40": [
                    {
                        "metric": 40,
                        "_id": "Robert",
                        "name": "Robert"
                    }
                ],
                "1": [
                    {
                        "metric": 1,
                        "_id": "Drew",
                        "name": "Drew"
                    }
                ],
                "3": [
                    {
                        "metric": 3,
                        "_id": "Felix",
                        "name": "Felix"
                    }
                ],
                "2": [
                    {
                        "metric": 2,
                        "_id": "Max",
                        "name": "Max"
                    },
                    {
                        "metric": 2,
                        "_id": "Roberta",
                        "name": "Roberta"
                    },
                    {
                        "metric": 2,
                        "_id": "Rowena",
                        "name": "Rowena"
                    },
                    {
                        "metric": 2,
                        "_id": "Dan",
                        "name": "Dan"
                    }
                ],
                "9000": [
                    {
                        "metric": 9000,
                        "_id": "Casey",
                        "name": "Casey"
                    }
                ]
            }
        }
    }

### Categorize by value for field, metric

    {
        "name": {
            "$regex": "/.*/"
        },
        "$per_page": 0,
        "$categorize": [
            {
                "field": "metric"
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"field\": \"metric\"}]}" | python -m json.tool

#### Results:

    {
        "num_pages": 1,
        "per_page": 0,
        "total": 12,
        "data": [],
        "page": 0,
        "categories": {
            "metric": {
                "33": 1,
                "31": 1,
                "28": 2,
                "40": 1,
                "1": 1,
                "3": 1,
                "2": 4,
                "9000": 1
            }
        }
    }

### Categorize by range for field, metric

    {
        "name": {
            "$regex": "/.*/"
        },
        "$per_page": 0,
        "$categorize": [
            {
                "range": {
                    "field": "metric",
                    "start": 1,
                    "end": 50,
                    "increment": 10
                }
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"range\": {\"field\": \"metric\", \"start\": 1, \"end\": 50, \"increment\": 10}}]}" | python -m json.tool

#### Results: 

    {
        "num_pages": 1,
        "per_page": 0,
        "total": 12,
        "data": [],
        "page": 0,
        "categories": {
            "metric": {
                "1": 6,
                "11": 0,
                "31": 3,
                "21": 2,
                "41": 0
            }
        }
    }

### Categorize by range for field, name

    {
        "name": {
            "$regex": "/.*/"
        },
        "$per_page": 0,
        "$categorize": [
            {
                "queries": [
                    {
                        "name": {
                            "$regex": "/R.*/"
                        }
                    },
                    {
                        "name": {
                            "$regex": "/.*a/"
                        }
                    }
                ]
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"queries\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"name\": {\"\$regex\": \"/.*a/\"}}]}]}" | python -m json.tool

#### Results: 

    {
        "num_pages": 1,
        "per_page": 0,
        "total": 12,
        "data": [],
        "page": 0,
        "categories": {
            "Queries": {
                "name:/.*a/": 3,
                "name:/R.*/": 3
            }
        }
    }

### Categorize with stats on field, metric

    {
        "name": {
            "$regex": "/.*/"
        },
        "$per_page": 0,
        "$categorize": [
                "stats": {
                    "field": "name",
                    "calculate": "metric"
                }
            }
        ]
    }

    curl -v -H"content-type: application/json" -H"accept: application/json" -XPUT http://127.0.0.1:8098/document/collection/demo_collection/query/all -d "{\"name\": {\"\$regex\": \"/.*/\"}, \"\$per_page\": 0, \"\$categorize\": [{\"queries\": [{\"name\": {\"\$regex\": \"/R.*/\"}}, {\"name\": {\"\$regex\": \"/.*a/\"}}], \"stats\": {\"field\": \"name\", \"calculate\": \"metric\"}}]}" | python -m json.tool

#### Results    

    {
        "num_pages": 1,
        "per_page": 0,
        "total": 12,
        "data": [],
        "page": 0,
        "categories": {
            "name": {
                "Casey": {
                    "count": 1,
                    "missing": 0,
                    "max": 9000.0,
                    "sum": 9000.0,
                    "min": 9000.0,
                    "sumOfSquares": 81000000.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 9000.0
                },
                "Harold": {
                    "count": 1,
                    "missing": 0,
                    "max": 33.0,
                    "sum": 33.0,
                    "min": 33.0,
                    "sumOfSquares": 1089.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 33.0
                },
                "Carrie": {
                    "count": 1,
                    "missing": 0,
                    "max": 28.0,
                    "sum": 28.0,
                    "min": 28.0,
                    "sumOfSquares": 784.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 28.0
                },
                "Wilt": {
                    "count": 1,
                    "missing": 0,
                    "max": 28.0,
                    "sum": 28.0,
                    "min": 28.0,
                    "sumOfSquares": 784.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 28.0
                },
                "Max": {
                    "count": 1,
                    "missing": 0,
                    "max": 2.0,
                    "sum": 2.0,
                    "min": 2.0,
                    "sumOfSquares": 4.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 2.0
                },
                "Robert": {
                    "count": 1,
                    "missing": 0,
                    "max": 40.0,
                    "sum": 40.0,
                    "min": 40.0,
                    "sumOfSquares": 1600.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 40.0
                },
                "Felix": {
                    "count": 1,
                    "missing": 0,
                    "max": 3.0,
                    "sum": 3.0,
                    "min": 3.0,
                    "sumOfSquares": 9.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 3.0
                },
                "Roberta": {
                    "count": 1,
                    "missing": 0,
                    "max": 2.0,
                    "sum": 2.0,
                    "min": 2.0,
                    "sumOfSquares": 4.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 2.0
                },
                "Drew": {
                    "count": 1,
                    "missing": 0,
                    "max": 1.0,
                    "sum": 1.0,
                    "min": 1.0,
                    "sumOfSquares": 1.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 1.0
                },
                "Rowena": {
                    "count": 1,
                    "missing": 0,
                    "max": 2.0,
                    "sum": 2.0,
                    "min": 2.0,
                    "sumOfSquares": 4.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 2.0
                },
                "Petunia": {
                    "count": 1,
                    "missing": 0,
                    "max": 31.0,
                    "sum": 31.0,
                    "min": 31.0,
                    "sumOfSquares": 961.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 31.0
                },
                "Dan": {
                    "count": 1,
                    "missing": 0,
                    "max": 2.0,
                    "sum": 2.0,
                    "min": 2.0,
                    "sumOfSquares": 4.0,
                    "stddev": 0.0,
                    "facets": {},
                    "mean": 2.0
                }
            }
        }
    }

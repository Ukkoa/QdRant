# QdRant
An easy way for R users to interact with Qdrant's vector database.  

Work in progress and help appreciated! =) 



## Getting Started

```r
library(QdRant)

my_client  <- QdrantClient$new(my_host, my_port, my_api_key)
collection <- Collection$new(my_client)
points     <- Points$new(my_client)
services   <- Services$new(my_client)

## Collections

collection$create('test_collection', number_of_embeddings)
collection$detail('test_collection')
collection$list()
collection$exists('test_collection')
collection$delete('test_collection')

## Points

points$retrieve_point('my_collection', id)
points$retrieve_points('my_collection', with_vector = F, with_payload = T, shard_keys = NULL, ids = list('30627340-1d9b-43a3-b4ed-1574076311f1'))

## Services

services$kubernetes_readiness_probe()
services$kubernetes_liveness_probe()
services$kubernetes_health_check()
services$set_write_protection()
services$check_write_protection()
services$collect_prometheus_metrics()
services$retrieve_instance_details()
services$collect_telemetry_data()

```



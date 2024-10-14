# QdRant
An easy way for R users to interact with Qdrant's vector database

## Getting Started

```r
library(QdRant)

number_of_embeddings <- 768
vector_body          <- 'TBD'
input_text           <- 'How to of quantum circuits' 

my_client  <- QdrantClient$new(my_host, my_port, my_api_key)

## Collections

collection <- Collection$new(my_client)
collection$create('test_collection', number_of_embeddings)
collection$detail('test_collection')
collection$list()
collection$exists('test_collection')
collection$delete('test_collection')

## Vectors

vector <- VectorManager$new(my_client)
vectors$insert_vectors('test_collection', vector_body))
closest_point    <- vectors$search_vectors('test_collection', get_nomic_embedding(input_text), limit = 1)
closest_document <- vectors$get_vector('test_collection', closest_point$result[[1]]$id)

```



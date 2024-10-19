
Search <-
  R6::R6Class("Search",
              public = list(
                client = NULL,
                initialize = function(client) {
                  self$client <- client
                },

                search_points = function(collection_name, vector, limit = 1, filter = NULL) {
                  url <- paste0(self$client$get_base_url(), "/collections/", collection_name, "/points/search")
                  body <- list(
                    vector = vector,
                    limit = limit,
                    filter = filter
                  )
                  return(self$client$make_request("POST", url, body))
                }

              ))





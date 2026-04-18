
#' Search
#'
#' @description
#' Vector search, recommendation, and discovery methods for a Qdrant collection.
#'
#' The modern \code{query_points} family (POST \code{/points/query}) is the
#' preferred interface and supersedes the legacy \code{search_points} /
#' \code{recommend_points} endpoints.  Both are provided for compatibility.
#'
#' @field client A \code{QdrantClient} instance.
#'
#' @export
Search <- R6::R6Class("Search",
  cloneable = FALSE,
  private = list(.req = NULL, .base_url = NULL),
  public = list(

    #' @description Initialize with closures provided by \code{QdrantClient}.
    #' @param req_fn Function. Makes HTTP requests.
    #' @param base_url_fn Function. Returns the base URL.
    initialize = function(req_fn, base_url_fn) {
      private$.req      <- req_fn
      private$.base_url <- base_url_fn
    },

    # -------------------------------------------------------------------------
    # Modern universal query (preferred)
    # -------------------------------------------------------------------------

    #' @description
    #' Universal point query. Replaces \code{search_points}, \code{recommend_points},
    #' and \code{discover_points} in a single flexible endpoint.
    #'
    #' @param collection_name Name of the collection.
    #' @param query Named list specifying the query type. Examples:
    #'   \itemize{
    #'     \item Dense ANN: \code{list(nearest = c(0.1, 0.2, 0.3))}
    #'     \item By ID:     \code{list(nearest = 42L)}
    #'     \item Recommend: \code{list(recommend = list(positive = list(1L, 2L), negative = list(3L)))}
    #'     \item Discover:  \code{list(discover = list(target = 1L, context = list(...)))}
    #'     \item Fusion:    \code{list(fusion = "rrf")}
    #'   }
    #' @param prefetch (Optional) List of sub-queries to execute before the main
    #'   query (for re-ranking / hybrid search).
    #' @param using (Optional) Character. Named vector to search when the
    #'   collection has multiple named vectors.
    #' @param filter (Optional) Named list filter expression.
    #' @param params (Optional) Named list of search parameters (e.g. \code{hnsw_ef}).
    #' @param score_threshold (Optional) Numeric. Minimum score to include a result.
    #' @param limit Integer. Maximum results to return (default 10).
    #' @param offset Integer. Number of results to skip (default 0).
    #' @param with_vector Logical or vector names. Include vector(s) in response.
    #' @param with_payload Logical or field selector. Include payload in response.
    #' @param lookup_from (Optional) Named list with \code{collection} and
    #'   optionally \code{vector_name} — look up vectors from another collection.
    #' @param shard_key (Optional) Target specific shard(s).
    #'
    #' @return API response list with \code{points}.
    #'
    #' @examples
    #' \dontrun{
    #'   search$query_points("my_collection",
    #'     query = list(nearest = c(0.1, 0.2, 0.3)), limit = 5)
    #' }
    query_points = function(collection_name, query, prefetch = NULL,
                            using = NULL, filter = NULL, params = NULL,
                            score_threshold = NULL, limit = 10, offset = 0,
                            with_vector = FALSE, with_payload = TRUE,
                            lookup_from = NULL, shard_key = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(query))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/query")
      body <- list(query = query, limit = limit, offset = offset,
                   with_vector = with_vector, with_payload = with_payload)
      if (!is.null(prefetch))        body$prefetch        <- prefetch
      if (!is.null(using))           body$using           <- using
      if (!is.null(filter))          body$filter          <- filter
      if (!is.null(params))          body$params          <- params
      if (!is.null(score_threshold)) body$score_threshold <- score_threshold
      if (!is.null(lookup_from))     body$lookup_from     <- lookup_from
      if (!is.null(shard_key))       body$shard_key       <- shard_key
      private$.req("POST", url, body)
    },

    #' @description
    #' Execute multiple \code{query_points} requests in one round-trip.
    #'
    #' @param collection_name Name of the collection.
    #' @param searches List of query request objects (each is a named list
    #'   with the same fields as \code{query_points}).
    #'
    #' @return API response list with one result set per search.
    #'
    #' @examples
    #' \dontrun{
    #'   search$query_points_batch("my_collection", searches = list(
    #'     list(query = list(nearest = c(0.1, 0.2)), limit = 3),
    #'     list(query = list(nearest = c(0.4, 0.5)), limit = 3)
    #'   ))
    #' }
    query_points_batch = function(collection_name, searches) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(searches), length(searches) > 0)
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/query/batch")
      body <- list(searches = searches)
      private$.req("POST", url, body)
    },

    #' @description
    #' Query points and group results by a payload field.
    #'
    #' @param collection_name Name of the collection.
    #' @param query Named list. Same as in \code{query_points}.
    #' @param group_by Character. Payload field to group by.
    #' @param group_size Integer. Points per group (default 3).
    #' @param limit Integer. Maximum number of groups (default 10).
    #' @param filter (Optional) Named list filter expression.
    #' @param with_payload Logical or field selector.
    #' @param with_vector Logical or vector names.
    #' @param score_threshold (Optional) Numeric. Minimum score.
    #' @param using (Optional) Character. Named vector to query.
    #'
    #' @return API response list with grouped results.
    #'
    #' @examples
    #' \dontrun{
    #'   search$query_points_groups("my_collection",
    #'     query    = list(nearest = c(0.1, 0.2, 0.3)),
    #'     group_by = "category", group_size = 2, limit = 5)
    #' }
    query_points_groups = function(collection_name, query, group_by,
                                   group_size = 3, limit = 10, filter = NULL,
                                   with_payload = TRUE, with_vector = FALSE,
                                   score_threshold = NULL, using = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(query), is.character(group_by), nzchar(group_by))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/query/groups")
      body <- list(query = query, group_by = group_by,
                   group_size = group_size, limit = limit,
                   with_payload = with_payload, with_vector = with_vector)
      if (!is.null(filter))          body$filter          <- filter
      if (!is.null(score_threshold)) body$score_threshold <- score_threshold
      if (!is.null(using))           body$using           <- using
      private$.req("POST", url, body)
    },

    # -------------------------------------------------------------------------
    # Legacy search
    # -------------------------------------------------------------------------

    #' @description
    #' Approximate nearest-neighbour search (legacy endpoint).
    #'
    #' For new code prefer \code{query_points}.
    #'
    #' @param collection_name Name of the collection.
    #' @param vector Numeric vector to search with.
    #' @param limit Integer. Maximum results (default 10).
    #' @param filter (Optional) Named list filter expression.
    #' @param params (Optional) Named list of search parameters.
    #' @param score_threshold (Optional) Numeric. Minimum score.
    #' @param offset Integer. Results to skip (default 0).
    #' @param with_payload Logical or field selector.
    #' @param with_vector Logical or vector names.
    #'
    #' @return API response list with scored results.
    #'
    #' @examples
    #' \dontrun{
    #'   search$search_points("my_collection",
    #'     vector = c(0.1, 0.2, 0.3), limit = 5)
    #' }
    search_points = function(collection_name, vector, limit = 10,
                             filter = NULL, params = NULL,
                             score_threshold = NULL, offset = 0,
                             with_payload = TRUE, with_vector = FALSE) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.numeric(vector))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/search")
      body <- list(vector = vector, limit = limit, offset = offset,
                   with_payload = with_payload, with_vector = with_vector)
      if (!is.null(filter))          body$filter          <- filter
      if (!is.null(params))          body$params          <- params
      if (!is.null(score_threshold)) body$score_threshold <- score_threshold
      private$.req("POST", url, body)
    },

    #' @description
    #' Batch ANN searches in one round-trip.
    #'
    #' @param collection_name Name of the collection.
    #' @param searches List of search request objects (each matches
    #'   \code{search_points} fields).
    #'
    #' @return API response list with one result set per search.
    #'
    #' @examples
    #' \dontrun{
    #'   search$search_batch_points("my_collection", searches = list(
    #'     list(vector = c(0.1, 0.2, 0.3), limit = 3),
    #'     list(vector = c(0.4, 0.5, 0.6), limit = 3)
    #'   ))
    #' }
    search_batch_points = function(collection_name, searches) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(searches), length(searches) > 0)
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/search/batch")
      body <- list(searches = searches)
      private$.req("POST", url, body)
    },

    #' @description
    #' ANN search with results grouped by a payload field.
    #'
    #' @param collection_name Name of the collection.
    #' @param vector Numeric vector to search with.
    #' @param group_by Character. Payload field to group by.
    #' @param group_size Integer. Points per group (default 3).
    #' @param limit Integer. Maximum groups (default 10).
    #' @param filter (Optional) Named list filter expression.
    #' @param with_payload Logical or field selector.
    #' @param with_vector Logical or vector names.
    #' @param score_threshold (Optional) Numeric. Minimum score.
    #'
    #' @return API response list with grouped results.
    #'
    #' @examples
    #' \dontrun{
    #'   search$search_point_groups("my_collection",
    #'     vector = c(0.1, 0.2), group_by = "category")
    #' }
    search_point_groups = function(collection_name, vector, group_by,
                                   group_size = 3, limit = 10, filter = NULL,
                                   with_payload = TRUE, with_vector = FALSE,
                                   score_threshold = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.numeric(vector), is.character(group_by), nzchar(group_by))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/search/groups")
      body <- list(vector = vector, group_by = group_by,
                   group_size = group_size, limit = limit,
                   with_payload = with_payload, with_vector = with_vector)
      if (!is.null(filter))          body$filter          <- filter
      if (!is.null(score_threshold)) body$score_threshold <- score_threshold
      private$.req("POST", url, body)
    },

    # -------------------------------------------------------------------------
    # Recommend
    # -------------------------------------------------------------------------

    #' @description
    #' Recommend points using positive and negative example IDs or vectors.
    #'
    #' @param collection_name Name of the collection.
    #' @param positive List of point IDs or vectors to use as positive examples.
    #' @param negative (Optional) List of point IDs or vectors as negative examples.
    #' @param strategy (Optional) Character. Recommendation strategy
    #'   (\code{"average_vector"}, \code{"best_score"}).
    #' @param filter (Optional) Named list filter expression.
    #' @param params (Optional) Named list of search parameters.
    #' @param limit Integer. Maximum results (default 10).
    #' @param offset Integer. Results to skip (default 0).
    #' @param score_threshold (Optional) Numeric. Minimum score.
    #' @param using (Optional) Character. Named vector field to use.
    #' @param lookup_from (Optional) Named list. Look up vectors from another
    #'   collection (\code{list(collection = "name", vector_name = "vec")}).
    #' @param with_payload Logical or field selector.
    #' @param with_vector Logical or vector names.
    #'
    #' @return API response list with scored results.
    #'
    #' @examples
    #' \dontrun{
    #'   search$recommend_points("my_collection",
    #'     positive = list(1L, 2L), negative = list(3L), limit = 5)
    #' }
    recommend_points = function(collection_name, positive, negative = NULL,
                                strategy = NULL, filter = NULL, params = NULL,
                                limit = 10, offset = 0, score_threshold = NULL,
                                using = NULL, lookup_from = NULL,
                                with_payload = TRUE, with_vector = FALSE) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(positive), length(positive) > 0)
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/recommend")
      body <- list(positive = positive, limit = limit, offset = offset,
                   with_payload = with_payload, with_vector = with_vector)
      if (!is.null(negative))        body$negative        <- negative
      if (!is.null(strategy))        body$strategy        <- strategy
      if (!is.null(filter))          body$filter          <- filter
      if (!is.null(params))          body$params          <- params
      if (!is.null(score_threshold)) body$score_threshold <- score_threshold
      if (!is.null(using))           body$using           <- using
      if (!is.null(lookup_from))     body$lookup_from     <- lookup_from
      private$.req("POST", url, body)
    },

    #' @description
    #' Batch recommendations in one round-trip.
    #'
    #' @param collection_name Name of the collection.
    #' @param searches List of recommend request objects.
    #'
    #' @return API response list with one result set per request.
    #'
    #' @examples
    #' \dontrun{
    #'   search$recommend_batch("my_collection", searches = list(
    #'     list(positive = list(1L), limit = 5),
    #'     list(positive = list(2L), limit = 5)
    #'   ))
    #' }
    recommend_batch = function(collection_name, searches) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(searches), length(searches) > 0)
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/recommend/batch")
      body <- list(searches = searches)
      private$.req("POST", url, body)
    },

    #' @description
    #' Recommend points with results grouped by a payload field.
    #'
    #' @param collection_name Name of the collection.
    #' @param positive List of positive example IDs or vectors.
    #' @param group_by Character. Payload field to group by.
    #' @param negative (Optional) List of negative example IDs or vectors.
    #' @param group_size Integer. Points per group (default 3).
    #' @param limit Integer. Maximum groups (default 10).
    #' @param filter (Optional) Named list filter expression.
    #' @param with_payload Logical or field selector.
    #' @param with_vector Logical or vector names.
    #' @param score_threshold (Optional) Numeric. Minimum score.
    #'
    #' @return API response list with grouped recommendations.
    #'
    #' @examples
    #' \dontrun{
    #'   search$recommend_groups("my_collection",
    #'     positive = list(1L), group_by = "category")
    #' }
    recommend_groups = function(collection_name, positive, group_by,
                                negative = NULL, group_size = 3, limit = 10,
                                filter = NULL, with_payload = TRUE,
                                with_vector = FALSE, score_threshold = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(positive), length(positive) > 0,
                is.character(group_by), nzchar(group_by))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/recommend/groups")
      body <- list(positive = positive, group_by = group_by,
                   group_size = group_size, limit = limit,
                   with_payload = with_payload, with_vector = with_vector)
      if (!is.null(negative))        body$negative        <- negative
      if (!is.null(filter))          body$filter          <- filter
      if (!is.null(score_threshold)) body$score_threshold <- score_threshold
      private$.req("POST", url, body)
    },

    # -------------------------------------------------------------------------
    # Discover
    # -------------------------------------------------------------------------

    #' @description
    #' Discover points using a target and context pairs (look-alike search).
    #'
    #' @param collection_name Name of the collection.
    #' @param target Point ID or vector to use as the discovery target.
    #' @param context List of context pairs, each a named list with
    #'   \code{positive} and \code{negative} keys.
    #' @param filter (Optional) Named list filter expression.
    #' @param params (Optional) Named list of search parameters.
    #' @param limit Integer. Maximum results (default 10).
    #' @param offset Integer. Results to skip (default 0).
    #' @param with_payload Logical or field selector.
    #' @param with_vector Logical or vector names.
    #' @param using (Optional) Character. Named vector field.
    #' @param lookup_from (Optional) Named list. Look up from another collection.
    #'
    #' @return API response list with discovered points.
    #'
    #' @examples
    #' \dontrun{
    #'   search$discover_points("my_collection",
    #'     target  = 1L,
    #'     context = list(list(positive = 2L, negative = 3L)),
    #'     limit   = 5)
    #' }
    discover_points = function(collection_name, target, context,
                               filter = NULL, params = NULL, limit = 10,
                               offset = 0, with_payload = TRUE,
                               with_vector = FALSE, using = NULL,
                               lookup_from = NULL) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(context))
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/discover")
      body <- list(target = target, context = context, limit = limit,
                   offset = offset, with_payload = with_payload,
                   with_vector = with_vector)
      if (!is.null(filter))      body$filter      <- filter
      if (!is.null(params))      body$params      <- params
      if (!is.null(using))       body$using       <- using
      if (!is.null(lookup_from)) body$lookup_from <- lookup_from
      private$.req("POST", url, body)
    },

    #' @description
    #' Batch discovery requests in one round-trip.
    #'
    #' @param collection_name Name of the collection.
    #' @param searches List of discover request objects.
    #'
    #' @return API response list with one result set per request.
    #'
    #' @examples
    #' \dontrun{
    #'   search$discover_batch("my_collection", searches = list(
    #'     list(target = 1L, context = list(list(positive = 2L, negative = 3L)))
    #'   ))
    #' }
    discover_batch = function(collection_name, searches) {
      stopifnot(is.character(collection_name), nzchar(collection_name),
                is.list(searches), length(searches) > 0)
      url  <- paste0(private$.base_url(),
                     "/collections/", collection_name, "/points/discover/batch")
      body <- list(searches = searches)
      private$.req("POST", url, body)
    }
  )
)

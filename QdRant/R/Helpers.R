# ── Filters ──────────────────────────────────────────────────────────────────

#' Build a Qdrant filter from clause functions
#'
#' @param ... One or more clause objects created by \code{must()}, \code{should()}, or \code{must_not()}.
#' @return A named list suitable for the \code{filter} parameter.
#' @export
#' @examples
#' qdrant_filter(
#'   must(be("color", "red")),
#'   must_not(be("color", "black"))
#' )
qdrant_filter <- function(...) {
  f <- list()
  for (clause in list(...)) {
    f[[clause$type]] <- clause$conditions
  }
  f
}

#' All conditions must match
#'
#' @param ... One or more conditions (e.g. from \code{be()}, \code{range_filter()}).
#' @return A clause object for use in \code{qdrant_filter()}.
#' @export
#' @examples
#' must(be("color", "red"), range_filter("price", lte = 100))
must <- function(...) {
  structure(list(type = "must", conditions = list(...)), class = "qdrant_clause")
}

#' At least one condition must match
#'
#' @param ... One or more conditions.
#' @return A clause object for use in \code{qdrant_filter()}.
#' @export
#' @examples
#' should(be("color", "red"), be("color", "blue"))
should <- function(...) {
  structure(list(type = "should", conditions = list(...)), class = "qdrant_clause")
}

#' None of the conditions may match
#'
#' @param ... One or more conditions.
#' @return A clause object for use in \code{qdrant_filter()}.
#' @export
#' @examples
#' must_not(be("color", "black"), be("color", "brown"))
must_not <- function(...) {
  structure(list(type = "must_not", conditions = list(...)), class = "qdrant_clause")
}

#' Match a field against a value or value descriptor
#'
#' Pass a scalar or vector for exact/any-of matching. Pass a value descriptor
#' (\code{in_range()}, \code{in_datetime_range()}, \code{in_geo_radius()},
#' \code{in_geo_bbox()}, \code{is_null()}, \code{is_empty()}) for other
#' condition types.
#'
#' @param key    Field name.
#' @param values A scalar, vector, or value descriptor.
#' @return A condition for use inside \code{must()}, \code{should()}, or \code{must_not()}.
#' @export
#' @examples
#' be("color", "red")
#' be("color", c("red", "blue"))
#' be("price",    in_range(gte = 10, lte = 100))
#' be("location", in_geo_radius(lon = -73.99, lat = 40.73, radius = 500))
#' be("created_at", in_datetime_range(gte = "2024-01-01T00:00:00Z"))
#' be("description", is_null())
#' be("tags", is_empty())
be <- function(key, values) {
  if (inherits(values, "qdrant_in_range") ||
      inherits(values, "qdrant_in_datetime_range")) {
    structure(list(key = key, range = values$range), class = "qdrant_condition")
  } else if (inherits(values, "qdrant_in_geo_radius")) {
    structure(list(key = key, geo_radius = values$geo_radius),
              class = "qdrant_condition")
  } else if (inherits(values, "qdrant_in_geo_bbox")) {
    structure(list(key = key, geo_bounding_box = values$geo_bounding_box),
              class = "qdrant_condition")
  } else if (inherits(values, "qdrant_is_null")) {
    structure(list(is_null = list(key = key)), class = "qdrant_condition")
  } else if (inherits(values, "qdrant_is_empty")) {
    structure(list(is_empty = list(key = key)), class = "qdrant_condition")
  } else if (length(values) == 1) {
    structure(list(key = key, match = list(value = values[[1]])),
              class = "qdrant_condition")
  } else {
    structure(list(key = key, match = list(any = as.list(values))),
              class = "qdrant_condition")
  }
}

#' Numeric range descriptor for use inside \code{be()}
#'
#' @param gte Greater-than-or-equal bound (optional).
#' @param gt  Greater-than bound (optional).
#' @param lte Less-than-or-equal bound (optional).
#' @param lt  Less-than bound (optional).
#' @return A value descriptor for \code{be()}.
#' @export
#' @examples
#' must(be("price", in_range(gte = 10, lte = 100)))
#' must(be("score", in_range(gt = 0.5)))
in_range <- function(gte = NULL, gt = NULL, lte = NULL, lt = NULL) {
  r <- list()
  if (!is.null(gte)) r$gte <- gte
  if (!is.null(gt))  r$gt  <- gt
  if (!is.null(lte)) r$lte <- lte
  if (!is.null(lt))  r$lt  <- lt
  structure(list(range = r), class = "qdrant_in_range")
}

#' Datetime range descriptor for use inside \code{be()}
#'
#' @param gte ISO 8601 lower bound (optional).
#' @param gt  ISO 8601 lower bound exclusive (optional).
#' @param lte ISO 8601 upper bound (optional).
#' @param lt  ISO 8601 upper bound exclusive (optional).
#' @return A value descriptor for \code{be()}.
#' @export
#' @examples
#' must(be("created_at", in_datetime_range(gte = "2024-01-01T00:00:00Z")))
in_datetime_range <- function(gte = NULL, gt = NULL, lte = NULL, lt = NULL) {
  r <- list()
  if (!is.null(gte)) r$gte <- gte
  if (!is.null(gt))  r$gt  <- gt
  if (!is.null(lte)) r$lte <- lte
  if (!is.null(lt))  r$lt  <- lt
  structure(list(range = r), class = "qdrant_in_datetime_range")
}

#' Geographic radius descriptor for use inside \code{be()}
#'
#' @param lon    Longitude of the center point.
#' @param lat    Latitude of the center point.
#' @param radius Radius in meters.
#' @return A value descriptor for \code{be()}.
#' @export
#' @examples
#' must(be("location", in_geo_radius(lon = -73.99, lat = 40.73, radius = 500)))
in_geo_radius <- function(lon, lat, radius) {
  structure(
    list(geo_radius = list(center = list(lon = lon, lat = lat), radius = radius)),
    class = "qdrant_in_geo_radius"
  )
}

#' Geographic bounding box descriptor for use inside \code{be()}
#'
#' @param top_left_lon     Longitude of the top-left corner.
#' @param top_left_lat     Latitude of the top-left corner.
#' @param bottom_right_lon Longitude of the bottom-right corner.
#' @param bottom_right_lat Latitude of the bottom-right corner.
#' @return A value descriptor for \code{be()}.
#' @export
#' @examples
#' must(be("location", in_geo_bbox(
#'   top_left_lon = -74.0, top_left_lat = 40.8,
#'   bottom_right_lon = -73.9, bottom_right_lat = 40.7)))
in_geo_bbox <- function(top_left_lon, top_left_lat,
                        bottom_right_lon, bottom_right_lat) {
  structure(
    list(geo_bounding_box = list(
      top_left     = list(lon = top_left_lon,     lat = top_left_lat),
      bottom_right = list(lon = bottom_right_lon, lat = bottom_right_lat)
    )),
    class = "qdrant_in_geo_bbox"
  )
}

#' Null field descriptor for use inside \code{be()}
#'
#' @return A value descriptor for \code{be()}.
#' @export
#' @examples
#' must(be("description", is_null()))
is_null <- function() {
  structure(list(), class = "qdrant_is_null")
}

#' Empty field descriptor for use inside \code{be()}
#'
#' @return A value descriptor for \code{be()}.
#' @export
#' @examples
#' must(be("tags", is_empty()))
is_empty <- function() {
  structure(list(), class = "qdrant_is_empty")
}

#' Filter by point ID
#'
#' @param ids Integer or character vector of point IDs.
#' @return A condition for use inside \code{must()}, \code{should()}, or \code{must_not()}.
#' @export
#' @examples
#' must(has_id(c(1L, 2L, 3L)))
has_id <- function(ids) {
  structure(list(has_id = as.list(ids)), class = "qdrant_condition")
}


# ── Vectors config ────────────────────────────────────────────────────────────

#' Build a dense vectors config for create_collection
#'
#' @param size     Integer. Number of dimensions.
#' @param distance Distance metric: \code{"Cosine"}, \code{"Dot"}, \code{"Euclid"}, or \code{"Manhattan"}.
#' @param on_disk  Logical. Store vectors on disk (optional).
#' @return A named list for the \code{vectors_config} parameter.
#' @export
#' @examples
#' vectors_config(384, "Cosine")
vectors_config <- function(size, distance = "Cosine", on_disk = NULL) {
  cfg <- list(size = size, distance = distance)
  if (!is.null(on_disk)) cfg$on_disk <- on_disk
  cfg
}

#' Build a named multi-vector config for create_collection
#'
#' @param ... Named arguments, each a \code{vectors_config()} result.
#' @return A named list for the \code{vectors_config} parameter.
#' @export
#' @examples
#' multi_vectors_config(
#'   image = vectors_config(512, "Cosine"),
#'   text  = vectors_config(384, "Dot")
#' )
multi_vectors_config <- function(...) {
  list(...)
}


# ── Points ────────────────────────────────────────────────────────────────────

#' Build a single point for update_vectors or batch operations
#'
#' @param id      Point ID (integer or UUID string).
#' @param vector  Numeric vector, or named list of vectors for multi-vector collections.
#' @param payload Named list of payload fields (optional).
#' @return A named list.
#' @export
#' @examples
#' point(1L, c(0.1, 0.2, 0.3), list(color = "red"))
#' point(1L, list(image = c(0.1, 0.2), text = c(0.9, 0.8)))
point <- function(id, vector, payload = NULL) {
  p <- list(id = id, vector = vector)
  if (!is.null(payload)) p$payload <- payload
  p
}


# ── Query helpers ─────────────────────────────────────────────────────────────

#' Build a nearest-neighbour query
#'
#' @param vector Numeric vector to search by.
#' @return A query list for \code{query_points}.
#' @export
#' @examples
#' query_nearest(c(0.1, 0.2, 0.3))
query_nearest <- function(vector) {
  list(nearest = vector)
}

#' Build a fusion query (RRF or DBSF)
#'
#' @param method \code{"rrf"} (Reciprocal Rank Fusion) or \code{"dbsf"}.
#' @return A query list for \code{query_points}.
#' @export
#' @examples
#' query_fusion("rrf")
query_fusion <- function(method = "rrf") {
  list(fusion = method)
}

#' Build a prefetch clause for multi-stage search
#'
#' @param query  A query list (e.g. from \code{query_nearest()}).
#' @param limit  Integer. Number of candidates to fetch.
#' @param filter A filter (optional, from \code{qdrant_filter()}).
#' @param using  Character. Which vector to use (optional, for multi-vector collections).
#' @return A prefetch list for the \code{prefetch} parameter of \code{query_points}.
#' @export
#' @examples
#' prefetch(query_nearest(c(0.1, 0.2)), limit = 50)
prefetch <- function(query, limit = 100, filter = NULL, using = NULL) {
  p <- list(query = query, limit = limit)
  if (!is.null(filter)) p$filter <- filter
  if (!is.null(using))  p$using  <- using
  p
}

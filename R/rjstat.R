#' Read and write JSON-stat data sets
#'
#' \href{http://json-stat.org}{JSON-stat} is a JSON format for data
#' dissemination. The \pkg{rjstat} package converts data frames to and from this
#' format. The extensive metadata features of JSON-stat are not supported.
#'
#' @docType package
#' @name rjstat
#' @import jsonlite
#' @import checkmate
NULL

#' Convert JSON-stat format to data frame(s)
#'
#' This function takes a JSON-stat format response and returns a data frame or a
#' list of data frames, with columns for each dimension and one \code{value}
#' column.
#'
#' @param x JSON-stat format response, or path or URL to such a response
#' @param naming whether to use (longer) \code{label}s or (shorter) \code{id}s
#' @param use_factors whether dimension categories should be factors or
#'   character objects
#' @param silent suppress warnings
#'
#' @return For responses with class \code{dataset}: A data frame. For responses
#'   with class \code{collection}: An unnamed list of one or more lists or data
#'   frames. For responses with class \code{bundle}: A named list of one or more
#'   data frames.
#'
#' @export
#' @examples
#' \dontrun{
#' oecd.canada.url <- "http://json-stat.org/samples/oecd-canada.json"
#' results <- fromJSONstat(oecd.canada.url)
#' names(results)
#' }
fromJSONstat <- function(x, naming = "label", use_factors = FALSE,
                         silent = FALSE) {
    assert_choice(naming, c("label", "id"))
    assert_flag(use_factors)
    assert_flag(silent)

    x <- fromJSON(x, simplifyDataFrame = FALSE)

    parse_list(x, naming, use_factors, silent)
}

parse_list <- function(x, naming, use_factors, silent) {
    assert_list(x, min.len = 1)
    if (identical(x$class, "dataset")) {
        parse_dataset_class(x, naming, use_factors, silent)
    } else if (identical(x$class, "dimension")) {
        parse_dimension_class(x, naming, use_factors, silent)
    } else if (identical(x$class, "collection")) {
        parse_collection_class(x, naming, use_factors, silent)
    } else {
        parse_bundle_class(x, naming, use_factors)
    }
}

parse_dataset_class <- function(x, naming, use_factors, silent) {
    if (is.null(x$value)) {
        if (!silent) {
            warning("no values in dataset, returning unparsed list",
                    call. = FALSE)
        }
        x
    } else {
        parse_dataset(x, naming, use_factors)
    }
}

parse_dimension_class <- function(x, naming, use_factors, silent) {
    if (!silent) {
        warning("dimension responses not implemented, returning unparsed list",
                call. = FALSE)
    }
    x
}

parse_collection_class <- function(x, naming, use_factors, silent) {
    if (is.null(x$link$item)) {
        if (!silent) {
            warning("no link items in collection, returning unparsed list",
                    call. = FALSE)
        }
        x
    } else {
        lapply(x$link$item, parse_list, naming, use_factors, silent)
    }
}

parse_bundle_class <- function(x, naming, use_factors) {
    datasets <- lapply(x, parse_dataset, naming, use_factors)

    if (identical(naming, "label")) {
        names(datasets) <- get_labels(x)
    }

    datasets
}

parse_dataset <- function(dataset, naming, use_factors) {
    assert_list(dataset, min.len = 2)

    sizes <- as.integer(dataset$size)
    if (length(sizes) < 1) {
        sizes <- as.integer(dataset$dimension$size)
    }
    assert_integer(sizes, lower = 1, any.missing = FALSE, min.len = 1)

    n_rows <- prod(sizes)
    n_dims <- length(sizes)

    dimension_ids <- as.character(dataset$id)
    if (length(dimension_ids) < 1) {
        dimension_ids <- as.character(dataset$dimension$id)
    }
    assert_character(dimension_ids, min.chars = 1, any.missing = FALSE,
                     len = n_dims, unique = TRUE)

    dimensions <- dataset$dimension
    assert_list(dimensions, min.len = n_dims)
    assert_subset(dimension_ids, names(dimensions))
    dimensions <- dimensions[dimension_ids]

    dimension_categories <- lapply(dimensions, parse_dimension, naming)
    assert_list(dimension_categories, types = "character", len = n_dims,
                names = "unique")

    s <- vapply(dimension_categories, length, integer(1))
    assert_set_equal(sizes, s, ordered = TRUE)

    dataframe <- rev(expand.grid(rev(dimension_categories),
                                 stringsAsFactors = use_factors))
    if (identical(naming, "label")) {
        names(dataframe) <- get_labels(dimensions)
    }
    assert_data_frame(dataframe, types = c("character", "factor"),
                      any.missing = FALSE, nrows = n_rows, ncols = n_dims)

    values <- dataset$value
    if (is.list(values)) {
        values <- unlist(values)
        v <- rep(NA, n_rows)
        i <- as.integer(names(values)) + 1L
        assert_integer(i, lower = 1, upper = n_rows, any.missing = FALSE,
                       max.len = n_rows, unique = TRUE)
        v[i] <- values
        values <- v
    }
    assert_atomic(values, len = n_rows)

    dataframe <- cbind(dataframe, value = values, stringsAsFactors = FALSE)
    assert_data_frame(dataframe, types = "atomic", nrows = n_rows,
                      ncols = n_dims + 1)

    dataframe
}

parse_dimension <- function(dimension, naming) {
    assert_list(dimension, min.len = 1)

    categories <- dimension$category
    assert_list(categories, min.len = 1)

    index <- categories$index
    labels <- categories$label

    if (is.null(index)) {
        parse_no_index(labels, naming)
    } else if (is.list(index)) {
        parse_object_index(index, labels, naming)
    } else {
        parse_array_index(index, labels, naming)
    }
}

parse_no_index <- function(labels, naming) {
    assert_list(labels, len = 1)
    if (identical(naming, "label")) {
        categories <- as.character(unlist(labels))
    } else {
        categories <- names(labels)
    }
    assert_character(categories, min.chars = 1, any.missing = FALSE, len = 1)
    categories
}

parse_object_index <- function(index, labels, naming) {
    categories <- names(sort(unlist(index)))
    if (identical(naming, "label") && setequal(names(labels), categories)) {
        categories <- as.character(unlist(labels[categories]))
    }
    assert_character(categories, min.chars = 1, any.missing = FALSE,
                     min.len = 1)
    categories
}

parse_array_index <-  function(index, labels, naming) {
    categories <- as.character(index)
    if (identical(naming, "label") && setequal(names(labels), categories)) {
        categories <- as.character(unlist(labels[categories]))
    }
    assert_character(categories, min.chars = 1, any.missing = FALSE,
                     min.len = 1)
    categories
}

get_labels <- function(x) {
    labels <- lapply(x, getElement, "label")
    i <- vapply(labels, is.null, logical(1))
    labels[i] <- names(labels)[i]
    as.character(labels)
}

#' Convert data frame(s) to JSON-stat format
#'
#' This function takes a data frame or list of data frames and returns
#' a string representation in JSON-stat format. The input data frame(s)
#' must be in maximally long tidy format: with only one \code{value}
#' column and all other columns representing dimensions.
#'
#' @param x a data frame or list of data frames
#' @param value name of value column
#' @param ... arguments passed on to \code{\link[jsonlite]{toJSON}}
#'
#' @return For a data frame: A JSON-stat format response with class
#'   \code{dataset}. For a list of data frames: A JSON-stat format response with
#'   class \code{collection}.
#'
#' @export
#' @examples
#' library(reshape)
#' irises <- melt(cbind(iris, Specimen=rep(1:50, 3)),
#'                id.vars=c("Species", "Specimen"))
#' irisJSONstat <- toJSONstat(list(iris=irises))
#' cat(substr(irisJSONstat, 1, 76))
#'
#' # Add indentation whitespace
#' toJSONstat(as.data.frame(Titanic), value = "Freq", pretty = TRUE)
toJSONstat <- function(x, value = "value", ...) {
    datasets <- c(list(version = unbox("2.0")),
                  unravel(x, value))

    toJSON(datasets, na = "null", ...)
}

unravel <- function(x, value) {
    assert(checkDataFrame(x), checkList(x))
    if (is.data.frame(x)) {
        unravel_dataset(x, value)
    } else {
        assert_list(x, min.len = 1)
        list(class = unbox("collection"),
             link = list(item = unname(lapply(x, unravel, value))))
    }
}

unravel_dataset <- function(dataset, value) {
    assert_data_frame(dataset, types = "atomic", min.rows = 1, min.cols = 2,
                      col.names = "unique")
    assert_choice(value, names(dataset))

    i <- vapply(dataset, is.raw, logical(1))
    dataset[i] <- lapply(dataset[i], as.character)

    dimensions <- dataset[names(dataset) != value]
    assert_data_frame(dimensions, any.missing = FALSE)
    j <- !vapply(dimensions, is.factor, logical(1))
    dimensions[j] <- lapply(dimensions[j], factor)

    dimension_sizes <- vapply(dimensions, nlevels, integer(1))
    dimension_ids <- names(dimensions)
    categories <- lapply(dimensions, function(dimension) {
        list(category = list(index = levels(dimension)))
    })

    dim_factors <- c(rev(cumprod(rev(dimension_sizes)))[-1], 1)

    sort_table <- lapply(dimensions, function(dimension) {
        unclass(dimension) - 1
    })
    sort_table <- Map(`*`, sort_table, dim_factors)

    sort_index <- Reduce(`+`, sort_table) + 1

    if (any(duplicated(sort_index))) {
        stop("non-value columns must constitute a unique ID", call. = FALSE)
    }

    n <- prod(dimension_sizes)
    values <- dataset[[value]]
    if (length(values) == n) {
        values[sort_index] <- values
    } else {
        values <- lapply(values, unbox)
        names(values) <- sort_index - 1
    }

    datalist <- list(class = unbox("dataset"),
                     id = dimension_ids,
                     size = dimension_sizes,
                     value = values,
                     dimension = categories)

    datalist
}

#' Read and write JSON-stat data sets
#'
#' \href{http://json-stat.org}{JSON-stat} is a JSON format for data
#' dissemination. The \pkg{rjstat} package converts data frames to and from this
#' format. The extensive metadata features of JSON-stat are not supported.
#'
#' @docType package
#' @name rjstat
#' @import jsonlite
#' @import assertthat
NULL

#' Convert JSON-stat format to data frame(s)
#'
#' This function takes characters of a JSON-stat format response (or path
#' or URL to such a response) and returns a list of data frames with
#' columns for each dimension and one \code{value} column.
#'
#' @param x the JSON-stat format in characters (or path or URL)
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
    assert_that(is.string(naming))
    if (!naming %in% c("label", "id")) {
        stop('naming must be "label" or "id"', call. = FALSE)
    }

    assert_that(is.flag(use_factors))
    assert_that(noNA(use_factors))

    assert_that(is.flag(silent))
    assert_that(noNA(silent))

    x <- fromJSON(x, simplifyDataFrame = FALSE)

    parse_list(x, naming, use_factors, silent)
}

parse_list <- function(x, naming, use_factors, silent) {
    if (identical(x$class, "dataset")) {
        parse_dataset(x, naming, use_factors, silent)
    } else if (identical(x$class, "dimension")) {
        parse_dimension(x, naming, use_factors, silent)
    } else if (identical(x$class, "collection")) {
        parse_collection(x, naming, use_factors, silent)
    } else {
        parse_bundle(x, naming, use_factors)
    }
}

sw <- function(msg, silent) {
    if (!silent) warning(msg, call. = FALSE)
}

parse_dataset <- function(x, naming, use_factors, silent) {
    if (is.null(x$value)) {
        sw("no values in dataset, returning unparsed list", silent)
        x
    } else {
        .parse_dataset(x, naming, use_factors)
    }
}

parse_dimension <- function(x, naming, use_factors, silent) {
    sw("dimension responses not implemented, returning unparsed list", silent)
    x
}

parse_collection <- function(x, naming, use_factors, silent) {
    if (is.null(x$link$item)) {
        sw("no link items in collection, returning unparsed list", silent)
        x
    } else {
        lapply(x$link$item, parse_list, naming, use_factors, silent)
    }
}

parse_bundle <- function(x, naming, use_factors) {
    datasets <- lapply(x, .parse_dataset, naming, use_factors)

    if (identical(naming, "label")) {
        names(datasets) <- .get_labels(x)
    }

    datasets
}

.parse_dataset <- function(dataset, naming, use_factors) {
    sizes <- as.integer(dataset$size)
    if (length(sizes) < 1) {
        sizes <- as.integer(dataset$dimension$size)
    }
    assert_that(length(sizes) > 0)
    n_rows <- prod(sizes)

    dimension_ids <- dataset$id
    if (is.null(dimension_ids)) {
        dimension_ids <- dataset$dimension$id
    }
    assert_that(!is.null(dimension_ids))
    assert_that(!any(duplicated(dimension_ids)))
    dimensions <- dataset$dimension[dimension_ids]

    dimension_categories <- lapply(dimensions, .parse_dimension, naming)

    s <- vapply(dimension_categories, length, integer(1), USE.NAMES = FALSE)
    assert_that(identical(sizes, s))

    if (identical(naming, "label")) {
        names(dimension_categories) <- .get_labels(dimensions)
    }

    dataframe <- rev(expand.grid(rev(dimension_categories),
                                 stringsAsFactors = use_factors))

    values <- dataset$value
    if (is.list(values)) {
        values <- unlist(values)
        v <- rep(NA, n_rows)
        i <- as.integer(names(values)) + 1
        suppressWarnings(assert_that(max(i) <= n_rows, min(i) > 0))
        v[i] <- values
        values <- v
    }
    assert_that(are_equal(length(values), n_rows))

    dataframe$value <- values

    dataframe
}

.parse_dimension <- function(dimension, naming) {
    index <- dimension$category$index
    labels <- dimension$category$label
    if (is.null(index)) {
        categories <- unlist(labels)
        if (identical(naming, "label")) {
            categories <- unname(categories)
        } else {
            categories <- names(categories)
        }
    } else if (is.list(index)) {
        categories <- sort(unlist(index))
        if (identical(naming, "label") && identical(length(labels),
                                                    length(categories))) {
            labels <- unlist(labels)
            categories[names(labels)] <- labels
            categories <- unname(categories)
        } else {
            categories <- names(categories)
        }
    } else {
        categories <- index
        if (identical(naming, "label") && identical(length(labels),
                                                    length(categories))) {
            categories <- unname(unlist(labels)[categories])
        }
    }
    assert_that(!is.null(categories))
    categories
}

.get_labels <- function(x) {
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
#' @export
#' @examples
#' library(reshape)
#' irises <- melt(cbind(iris, Specimen=rep(1:50, 3)),
#'                id.vars=c("Species", "Specimen"))
#' irisJSONstat <- toJSONstat(list(iris=irises))
#' cat(substr(irisJSONstat, 1, 76))
toJSONstat <- function(x, value = "value", ...) {
    assert_that(is.data.frame(x) || is.list(x))

    assert_that(is.string(value))
    if (identical(value, "")) {
        value <- "value"
    }

    datasets <- c(list(version = unbox("2.0")),
                  unravel(x, value))

    toJSON(datasets, na = "null", ...)
}

unravel <- function(x, value) {
    if (is.data.frame(x)) {
        .unravel_dataset(x, value)
    } else if (is.list(x)) {
        assert_that(length(x) > 0)
        list(class = unbox("collection"),
             link = list(item = unname(lapply(x, unravel, value))))
    } else {
        stop("list element is not a data frame", call. = FALSE)
    }
}

.unravel_dataset <- function(dataset, value) {
    assert_that(is.data.frame(dataset))
    assert_that(nrow(dataset) > 0)
    assert_that(ncol(dataset) > 1)
    if (is.null(dataset[[value]])) {
        stop("\"", value, "\" is not a column in dataset", call. = FALSE)
    }
    if (any(duplicated(names(dataset)))) {
        stop("duplicated column names", call. = FALSE)
    }
    if (any(!vapply(dataset, is.atomic, logical(1)))) {
        stop("all columns must be atomic", call. = FALSE)
    }
    if (any(vapply(dataset, is.raw, logical(1)))) {
        stop("columns can't be of type \"raw\"", call. = FALSE)
    }

    dimensions <- dataset[names(dataset) != value]
    assert_that(noNA(dimensions))
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

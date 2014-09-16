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
#' This function takes characters of a JSON-stat format response (or the
#' corresponding R list equivalent) and returns a list of data frames with
#' columns for each dimension and one \code{value} column.
#'
#' @param x the JSON-stat format in characters (or R list equivalent)
#' @param naming whether to use (longer) \code{label}s or (shorter) \code{id}s
#' @param use_factors whether dimension categories should be factors or
#'   character objects
#'
#' @export
#' @examples
#' \dontrun{
#' oecd.canada.url <- "http://json-stat.org/samples/oecd-canada.json"
#' results <- fromJSONstat(readLines(oecd.canada.url))
#' names(results)
#' }
fromJSONstat <- function(x, naming = "label", use_factors = FALSE) {
    assert_that(is.character(x))
    assert_that(length(x) > 0)
    if (length(x) > 1) {
        x <- paste(x, collapse = " ")
    }

    assert_that(is.string(naming))
    if (!naming %in% c("label", "id")) {
        stop('naming must be "label" or "id"', call. = FALSE)
    }

    assert_that(is.flag(use_factors))
    assert_that(noNA(use_factors))

    x <- fromJSON(x)

    dataset_labels <- unlist(lapply(x, getElement, "label"))

    datasets <- lapply(x, .parse_dataset, naming, use_factors)

    if (identical(naming, "label") && !is.null(dataset_labels)) {
        names(datasets) <- dataset_labels
    }

    datasets
}

.parse_dataset <- function(dataset, naming, use_factors) {
    n_rows <- prod(dataset$dimension$size)

    dimension_ids <- dataset$dimension$id
    assert_that(!is.null(dimension_ids))
    dimensions <- dataset$dimension[dimension_ids]
    dimension_labels <- unlist(lapply(dimensions, getElement, "label"))

    each <- c(rev(cumprod(rev(dataset$dimension$size))), 1)[-1]

    dimension_categories <- lapply(dimensions, .parse_dimension,
                                   naming, use_factors)

    assert_that(are_equal(dataset$dimension$size,
                          vapply(dimension_categories, length, 0,
                                 USE.NAMES = FALSE)))

    dimension_table <- Map(rep, dimension_categories, each = each,
                           length.out = n_rows)

    if (identical(naming, "label") && !is.null(dimension_labels)) {
        names(dimension_table) <- dimension_labels
    } else {
        names(dimension_table) <- dimension_ids
    }

    value <- dataset$value
    if (is.list(value)) {
        if (identical(length(value), 1L)) {
            if (is.null(value[[1]])) {
                value <- rep(NA_real_, n_rows)
            } else {
                value <- rep(value[[1]], n_rows)
            }
        } else {
            v <- rep(NA_real_, n_rows)
            i <- as.integer(names(value)) + 1
            assert_that(max(i) <= n_rows, min(i) > 0)
            for (j in 1:length(i)) {
                if (!is.null(value[[j]])) {
                    v[i[j]] <- value[[j]]
                }
            }
            value <- v
        }
    }

    data_frame <- c(dimension_table, list(value = value))
    class(data_frame) <- "data.frame"
    attr(data_frame, "row.names") <- .set_row_names(length(data_frame[[1]]))

    attr(data_frame, "source") <- dataset$source
    attr(data_frame, "updated") <- dataset$updated
    data_frame
}

.parse_dimension <- function(dimension, naming, use_factors) {
    index <- dimension$category$index
    labels <- dimension$category$label
    if (is.null(index)) {
        categories <- unlist(labels)
        if (identical(naming, "label")) {
            categories <- unname(categories)
        } else {
            categories <- names(categories)
        }
    } else {
        categories <- names(index)
        if (is.null(categories)) {
            categories <- index
        }
        if (!is.null(labels) && identical(naming, "label")) {
            categories <- unname(unlist(labels))
        }
    }
    assert_that(!is.null(categories))
    if (use_factors) {
        categories <- factor(categories, levels = categories)
    }
    categories
}

#' Convert data frame(s) to JSON-stat format
#'
#' This function takes a data frame or list of data frames and returns
#' a string representation in JSON-stat format. The input data frame(s)
#' must be in maximally long tidy format: with only one \code{value}
#' column and all other columns representing dimensions. The reserved
#' words \code{id}, \code{size} and \code{role} are not allowed
#' column names.
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

    if (is.data.frame(x)) {
        x <- list(dataset = x)
    }

    assert_that(length(x) > 0)

    datasets <- lapply(x, .unravel_dataset, value)

    dataset_names <- names(datasets)
    default_names <- paste0("dataset", 1:length(datasets))
    default_names[1] <- "dataset"

    if (is.null(dataset_names)) {
        dataset_names <- default_names
    } else {
        i <- which(dataset_names == "")
        dataset_names[i] <- default_names[i]
        j <- which(duplicated(dataset_names))
        dataset_names[j]  <- paste0(dataset_names[j]," (",
                                    default_names[j], ")")
    }

    if (!identical(dataset_names, names(datasets))) {
        names(datasets) <- dataset_names
    }

    toJSON(datasets, na = "null", ...)
}

.unravel_dataset <- function(dataset, value) {
    assert_that(is.data.frame(dataset))
    assert_that(nrow(dataset) > 0)
    assert_that(ncol(dataset) > 1)
    if (is.null(dataset[[value]])) {
        stop("\"", value, "\" is not a column in dataset", call. = FALSE)
    }
    if (any(colnames(dataset) %in% c("id", "size", "role"))) {
        stop("\"id\", \"size\" and \"role\" are not allowed column names",
             call. = FALSE)
    }

    i <- which(colnames(dataset) == value)
    if (ncol(dataset) > 2) {
        dimensions <- lapply(dataset[, -i], function(dimension) {
            if (is.factor(dimension)) {
                dimension
            } else {
                factor(dimension)
            }
        })
    } else {
        if (is.factor(dataset[, -i])) {
            dimensions <- list(dataset[, -i])
        } else {
            dimensions <- list(factor(dataset[, -i]))
        }
        names(dimensions) <- colnames(dataset)[-i]
    }
    class(dimensions) <- "data.frame"
    attr(dimensions, "row.names") <- .set_row_names(length(dimensions[[1]]))

    if (!all(!duplicated(dimensions))) {
        stop("non-value columns must constitute a unique ID", call. = FALSE)
    }

    dimension_sizes <- vapply(dimensions, nlevels, integer(1))
    dimension_ids <- names(dimensions)
    categories <- lapply(dimensions, function(dimension) {
        list(category = list(index = levels(dimension)))
    })

    dimension_list <- c(categories,
                        list(id = dimension_ids,
                             size = dimension_sizes))

    dim_factors <- c(rev(cumprod(rev(dimension_sizes)))[-1], 1)
    dim_factors <- as.integer(dim_factors)

    sort_table <- lapply(dimensions, function(dimension) {
        unclass(dimension) - 1L
    })
    sort_table <- Map(`*`, sort_table, dim_factors)

    sort_index <- Reduce(`+`, sort_table) + 1L
    attributes(sort_index) <- NULL

    assert_that(!any(duplicated(sort_index)))

    n <- prod(dimension_sizes)
    values <- dataset[[value]]
    if (length(values) == n) {
        if (!identical(sort_index, 1:n)) {
            values[sort_index] <- values
        }
    } else {
        values <- lapply(values, unbox)
        names(values) <- sort_index - 1
    }

    datalist <- list(dimension = dimension_list,
                     value = values)

    if (!is.null(attr(dataset, "source"))) {
        datalist$source <- unbox(paste(attr(dataset, "source"),
                                       collapse = " "))
    }
    if (!is.null(attr(dataset, "updated"))) {
        datalist$updated <- unbox(paste(attr(dataset, "updated"),
                                        collapse = " "))
    }

    datalist
}

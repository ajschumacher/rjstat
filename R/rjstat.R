#' @import jsonlite
#' @import assertthat
NULL

#' Convert JSON-stat format to a list of data frames
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
fromJSONstat <- function(x, naming = "label", use_factors = F) {
    assert_that(is.character(x))
    assert_that(length(x) > 0)
    if (length(x) > 1) {
        x <- paste(x, collapse = " ")
    }

    assert_that(is.string(naming))
    if (!naming %in% c("label", "id")) {
        stop('naming must be "label" or "id"', call. = F)
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
    if (is.null(dimension_ids)) stop("corrupt input", call. = F)
    dimensions <- dataset$dimension[dimension_ids]
    dimension_labels <- unlist(lapply(dimensions, getElement, "label"))

    each <- c(rev(cumprod(rev(dataset$dimension$size))), 1)[-1]

    dimension_categories <- lapply(dimensions, .parse_dimension,
                                   naming, use_factors)

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
            for (j in 1:length(i))
                v[i[j]] <- value[[j]]
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
    if (is.null(categories)) stop("corrupt input", call. = F)
    if (use_factors) {
        categories <- factor(categories, levels = categories)
    }
    categories
}

#' Convert a (list of) data frame(s) to JSON-stat format
#'
#' to update~
#'
#' @param x a data frame or list of data frames
#' @param value name of value column
#'
#' @export
toJSONstat <- function(x, value="value") {
  if (class(x) == "data.frame") {
    x <- list(x)
  }
  resultList <- list()
  for (k in 1:length(x)) {
    dims <- x[[k]][, names(x[[k]]) != value]
    if (!all(!duplicated(dims))) {
      stop("non-value columns must constitute a unique ID")
    }
    jsList <- list()
    dimensions <- list()
    for (i in 1:length(dims)) {
      dim <- dims[[i]]
      dimName <- names(dims)[i]
      dimensions[[i]] <- unique(dim)
      jsList$dimension[[dimName]] <- list(category=list(index=as.character(dimensions[[i]])))
    }
    jsList$dimension[['id']] <- names(dims)
    dimSizes <- sapply(dimensions, function(x){return(length(x))})
    jsList$dimension[['size']] <- dimSizes
    baseSys <- c(sapply(1:length(dimSizes), function(j){return(prod(dimSizes[j:length(dimSizes)]))}),1)
    values <- x[[k]][[value]]
    valuesList <- lapply(1:prod(dimSizes), function(x){return(NULL)})
    numDims <- length(dims)
    for (i in 1:length(values)) {
      index <- sum(sapply(1:numDims, function(j){return((which(dimensions[[j]]==dims[i,j])-1)*baseSys[j+1])})) + 1
      valuesList[[index]] <- values[i]
    }
    jsList[['value']] <- valuesList
    resultList[[k]] <- jsList
    names(resultList)[k] <- names(x)[k]
  }
  return(toJSON(resultList))
}

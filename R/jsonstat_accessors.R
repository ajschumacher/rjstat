#' \code{id} accessors of \code{jsonstat} objects
#'
#' @description
#' Access and change \code{id}s of jsonstat objects.
#'
#' @param x a \code{jsonstat_dataset} object
#' @param value a character vector of up to the same length as \code{id(x)}.
#'
#' @export
id <- function(x){
    assert_class(x, "jsonstat_dataset")
    x$id
}

#' @rdname id
#' @export
`id<-` <- function(x, value){
    assert_class(x, "jsonstat_dataset")
    assert_character(value,len = length(id(x)))
    names(value) <- id(x)
    x$id <- as.vector(value)
    names(x$dimension) <- as.vector(value[names(x$dimension)])
    x
}


#' \code{status} accessors of \code{jsonstat} objects
#'
#' @description
#' Access and change \code{status}s of jsonstat objects.
#'
#' @param x a \code{jsonstat_dataset} object
#' @param value a character vector of up to the same length as \code{as.vector(x)} or a named list with index as vector names.
#'
#' @export
status <- function(x){
    assert_class(x, "jsonstat_dataset")
    x$status
}

#' @rdname status
#' @export
`status<-` <- function(x, value){
    assert_class(x, "jsonstat_dataset")
    stopifnot(is.list(value) | is.character(value))
    max_len <- length(as.vector(x))
    if(is.list(value)){
        if(!is.null(names(value))){
            assert_subset(names(value), as.character(1:max_len))
            x$status <- value
        } else {
            stop("Not a named list! Use a character vector instead.")
        }
    }
    if(is.character(value)){
        if(length(value) == 1) {
            x$status <- value
        } else if(length(value) == max_len) {
            x$status <- value
        } else {
            stop("Not a correct length of 'value'.")
        }
    }
    x
}



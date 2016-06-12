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
#' @param value a character vector of up to the same length as \code{as.vector(x)} or a named vector with index as vector names.
#'
#' @export
status <- function(x){
    assert_class(x, "jsonstat_dataset")
    x$status
}
value <- "e"
names(value) <- "93"
#' @rdname status
#' @export
`status<-` <- function(x, value){
    assert_class(x, "jsonstat_dataset")
    max_len <- length(as.vector(x))
    assert_character(value, max.len = max_len)
    if(is.null(names(value))){
        if(length(value) == 1) {
            x$status <- value
        } else if(length(value) == max_len) {
            x$status <- value
        } else {
            stop()
        }
    } else {
        assert_subset(names(value), as.character(1:max_len))
        x$status <- value
    }
    x
}



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


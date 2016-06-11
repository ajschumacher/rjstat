#' @export
is.jsonstat <- function(x){
    inherits(x, "jsonstat")
}

#' @export
is.jsonstat_dataset <- function(x){
    inherits(x, "jsonstat_dataset")
}

#' @export
is.jsonstat_collection <- function(x){
    inherits(x, "jsonstat_collection")
}

#' @export
is.jsonstat_dimension <- function(x){
    inherits(x, "jsonstat_dimension")
}

#' @export
print.jsonstat <- function(x, ...){
    cat("JSON-stat ", x$class, " object v.", x$version, sep = "")
}

#' @export
print.jsonstat_dataset <- function(x, ...){
    NextMethod(x)
    if(!is.null(x$label)) cat("\n", x$label, sep="")
    cat("\ndimensions: ")
    cat(paste(x$id, "(", x$size, ")", sep=""), sep = ", ")
}

#' @export
dim.jsonstat_dataset <- function(x){
    x$size
}

#' @export
dimnames.jsonstat_dataset <- function(x){
    dn <- lapply(x$dimension,
                 FUN=function(X) {
                     idx <- names(X$category$index)
                     if(is.null(idx)) idx <- names(X$category$label)
                     idx
                     })
    dn[x$id]
}


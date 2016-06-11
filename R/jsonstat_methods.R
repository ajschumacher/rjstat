#' @export
print.jsonstat <- function(x, ...){
    cat("JSON-stat ", x$class, " object v.", x$version, sep = "")
}

#' @export
print.jsonstat_dataset <- function(x, ...){
    NextMethod(x)
    if(!is.null(x$label)) cat("\n", x$label, sep="")
    cat("\nsize: ")
    cat(paste(x$id, "(", x$size, ")", sep=""), sep = ", ")
}




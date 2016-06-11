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

#' @export
`dimnames<-.jsonstat_dataset` <- function (x, value){
    stopifnot(all(names(value) == names(dimnames(x))))
    for(i in seq_along(names(value))){ # i <- 2
        var_name <- names(value)[i]
        if(length(value[[i]]) == 1){
            x$dimension[[var_name]]$category$index <- NULL
            value_exist <- value[[i]] %in% names(x$dimension[[var_name]]$category$label)
            if(value_exist) {
                lab <- x$dimension[[var_name]]$category$label[value_exist]
            } else {
                lab <- list("")
            }
            names(lab) <- value[[i]]
            x$dimension[[var_name]]$category$label <- lab
        } else {
            has_label <- !is.null(x$dimension[[var_name]]$category$label)
            has_index <- !is.null(x$dimension[[var_name]]$category$index)

            if(has_index){
                idx <- as.list(0:(length(value[[i]])-1))
                names(idx) <- value[[i]]
                x$dimension[[var_name]]$category$index <- idx
            }
            if(has_label){
                lab_idx <- names(x$dimension[[var_name]]$category$label) %in% value[[i]]
                labs <- x$dimension[[var_name]]$category$label[lab_idx]
                new_labs <- !value[[i]] %in% names(x$dimension[[var_name]]$category$label)
                if(any(new_labs)) {
                    extra_lab <- as.list(rep("", sum(new_labs)))
                    names(extra_lab) <- value[[i]][new_labs]
                    labs <- c(labs, extra_lab)
                }
                x$dimension[[var_name]]$category$label <- labs
            }
        }
    }
    x
}

#' @export
as.array.jsonstat_dataset <- function(x, ...){
    array(data = x$value, dim = x$size, dimnames = dimnames(x)[x$id])
}

#' @export
`[.jsonstat_dataset` <- function(x, i, ..., drop = FALSE)
{
    jsarray <- as.array(x)
    subs <- array_to_jsonstat_helper(jsonstat_array = jsarray[i, ..., drop=FALSE])
    x$size <- subs$size
    x$value <- subs$value
    dimnames(x) <- subs$dimnames
    x
}

#' @export
`[[.jsonstat_dataset` <- function(x, i, ...)
{
    jsarray <- as.array(x)
    jsarray[i, ..., drop=FALSE]
}

array_to_jsonstat_helper <- function(jsonstat_array){
    res <- list()
    res$size <- dim(jsonstat_array)
    res$value <- as.vector(jsonstat_array)
    res$dimnames <- dimnames(jsonstat_array)
    res
}

#' @export
`[<-.jsonstat_dataset` <- function(x, i, ..., value){
    jsarray <- as.array(x)
    jsarray[i, ...] <- value
    subs <- array_to_jsonstat_helper(jsonstat_array = jsarray)
    x$value <- subs$value
    x
}

#' @export
as.data.frame.jsonstat_dataset <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors()){
    df <- parse_dataset(dataset = x, naming = "id", use_factors = stringsAsFactors)
    if(!is.null(row.names)) rownames(df) <- row.names
    df
}



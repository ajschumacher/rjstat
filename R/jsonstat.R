#' Create a JSON-stat object
#'
#' @param x a JSON-stat string, URL or file
#'
#' @return a JSON-stat object with subclass dataset, dimension or collection
#'
#' @examples
#' file_path <- system.file("example_files/oecd.json", package = "rjstat")
#' x <- as.jsonstat(file_path)
#' print(x)
#'
#' @export
as.jsonstat <-function(x){
    x <- fromJSON(x, simplifyDataFrame = FALSE)
    x <- parse_value(x)
    validate_jsonstat(x)
    class(x) <- c(paste0("jsonstat_", x$class), "jsonstat", "list")
    x
}

parse_value <- function(x){
    if(!is.null(names(x$value))){
        idx <- as.integer(names(x$value)) + 1
        vals <- unlist(x$value)
        x$value <- rep(NA, prod(x$size))
        if(length(vals) > 0) x$value[idx] <- vals
    }
    x
}

#' Function to validate json stat
#'
#' @description
#' Now this is just a simple light-weight validation.
#' Hopefully this can be complemented with a real json stat
#' schema validator.
#'
#' @keywords internal
validate_jsonstat <- function(x){
    assert_subset(c("class", "version"), names(x))
    assert_set_equal(x$version, "2.0")
    assert_subset(x$class, c("dataset", "dimension", "collection"))
}



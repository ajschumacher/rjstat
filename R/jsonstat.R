#' Create a JSON-stat object
#'
#' @param x a JSON-stat string, URL or file
#'
#' @return a JSON-stat object with subclass dataset, dimension or collection
#'
#' @examples
#' oecd_jsonstat <- jsonstat("http://json-stat.org/samples/oecd.json")
#'
#' @export
jsonstat <-function(x){
    x <- fromJSON(x, simplifyDataFrame = FALSE)
    validate_jsonstat(x)
    class(x) <- c(x$class, "json-stat", "list")
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



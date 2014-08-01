#' @import rjson
NULL

#' Convert JSON-stat format to a list of data frames
#'
#' This function takes characters of a JSON-stat format response (or the
#' corresponding R list equivalent) and returns a list of data frames with
#' columns for each dimension and one \code{value} column.
#'
#' @param x the JSON-stat format in characters (or R list equivalent)
#' @param naming whether to use (longer) \code{label}s or (shorter) \code{id}s
#'
#' @export
fromJSONstat <- function(x, naming="label") {
  if (!naming %in% c("label", "id")) {
    stop('naming must be "label" or "id"')
  }
  if (class(x) == "character") {
    if (length(x) != 1) {
      x <- paste(x, collapse=" ")
    }
    x <- fromJSON(x)
  }
  result <- list()
  for (k in 1:length(x)) {
    jsList <- x[[k]]
    dimensions <- list()
    dimSizes <- jsList$dimension$size
    numDims <- length(dimSizes)
    baseSys <- c(sapply(1:numDims, function(i){return(prod(dimSizes[i:numDims]))}),1)

    for (i in 1:numDims) {
      thisDim <- jsList$dimension$id[[i]]
      thisDimName <- jsList$dimension[[thisDim]]$label
      if (is.null(thisDimName)) {
        thisDimName <- thisDim
      }
      thisDimSize <- jsList$dimension$size[[i]]
      thisDimLabel <- jsList$dimension[[thisDim]]$category$label
      if (!is.null(thisDimLabel)) {
        thisDimLabel <- data.frame(id=names(thisDimLabel),
                                   label=unlist(thisDimLabel),
                                   stringsAsFactors=FALSE)
      }
      thisDimIndex <- jsList$dimension[[thisDim]]$category$index
      if (is.null(thisDimIndex)) {
        thisDimIndex <- data.frame(id=thisDimLabel[1, "id"],
                                   index=0,
                                   stringsAsFactors=FALSE)
      } else {
        if (class(thisDimIndex)=="list") {
          thisDimIndex <- data.frame(id=names(thisDimIndex),
                                     index=unlist(thisDimIndex),
                                     stringsAsFactors=FALSE)
        } else {
          thisDimIndex <- data.frame(id=thisDimIndex,
                                     index=0:(length(thisDimIndex)-1),
                                     stringsAsFactors=FALSE)
        }
      }
      if (is.null(thisDimLabel)) {
        thisDimLabel <- data.frame(id=thisDimIndex$id,
                                   label=thisDimIndex$id,
                                   stringsAsFactors=FALSE)
      }
      thisDimAll <- merge(thisDimIndex, thisDimLabel)
      if (naming == "label") {
        dimensions[[i]] <- thisDimAll$label[order(thisDimAll$index)]
      } else {
        dimensions[[i]] <- thisDimAll$id[order(thisDimAll$index)]
      }
      if (naming == "label") {
        names(dimensions)[i] <- thisDimName
      } else {
        names(dimensions)[i] <- thisDim
      }
    }

    thisN <- length(jsList$value)
    output <- as.data.frame(matrix(data=integer(),
                                   nrow=thisN,
                                   ncol=length(dimensions)+1))
    names(output) <- c(names(dimensions), "value")

    theseVals <- jsList$value
    if (class(theseVals) == "list") {
      if (is.null(names(theseVals))) {
        indices <- 0:(thisN-1)
      } else {
        indices <- as.integer(names(theseVals))
      }
    } else {
      indices <- 0:(thisN-1)
      theseVals <- as.list(theseVals)
    }

    for (i in 1:thisN) {
      value <- theseVals[[i]]
      if (is.null(value)) {
        value <- NA
      }
      index <- indices[i]
      output[i,1:numDims] <- sapply(1:numDims, function(j){
        return(floor((index)/baseSys[j+1])%%dimSizes[j])
      })
      output[i,numDims+1] <- value
    }

    for (i in 1:numDims) {
      output[[i]] <- dimensions[[i]][output[[i]]+1]
    }

    result[[k]] <- output
    thisLabel <- jsList$label
    if (is.null(thisLabel) | naming == "id") {
      thisLabel <- names(x)[k]
    }
    names(result)[k] <- thisLabel
  }
  return(result)
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

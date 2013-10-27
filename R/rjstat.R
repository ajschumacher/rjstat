read.jsonstat <- function(jsonstat, naming="label") {
  if (!naming %in% c("label", "id")) {
    stop('naming must be "label" or "id"')
  }
  if (class(jsonstat) == "character") {
    if (length(jsonstat) != 1) {
      jsonstat <- paste(jsonstat, collapse=" ")
    }
    jsonstat <- fromJSON(jsonstat)
  }
  result <- list()
  for (k in 1:length(jsonstat)) {
    jsList <- jsonstat[[k]]
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
      indices <- as.integer(names(theseVals))
      theseVals <- unlist(theseVals)
    } else {
      indices <- 0:(thisN-1)
    }

    for (i in 1:thisN) {
      value <- theseVals[i]
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
      thisLabel <- names(jsonstat)[k]
    }
    names(result)[k] <- thisLabel
  }
  return(result)
}

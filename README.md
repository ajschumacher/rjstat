# rjstat: read and write JSON-stat with R
[![Build Status](https://travis-ci.org/ajschumacher/rjstat.svg)](https://travis-ci.org/ajschumacher/rjstat) [![Coverage Status](https://coveralls.io/repos/github/MansMeg/rjstat/badge.svg?branch=master)](https://coveralls.io/github/MansMeg/rjstat?branch=master) [![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/rjstat)](https://github.com/r-hub/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/rjstat)](https://CRAN.R-project.org/package=rjstat)

Read and write data sets in the [JSON-stat](https://json-stat.org/) format.


### Installation:

[From CRAN](https://cran.r-project.org/package=rjstat) (most people use this):

```s
install.packages('rjstat')
```

[From github](https://github.com/ajschumacher/rjstat) (development version):

```s
library(devtools)
install_github("ajschumacher/rjstat")
```


### Usage:

```s
library(rjstat)

oecd.canada.url <- "https://json-stat.org/samples/oecd-canada.json"

# Read from JSON-stat to a list of data frames:
results <- fromJSONstat(readLines(oecd.canada.url))
names(results)

## [1] "Unemployment rate in the OECD countries 2003-2014"
## [2] "Population by sex and age group. Canada. 2012"

# You can also read in using the typically terser IDs rather than labels.
results <- fromJSONstat(readLines(oecd.canada.url), naming="id")
names(results)

## [1] "oecd"   "canada"


# Convert from a list of data frames to a JSON-stat string.
# (The data frames must have exactly one value column.)
library(reshape)
irises <- melt(cbind(iris, Specimen=rep(1:50, 3)),
               id.vars=c("Species", "Specimen"))
irisJSONstat <- toJSONstat(list(iris=irises))
cat(substr(irisJSONstat, 1, 76))

## {"version":"2.0","class":"collection","link":{"item":[{"class":"dataset","id

# You can successfully convert back and forth, but only for the features that
# make sense in both R and JSON-stat.
head(fromJSONstat(irisJSONstat)[[1]])

##   Species Specimen     variable value
## 1  setosa        1 Sepal.Length   5.1
## 2  setosa        1  Sepal.Width   3.5
## 3  setosa        1 Petal.Length   1.4
## 4  setosa        1  Petal.Width   0.2
## 5  setosa        2 Sepal.Length   4.9
## 6  setosa        2  Sepal.Width   3.0
```

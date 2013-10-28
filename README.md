# rjstat: read and write JSON-stat with R

Read and write data sets in the [JSON-stat](http://json-stat.org/) format.

Installation:

```r
library(devtools)
install_github("rjstat", "ajschumacher")
```

Usage:

```r
library(rjstat)


# Read from JSON-stat to a list of data frames:
results <- fromJSONstat(readLines("http://json-stat.org/samples/oecd-canada.json"))
names(results)

## [1] "Unemployment rate in the OECD countries 2003-2014"
## [2] "Population by sex and age group. Canada. 2012"

# You can also read in using the typically terser IDs rather than labels.
results <- fromJSONstat(readLines("http://json-stat.org/samples/oecd-canada.json"), naming="id")
names(results)

## [1] "oecd"   "canada"


# Convert from a list of data frames to a JSON-stat string.
# (The data frames must have exactly one value column.)
library(reshape)
irises <- melt(cbind(iris, Specimen=rep(1:50, 3)), id.vars=c("Species", "Specimen"))
irisJSONstat <- toJSONstat(list(iris=irises))
cat(substr(irisJSONstat, 1, 80))

## {"iris":{"dimension":{"Species":{"category":{"index":["setosa","versicolor","vir
```

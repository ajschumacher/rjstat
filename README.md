# rjstat: read and write JSON-stat with R

Read and write data sets in the [JSON-stat](http://json-stat.org/) format. Currently reading mostly works but writing is not yet implemented.

Installation:

```r
library(devtools)
install_github("rjstat", "ajschumacher")
```

Usage:

```r
library(rjstat)
read.jsonstat(readLines("http://json-stat.org/samples/order.json"))

## $`Demo of value ordering: what does not change, first`
##    A: 3-categories dimension B: 2-categories dimension C: 4-categories dimension  value
## 1                          1                         1                         1 A1B1C1
## 2                          1                         1                         2 A1B1C2
## 3                          1                         1                         3 A1B1C3
## 4                          1                         1                         4 A1B1C4
## 5                          1                         2                         1 A1B2C1
## 6                          1                         2                         2 A1B2C2
## 7                          1                         2                         3 A1B2C3
## 8                          1                         2                         4 A1B2C4
## 9                          2                         1                         1 A2B1C1
## 10                         2                         1                         2 A2B1C2
## 11                         2                         1                         3 A2B1C3
## 12                         2                         1                         4 A1B1C4
## 13                         2                         2                         1 A2B2C1
## 14                         2                         2                         2 A2B2C2
## 15                         2                         2                         3 A2B2C3
## 16                         2                         2                         4 A2B2C4
## 17                         3                         1                         1 A3B1C1
## 18                         3                         1                         2 A3B1C2
## 19                         3                         1                         3 A3B1C3
## 20                         3                         1                         4 A3B1C4
## 21                         3                         2                         1 A3B2C1
## 22                         3                         2                         2 A3B2C2
## 23                         3                         2                         3 A3B2C3
## 24                         3                         2                         4 A3B2C4
```

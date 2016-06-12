context("jsonstat methods")

test_that("print", {
    expect_output(print(as.jsonstat("dataset.json")), regexp = "JSON-stat dataset")
    expect_output(print(as.jsonstat("collection.json")), regexp = "JSON-stat collection")
})

test_that("dim", {
    dim(as.jsonstat("oecd.json")) %>%
        expect_equal(c(1, 36, 12))
    dim(as.jsonstat("canada.json")) %>%
        expect_equal(c(1, 1, 20, 2, 3))
    dim(as.jsonstat("galicia.json")) %>%
        expect_equal(c(6, 22, 3, 2, 5, 1))
    dim(as.jsonstat("collection.json")) %>%
        expect_null()
})


test_that("dimnames", {
    test_dimnames_canada <- list(country = "CA")
    test_dimnames_canada$year <- "2012"
    test_dimnames_canada$age <- c("T", "4", "9", "14", "19", "24", "29", "34", "39", "44", "49", "54", "59", "64", "69", "74", "79", "84", "89", "older")
    test_dimnames_canada$concept <- c("POP", "PERCENT")
    test_dimnames_canada$sex <- c("T", "M", "F")

    dimnames(as.jsonstat("canada.json")) %>%
        expect_equal(test_dimnames_canada)
})


test_that("is methods", {
    x <- as.jsonstat("oecd.json")
    expect_true(is.jsonstat(x))
    expect_true(is.jsonstat_dataset(x))
    expect_false(is.jsonstat_collection(x))
    expect_false(is.jsonstat_dimension(x))

    x <- as.jsonstat("oecd-canada-col.json")
    expect_true(is.jsonstat(x))
    expect_false(is.jsonstat_dataset(x))
    expect_true(is.jsonstat_collection(x))
    expect_false(is.jsonstat_dimension(x))
})

test_that("extract", {
    x <- as.jsonstat("hierarchy.json")
    expect_identical(dim(x[2:3]), 2L)
    expect_identical(class(x[2:3]), class(x))
    expect_identical(as.vector(x[[2:3]]), c(NA,NA))
    expect_identical(class(x[[2:3]]), "array")

    x <- as.jsonstat("oecd.json")
    expect_identical(dim(x[1,5,1]), c(1L, 1L, 1L))
    expect_identical(class(x[1,5,1]), class(x))
    expect_identical(dim(x[1,2:5,1:3]), c(1L, 4L, 3L))
    expect_identical(class(x[1,2:5,1:3]), class(x))
    expect_identical(round(as.vector(x[[1,5,1]]), 4), 9.5434)
    expect_identical(class(x[[1,2:5,1:3]]), "array")
})

test_that("as.array", {
    x <- as.jsonstat("hierarchy.json")
    expect_class(as.array(x), "array")
    expect_identical(as.vector(as.array(x)), rep(NA, 132))

    x <- as.jsonstat("oecd.json")
    expect_class(as.array(x), "array")
    expect_identical(round(as.vector(as.array(x))[6:7],2), c(7.82, 5.34))

    x <- as.jsonstat("us-gsp.json")
    expect_identical(as.vector(as.array(x = x[1,2,])), c(45600.0, 0.31, 0.7, 65143))
})


test_that("as.array and as.data.frame", {
    x <- as.jsonstat("hierarchy.json")
    expect_class(as.data.frame(x), "data.frame")
    expect_identical(as.character(as.data.frame(x)$commodity[2:3]), c("1", "1.1"))
    expect_identical(as.data.frame(x)$value[2:3], rep(NA, 2))

    x <- as.jsonstat("oecd.json")
    expect_class(as.data.frame(x), "data.frame")
    expect_identical(as.character(as.data.frame(x)$concept[7]), "UNR")
    expect_identical(as.character(as.data.frame(x)$area[10]), "AU")
    expect_identical(as.character(as.data.frame(x)$year[1]), "2003")
    expect_identical(round(as.data.frame(x)$value[12],3), 5.463)
})

test_that("as.character", {
    x <- as.jsonstat("hierarchy.json")
    y <- as.jsonstat(as.character(x))
    expect_identical(x, y)

    x <- as.jsonstat("oecd.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("oecd-canada-col.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("canada.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("galicia.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("order.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("order.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("us-gsp.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("us-unr.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("us-labor.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)

    x <- as.jsonstat("collection_sample.json")
    y <- as.jsonstat(as.character(x, digits=10))
    expect_identical(x, y)
})

test_that("set values", {
    x <- as.jsonstat("oecd.json")
    expect_silent(x[1,1,1] <- 100.1)
    expect_identical(as.vector(x[[1,1,1]]), 100.1)
    expect_silent(x[1,1,3:5] <- 10:12)
    expect_identical(as.vector(x[[1,1,3:5]]), as.numeric(10:12))
})

test_that("identified bugs", {
    x <- as.jsonstat("oecd.json")
    expect_silent(as.data.frame(x[1,1,1]))
})

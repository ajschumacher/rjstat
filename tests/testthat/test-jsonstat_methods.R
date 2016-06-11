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

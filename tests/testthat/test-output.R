context("Output")

dataset <- readLines("dataset.json")

test_that("dataset names are correct", {
    expect_named(fromJSONstat(dataset, naming = "label"), "A dataset")
    expect_named(fromJSONstat(dataset, naming = "id"), "dataset")
})

test_that("column names are correct", {
    expect_named(fromJSONstat(dataset, naming = "label")[[1]],
                 c("A dimension", "Another dimension", "value"))
    expect_named(fromJSONstat(dataset, naming = "id")[[1]],
                 c("testdimension1", "testdimension2", "value"))
})

test_that("columns are correct", {
    expect_equal(fromJSONstat(dataset, naming = "label")[[1]][[1]],
                 c("Category 11", "Category 11", "Category 12", "Category 12"))
    expect_equal(fromJSONstat(dataset, naming = "id")[[1]][[1]],
                 c("testcategory11", "testcategory11", "testcategory12",
                   "testcategory12"))
    expect_equal(fromJSONstat(dataset, naming = "label")[[1]][[2]],
                 c("Category 21", "Category 22", "Category 21", "Category 22"))
    expect_equal(fromJSONstat(dataset, naming = "id")[[1]][[2]],
                 c("testcategory21", "testcategory22", "testcategory21",
                   "testcategory22"))
})

test_that("factors are factors", {
    expect_is(fromJSONstat(dataset, naming = "label",
                           use_factors = TRUE)[[1]][[1]],
              "factor")
    expect_is(fromJSONstat(dataset, naming = "id",
                           use_factors = TRUE)[[1]][[1]],
              "factor")
    expect_is(fromJSONstat(dataset, naming = "label",
                           use_factors = TRUE)[[1]][[2]],
              "factor")
    expect_is(fromJSONstat(dataset, naming = "id",
                           use_factors = TRUE)[[1]][[2]],
              "factor")
})

test_that("factor levels are correct", {
    expect_equal(nlevels(fromJSONstat(dataset, naming = "label",
                                      use_factors = TRUE)[[1]][[1]]),
                 2)
    expect_equal(levels(fromJSONstat(dataset, naming = "label",
                                     use_factors = TRUE)[[1]][[1]]),
                 c("Category 11", "Category 12"))
    expect_equal(nlevels(fromJSONstat(dataset, naming = "id",
                                      use_factors = TRUE)[[1]][[1]]),
                 2)
    expect_equal(levels(fromJSONstat(dataset, naming = "id",
                                     use_factors = TRUE)[[1]][[1]]),
                 c("testcategory11", "testcategory12"))
    expect_equal(nlevels(fromJSONstat(dataset, naming = "label",
                                      use_factors = TRUE)[[1]][[2]]),
                 2)
    expect_equal(levels(fromJSONstat(dataset, naming = "label",
                                     use_factors = TRUE)[[1]][[2]]),
                 c("Category 21", "Category 22"))
    expect_equal(nlevels(fromJSONstat(dataset, naming = "id",
                                      use_factors = TRUE)[[1]][[2]]),
                 2)
    expect_equal(levels(fromJSONstat(dataset, naming = "id",
                                     use_factors = TRUE)[[1]][[2]]),
                 c("testcategory21", "testcategory22"))
})

test_that("factor integer codes are correct", {
    expect_equivalent(unclass(fromJSONstat(dataset,
                                           use_factors = TRUE)[[1]][[1]]),
                      c(1, 1, 2, 2))
    expect_equivalent(unclass(fromJSONstat(dataset,
                                           use_factors = TRUE)[[1]][[2]]),
                      c(1, 2, 1, 2))
})

test_that("values are correct", {
    expect_equal(fromJSONstat(dataset)[[1]]$value,
                 c(1.23456789, 2.3456789, 3.456789, 4.56789))
})

test_that("attributes are correct", {
    expect_equal(attr(fromJSONstat(dataset)[[1]], "source"),
                 "Random data")
    expect_equal(attr(fromJSONstat(dataset)[[1]], "updated"),
                 "2014-09-29")
})

test_that("single-dimension input gives correct output", {
    txt <- "{\"dataset\":{\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\"]}},\"id\":[\"V1\"],\"size\":[1]},\"value\":[1]}}"
    expect_identical(toJSONstat(data.frame(V1 = "a", value = 1)),
                     structure(txt, class = "json"))
})

test_that("sparse cubes give correct output", {
    txt <- "{\"dataset\":{\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\",\"b\"]}},\"V2\":{\"category\":{\"index\":[\"A\",\"B\"]}},\"id\":[\"V1\",\"V2\"],\"size\":[2,2]},\"value\":{\"0\":1,\"3\":2}}}"
    expect_identical(toJSONstat(data.frame(V1 = c("a", "b"), V2 = c("A", "B"),
                                           value = 1:2)),
                     structure(txt, class = "json"))
    expect_identical(fromJSONstat(txt),
                     list(dataset = data.frame(V1 = c("a", "a", "b", "b"),
                                               V2 = c("A", "B", "A", "B"),
                                               value = c(1, NA, NA, 2),
                                               stringsAsFactors = FALSE)))
})

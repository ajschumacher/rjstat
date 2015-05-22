context("Output")

dataset <- readLines("dataset.json")

test_that("dataset names are correct", {
    expect_named(fromJSONstat(dataset, naming = "label"),
                 c("A dataset with array value", "A dataset with object value"))
    expect_named(fromJSONstat(dataset, naming = "id"), c("dataset", "dataset2"))
    d <- data.frame(V1 = "a", value = 1)
    expect_named(fromJSONstat(toJSONstat(d)), "dataset")
    expect_named(fromJSONstat(toJSONstat(list(d, d))), c("dataset", "dataset2"))
    expect_named(fromJSONstat(toJSONstat(list(a = d, d))), c("a", "dataset2"))
    expect_named(fromJSONstat(toJSONstat(list(d, b = d))), c("dataset", "b"))
    expect_named(fromJSONstat(toJSONstat(list(a = d, b = d))), c("a", "b"))
    expect_named(fromJSONstat(toJSONstat(list(a = d, a = d))),
                 c("a", "a (dataset2)"))
})

test_that("column names are correct", {
    expect_named(fromJSONstat(dataset, naming = "label")[[1]],
                 c("A dimension with array index",
                   "A dimension with object index",
                   "A dimension without index", "value"))
    expect_named(fromJSONstat(dataset, naming = "id")[[1]],
                 c("testdimension1", "testdimension2",
                   "testdimension3", "value"))
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

    expect_equal(fromJSONstat(dataset, naming = "label")[[1]][[3]],
                 c("Category 3", "Category 3", "Category 3", "Category 3"))
    expect_equal(fromJSONstat(dataset, naming = "id")[[1]][[3]],
                 c("testcategory3", "testcategory3", "testcategory3",
                   "testcategory3"))
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

    expect_is(fromJSONstat(dataset, naming = "label",
                           use_factors = TRUE)[[1]][[3]],
              "factor")
    expect_is(fromJSONstat(dataset, naming = "id",
                           use_factors = TRUE)[[1]][[3]],
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

    expect_equal(nlevels(fromJSONstat(dataset, naming = "label",
                                      use_factors = TRUE)[[1]][[3]]),
                 1)
    expect_equal(levels(fromJSONstat(dataset, naming = "label",
                                     use_factors = TRUE)[[1]][[3]]),
                 "Category 3")
    expect_equal(nlevels(fromJSONstat(dataset, naming = "id",
                                      use_factors = TRUE)[[1]][[3]]),
                 1)
    expect_equal(levels(fromJSONstat(dataset, naming = "id",
                                     use_factors = TRUE)[[1]][[3]]),
                 "testcategory3")
})

test_that("factor integer codes are correct", {
    expect_equivalent(unclass(fromJSONstat(dataset,
                                           use_factors = TRUE)[[1]][[1]]),
                      c(1, 1, 2, 2))
    expect_equivalent(unclass(fromJSONstat(dataset,
                                           use_factors = TRUE)[[1]][[2]]),
                      c(1, 2, 1, 2))
    expect_equivalent(unclass(fromJSONstat(dataset,
                                           use_factors = TRUE)[[1]][[3]]),
                      c(1, 1, 1, 1))
})

test_that("values are correct", {
    expect_equal(fromJSONstat(dataset)[[1]]$value,
                 c(1.23456789, 2.3456789, 3.456789, 4.56789))
    expect_equal(fromJSONstat(dataset)[[2]]$value, c(NA, 2, NA, 4))
    d <- data.frame(V1 = rev(letters), value = 1:26)
    expect_equal(fromJSONstat(toJSONstat(d))[[1]]$value, 26:1)
})

test_that("attributes are correct", {
    expect_equal(attr(fromJSONstat(dataset)[[1]], "source"),
                 "Random data")
    expect_equal(attr(fromJSONstat(dataset)[[1]], "updated"),
                 "2014-09-29")
    expect_match(toJSONstat(fromJSONstat(dataset)),
                 "\"source\":\"Random data\"")
    expect_match(toJSONstat(fromJSONstat(dataset)),
                 "\"updated\":\"2014-09-29\"")
    expect_null(attr(fromJSONstat(dataset)[[2]], "source"))
    expect_null(attr(fromJSONstat(dataset)[[2]], "updated"))
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
    expect_equal(fromJSONstat(txt),
                 list(dataset = data.frame(V1 = c("a", "a", "b", "b"),
                                           V2 = c("A", "B", "A", "B"),
                                           value = c(1, NA, NA, 2),
                                           stringsAsFactors = FALSE)))
    txt2 <- "{\"dataset\":{\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\",\"b\"]}},\"V2\":{\"category\":{\"index\":[\"A\",\"B\"]}},\"id\":[\"V1\",\"V2\"],\"size\":[2,2]},\"value\":{\"0\":true,\"3\":false}}}"
    expect_identical(toJSONstat(data.frame(V1 = c("a", "b"), V2 = c("A", "B"),
                                           value = c(TRUE, FALSE))),
                     structure(txt2, class = "json"))
    expect_equal(fromJSONstat(txt2),
                 list(dataset = data.frame(V1 = c("a", "a", "b", "b"),
                                           V2 = c("A", "B", "A", "B"),
                                           value = c(TRUE, NA, NA, FALSE),
                                           stringsAsFactors = FALSE)))
})

test_that("value objects give correct output", {
    df1 <- data.frame(V1 = factor(letters[1], letters), value = 1)
    txt1 <- toJSONstat(df1)
    expect_equal(fromJSONstat(txt1, use_factors = TRUE),
                 list(dataset = data.frame(V1 = letters,
                                           value = c(1, rep(NA, 25)))))
    df2 <- data.frame(V1 = factor(letters[1:2], letters), value = 1:2)
    txt2 <- toJSONstat(df2)
    expect_equal(fromJSONstat(txt2, use_factors = TRUE),
                 list(dataset = data.frame(V1 = letters,
                                           value = c(1:2, rep(NA, 24)))))
})

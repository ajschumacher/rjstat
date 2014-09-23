context("Output")

salmon <- readLines("1120.json")

test_that("dataset names are correct", {
    txt <- "Export of salmon, fish-farm bred, by commodity group, time and contents"
    expect_named(fromJSONstat(salmon, naming  = "label"), txt)
    expect_named(fromJSONstat(salmon, naming  = "id"), "dataset")
})

test_that("column names are correct", {
    expect_named(fromJSONstat(salmon, naming  = "label")[[1]],
                 c("commodity group", "time", "contents", "value"))
    expect_named(fromJSONstat(salmon, naming  = "id")[[1]],
                 c("VareGrupper2", "Tid", "ContentsCode", "value"))
})

test_that("columns are correct", {
    expect_equal(fromJSONstat(salmon, naming  = "label")[[1]][[1]],
                 c("Fish-farm bred salmon, fresh or chilled",
                   "Fish-farm bred salmon, fresh or chilled",
                   "Fish-farm bred salmon, fresh or chilled",
                   "Fish-farm bred salmon, fresh or chilled",
                   "Fish-farm bred salmon, frozen",
                   "Fish-farm bred salmon, frozen",
                   "Fish-farm bred salmon, frozen",
                   "Fish-farm bred salmon, frozen"))
    expect_equal(fromJSONstat(salmon, naming  = "id")[[1]][[1]],
                 c("01", "01", "01", "01", "02", "02", "02", "02"))
    expect_equal(fromJSONstat(salmon, naming  = "label")[[1]][[2]],
                 c("2014U30", "2014U30", "2014U31", "2014U31", "2014U30",
                   "2014U30", "2014U31", "2014U31"))
    expect_equal(fromJSONstat(salmon, naming  = "id")[[1]][[2]],
                 c("2014U30", "2014U30", "2014U31", "2014U31", "2014U30",
                   "2014U30", "2014U31", "2014U31"))
    expect_equal(fromJSONstat(salmon, naming  = "label")[[1]][[3]],
                 c("Weight (tonnes)", "Price per kilo (NOK)",
                   "Weight (tonnes)", "Price per kilo (NOK)",
                   "Weight (tonnes)", "Price per kilo (NOK)",
                   "Weight (tonnes)", "Price per kilo (NOK)"))
    expect_equal(fromJSONstat(salmon, naming  = "id")[[1]][[3]],
                 c("Vekt", "Kilopris", "Vekt", "Kilopris", "Vekt",
                   "Kilopris", "Vekt", "Kilopris"))
})

test_that("factors are factors", {
    expect_is(fromJSONstat(salmon, naming  = "label",
                           use_factors = TRUE)[[1]][[1]],
              "factor")
    expect_is(fromJSONstat(salmon, naming  = "id",
                           use_factors = TRUE)[[1]][[1]],
              "factor")
    expect_is(fromJSONstat(salmon, naming  = "label",
                           use_factors = TRUE)[[1]][[2]],
              "factor")
    expect_is(fromJSONstat(salmon, naming  = "id",
                           use_factors = TRUE)[[1]][[2]],
              "factor")
    expect_is(fromJSONstat(salmon, naming  = "label",
                           use_factors = TRUE)[[1]][[3]],
              "factor")
    expect_is(fromJSONstat(salmon, naming  = "id",
                           use_factors = TRUE)[[1]][[3]],
              "factor")
})

test_that("factor levels are correct", {
    expect_equal(nlevels(fromJSONstat(salmon, naming  = "label",
                                      use_factors = TRUE)[[1]][[1]]),
                 2)
    expect_equal(levels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[1]]),
                 c("Fish-farm bred salmon, fresh or chilled",
                   "Fish-farm bred salmon, frozen"))
    expect_equal(nlevels(fromJSONstat(salmon, naming  = "id",
                                      use_factors = TRUE)[[1]][[1]]),
                 2)
    expect_equal(levels(fromJSONstat(salmon, naming  = "id",
                                     use_factors = TRUE)[[1]][[1]]),
                 c("01", "02"))
    expect_equal(nlevels(fromJSONstat(salmon, naming  = "label",
                                      use_factors = TRUE)[[1]][[2]]),
                 2)
    expect_equal(levels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[2]]),
                 c("2014U30", "2014U31"))
    expect_equal(nlevels(fromJSONstat(salmon, naming  = "id",
                                      use_factors = TRUE)[[1]][[2]]),
                 2)
    expect_equal(levels(fromJSONstat(salmon, naming  = "id",
                                     use_factors = TRUE)[[1]][[2]]),
                 c("2014U30", "2014U31"))
    expect_equal(nlevels(fromJSONstat(salmon, naming  = "label",
                                      use_factors = TRUE)[[1]][[3]]),
                 2)
    expect_equal(levels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[3]]),
                 c("Weight (tonnes)", "Price per kilo (NOK)"))
    expect_equal(nlevels(fromJSONstat(salmon, naming  = "id",
                                      use_factors = TRUE)[[1]][[3]]),
                 2)
    expect_equal(levels(fromJSONstat(salmon, naming  = "id",
                                     use_factors = TRUE)[[1]][[3]]),
                 c("Vekt", "Kilopris"))
})

test_that("factor integer codes are correct", {
    expect_equivalent(unclass(fromJSONstat(salmon,
                                           use_factors = TRUE)[[1]][[1]]),
                      c(1, 1, 1, 1, 2, 2, 2, 2))
    expect_equivalent(unclass(fromJSONstat(salmon,
                                           use_factors = TRUE)[[1]][[2]]),
                      c(1, 1, 2, 2, 1, 1, 2, 2))
    expect_equivalent(unclass(fromJSONstat(salmon,
                                           use_factors = TRUE)[[1]][[3]]),
                      c(1, 2, 1, 2, 1, 2, 1, 2))
})

test_that("values are correct", {
    expect_equal(fromJSONstat(salmon)[[1]]$value,
                 c(14297, 42.23, 15439, 37.7, 342, 43.49, 447, 43.32))
})

test_that("attributes are correct", {
    expect_equal(attr(fromJSONstat(salmon)[[1]], "source"),
                 "Statistics Norway")
    expect_equal(attr(fromJSONstat(salmon)[[1]], "updated"),
                 "2014-08-09T15:21:14Z")
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

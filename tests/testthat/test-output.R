context("Output")

salmon <- readLines("1120.json")

test_that("dataset names are correct", {
    expect_that(fromJSONstat(salmon, naming  = "label"),
                has_names("Export of salmon, fish-farm bred, by commodity group, time and contents"))
    expect_that(fromJSONstat(salmon, naming  = "id"), has_names("dataset"))
})

test_that("column names are correct", {
    expect_that(fromJSONstat(salmon, naming  = "label")[[1]],
                has_names(c("commodity group", "time", "contents", "value")))
    expect_that(fromJSONstat(salmon, naming  = "id")[[1]],
                has_names(c("VareGrupper2", "Tid", "ContentsCode", "value")))
})

test_that("columns are correct", {
    expect_that(fromJSONstat(salmon, naming  = "label")[[1]][[1]],
                equals(c("Fish-farm bred salmon, fresh or chilled",
                         "Fish-farm bred salmon, fresh or chilled",
                         "Fish-farm bred salmon, fresh or chilled",
                         "Fish-farm bred salmon, fresh or chilled",
                         "Fish-farm bred salmon, frozen",
                         "Fish-farm bred salmon, frozen",
                         "Fish-farm bred salmon, frozen",
                         "Fish-farm bred salmon, frozen")))
    expect_that(fromJSONstat(salmon, naming  = "id")[[1]][[1]],
                equals(c("01", "01", "01", "01", "02", "02", "02", "02")))
    expect_that(fromJSONstat(salmon, naming  = "label")[[1]][[2]],
                equals(c("2014U30", "2014U30", "2014U31", "2014U31", "2014U30",
                         "2014U30", "2014U31", "2014U31")))
    expect_that(fromJSONstat(salmon, naming  = "id")[[1]][[2]],
                equals(c("2014U30", "2014U30", "2014U31", "2014U31", "2014U30",
                         "2014U30", "2014U31", "2014U31")))
    expect_that(fromJSONstat(salmon, naming  = "label")[[1]][[3]],
                equals(c("Weight (tonnes)", "Price per kilo (NOK)",
                         "Weight (tonnes)", "Price per kilo (NOK)",
                         "Weight (tonnes)", "Price per kilo (NOK)",
                         "Weight (tonnes)", "Price per kilo (NOK)")))
    expect_that(fromJSONstat(salmon, naming  = "id")[[1]][[3]],
                equals(c("Vekt", "Kilopris", "Vekt", "Kilopris", "Vekt",
                         "Kilopris", "Vekt", "Kilopris")))
})

test_that("factors are factors", {
    expect_that(fromJSONstat(salmon, naming  = "label",
                             use_factors = TRUE)[[1]][[1]], is_a("factor"))
    expect_that(fromJSONstat(salmon, naming  = "id",
                             use_factors = TRUE)[[1]][[1]], is_a("factor"))
    expect_that(fromJSONstat(salmon, naming  = "label",
                             use_factors = TRUE)[[1]][[2]], is_a("factor"))
    expect_that(fromJSONstat(salmon, naming  = "id",
                             use_factors = TRUE)[[1]][[2]], is_a("factor"))
    expect_that(fromJSONstat(salmon, naming  = "label",
                             use_factors = TRUE)[[1]][[3]], is_a("factor"))
    expect_that(fromJSONstat(salmon, naming  = "id",
                             use_factors = TRUE)[[1]][[3]], is_a("factor"))
})

test_that("factor levels are correct", {
    expect_that(nlevels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[1]]), equals(2))
    expect_that(levels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[1]]),
                equals(c("Fish-farm bred salmon, fresh or chilled",
                         "Fish-farm bred salmon, frozen")))
    expect_that(nlevels(fromJSONstat(salmon, naming  = "id",
                                     use_factors = TRUE)[[1]][[1]]), equals(2))
    expect_that(levels(fromJSONstat(salmon, naming  = "id",
                                    use_factors = TRUE)[[1]][[1]]),
                equals(c("01", "02")))
    expect_that(nlevels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[2]]), equals(2))
    expect_that(levels(fromJSONstat(salmon, naming  = "label",
                                    use_factors = TRUE)[[1]][[2]]),
                equals(c("2014U30", "2014U31")))
    expect_that(nlevels(fromJSONstat(salmon, naming  = "id",
                                     use_factors = TRUE)[[1]][[2]]), equals(2))
    expect_that(levels(fromJSONstat(salmon, naming  = "id",
                                    use_factors = TRUE)[[1]][[2]]),
                equals(c("2014U30", "2014U31")))
    expect_that(nlevels(fromJSONstat(salmon, naming  = "label",
                                     use_factors = TRUE)[[1]][[3]]), equals(2))
    expect_that(levels(fromJSONstat(salmon, naming  = "label",
                                    use_factors = TRUE)[[1]][[3]]),
                equals(c("Weight (tonnes)", "Price per kilo (NOK)")))
    expect_that(nlevels(fromJSONstat(salmon, naming  = "id",
                                     use_factors = TRUE)[[1]][[3]]), equals(2))
    expect_that(levels(fromJSONstat(salmon, naming  = "id",
                                    use_factors = TRUE)[[1]][[3]]),
                equals(c("Vekt", "Kilopris")))
})

test_that("factor integer codes are correct", {
    expect_that(unclass(fromJSONstat(salmon, use_factors = TRUE)[[1]][[1]]),
                is_equivalent_to(c(1, 1, 1, 1, 2, 2, 2, 2)))
    expect_that(unclass(fromJSONstat(salmon, use_factors = TRUE)[[1]][[2]]),
                is_equivalent_to(c(1, 1, 2, 2, 1, 1, 2, 2)))
    expect_that(unclass(fromJSONstat(salmon, use_factors = TRUE)[[1]][[3]]),
                is_equivalent_to(c(1, 2, 1, 2, 1, 2, 1, 2)))
})

test_that("values are correct", {
    expect_that(fromJSONstat(salmon)[[1]]$value,
                equals(c(14297, 42.23, 15439, 37.7, 342, 43.49, 447, 43.32)))
})

test_that("attributes are correct", {
    expect_that(attr(fromJSONstat(salmon)[[1]], "source"),
                equals("Statistics Norway"))
    expect_that(attr(fromJSONstat(salmon)[[1]], "updated"),
                equals("2014-08-09T15:21:14Z"))
})

test_that("single-dimension input gives correct output", {
    expect_that(toJSONstat(data.frame(V1 = "a", value = 1)),
                is_identical_to(structure("{\"dataset\":{\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\"]}},\"id\":[\"V1\"],\"size\":[1]},\"value\":[1]}}",
                                          class = "json")))
})

test_that("sparse cubes give correct output", {
    expect_that(toJSONstat(data.frame(V1 = c("a", "b"), V2 = c("A", "B"),
                                      value = 1:2)),
                is_identical_to(structure("{\"dataset\":{\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\",\"b\"]}},\"V2\":{\"category\":{\"index\":[\"A\",\"B\"]}},\"id\":[\"V1\",\"V2\"],\"size\":[2,2]},\"value\":{\"0\":1,\"3\":2}}}",
                                          class = "json")))
    expect_that(fromJSONstat("{\"dataset\":{\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\",\"b\"]}},\"V2\":{\"category\":{\"index\":[\"A\",\"B\"]}},\"id\":[\"V1\",\"V2\"],\"size\":[2,2]},\"value\":{\"0\":1,\"3\":2}}}"),
                is_identical_to(structure(list(dataset = structure(list(V1 = c("a", "a", "b", "b"), V2 = c("A", "B", "A", "B"), value = c(1, NA, NA, 2)), .Names = c("V1", "V2", "value"), class = "data.frame", row.names = c(NA, -4L))), .Names = "dataset")))
})

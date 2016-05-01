context("Input")

non_unique <- data.frame(V1 = c("a", "a"), V2 = c("b", "b"), value = 1:2)
txt <- "{\"version\":\"2.0\",\"class\":\"dataset\",\"id\":[\"V1\"],\"size\":[1],\"value\":[1],\"dimension\":{\"V1\":{\"category\":{\"index\":[\"a\"]}}}}"

test_that("wrong input fails", {
    expect_error(toJSONstat(data.frame(value = 1, V1 = NA)), "missing values")
    expect_error(toJSONstat(non_unique),
                 "non-value columns must constitute a unique ID")
})

test_that("name of value column works", {
    df1 <- data.frame(V1 = "a", value = 1)
    expect_match(toJSONstat(df1), "\"value\":\\[1\\]")
    df2 <- data.frame(V1 = "a", v = 1)
    expect_match(toJSONstat(df2, value = "v"), "\"value\":\\[1\\]")
})

test_that("round-trip works", {
    df1 <- fromJSONstat(txt)
    df2 <- fromJSONstat(toJSONstat(df1))
    expect_equal(df1, df2)
})

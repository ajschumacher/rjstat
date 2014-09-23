context("Input")

oecd <- readLines("oecd-canada.json")
non_unique <- data.frame(V1 = c("a", "a"), V2 = c("b", "b"), value = 1:2)

test_that("wrong input fails", {
    expect_error(fromJSONstat(1), "is not a character vector")
    expect_error(fromJSONstat(character(0)), "not greater than 0")
    expect_error(fromJSONstat(oecd, 1), "is not a string")
    expect_error(fromJSONstat(oecd, letters), "is not a string")
    expect_error(fromJSONstat(oecd, "a"), "naming must be \"label\" or \"id\"")
    expect_error(toJSONstat(1), "(?:.*is not a data frame)(?:.* is not a list)")
    expect_error(toJSONstat(fromJSONstat(oecd), letters), "is not a string")
    expect_error(toJSONstat(list(1)), "is not a data frame")
    expect_error(toJSONstat(non_unique),
                 "non-value columns must constitute a unique ID")
    expect_error(toJSONstat(data.frame(value = 1, id = 1)),
                 "not allowed column names")
    expect_error(toJSONstat(data.frame(value = 1, size = 1)),
                 "not allowed column names")
    expect_error(toJSONstat(data.frame(value = 1, role = 1)),
                 "not allowed column names")
})

test_that("round-trip works", {
    df1 <- fromJSONstat(oecd, use_factors = TRUE)
    df2 <- fromJSONstat(toJSONstat(df1, digits = 8), use_factors = TRUE)
    expect_equal(df1, df2)
})

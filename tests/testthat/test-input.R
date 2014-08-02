context("Input")

oecd <- readLines("oecd-canada.json")

test_that("wrong input fails", {
    expect_that(fromJSONstat(1), throws_error("is not a character vector"))
    expect_that(fromJSONstat(character(0)), throws_error("not greater than 0"))
    expect_that(fromJSONstat(oecd, 1), throws_error("is not a string"))
    expect_that(fromJSONstat(oecd, letters), throws_error("is not a string"))
    expect_that(fromJSONstat(oecd, "a"),
                throws_error("naming must be \"label\" or \"id\""))
})

test_that("correct input doesn't fail", {
    expect_that(fromJSONstat(oecd, naming = "label", use_factors = F),
                not(throws_error()))
    expect_that(fromJSONstat(oecd, naming = "label", use_factors = T),
                not(throws_error()))
    expect_that(fromJSONstat(oecd, naming = "id", use_factors = F),
                not(throws_error()))
    expect_that(fromJSONstat(oecd, naming = "id", use_factors = T),
                not(throws_error()))
})

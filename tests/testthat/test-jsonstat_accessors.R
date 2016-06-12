context("jsonstat accessors")

test_that("id", {
    x <- as.jsonstat("oecd.json")
    expect_equal(id(x), c("concept", "area", "year"))
    expect_error(id(x) <- "Hej")
    id(x) <- c("1", "2", "3")
    expect_equal(id(x),  c("1", "2", "3"))
})


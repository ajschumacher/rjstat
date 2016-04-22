context("Warnings")

test_that("warnings are suppressed", {
    expect_silent(fromJSONstat("nonexistent.json", silent = TRUE))
})

test_that("warnings work", {
    expect_warning(fromJSONstat("nonexistent.json"), "returning unparsed list",
                   all = TRUE)
})

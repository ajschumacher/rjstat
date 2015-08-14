context("Classes")

test_that("dataset responses work", {
    expect_equal(names(fromJSONstat("dataset.json")), "A dataset")
})

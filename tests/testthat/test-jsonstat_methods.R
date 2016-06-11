context("jsonstat methods")

test_that("print works", {
    expect_output(print(as.jsonstat("dataset.json")), regexp = "JSON-stat dataset")
    expect_output(print(as.jsonstat("collection.json")), regexp = "JSON-stat collection")
})

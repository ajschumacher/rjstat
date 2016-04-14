context("Classes")

test_that("dataset responses work", {
    fromJSONstat("dataset.json") %>%
        expect_named("A dataset")
})

context("Datasets")

test_that("dataset names are correct", {
    fromJSONstat("bundle.json", naming = "label") %>%
        expect_named(c("A dataset with array value",
                       "A dataset with object value"))
    fromJSONstat("bundle.json", naming = "id") %>%
        expect_named(c("dataset", "dataset2"))
})

test_that("dataset names are correct for missing labels", {
    readLines("bundle.json")[-3] %>%
        fromJSONstat(naming = "label") %>%
        expect_named(c("dataset", "A dataset with object value"))
})

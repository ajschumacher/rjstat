context("Datasets")

test_that("dataset names are correct", {
    fromJSONstat("bundle.json", naming = "label") %>%
        expect_named(c("A dataset with array value",
                       "A dataset with object value"))
    fromJSONstat("bundle.json", naming = "id") %>%
        expect_named(c("dataset", "dataset2"))
    d <- data.frame(V1 = "a", value = 1)
    toJSONstat(d) %>%
        fromJSONstat() %>%
        expect_named("dataset")
    toJSONstat(list(d, d)) %>%
        fromJSONstat() %>%
        expect_named(c("dataset", "dataset2"))
    toJSONstat(list(a = d, d)) %>%
        fromJSONstat() %>%
        expect_named(c("a", "dataset2"))
    toJSONstat(list(d, b = d)) %>%
        fromJSONstat() %>%
        expect_named(c("dataset", "b"))
    toJSONstat(list(a = d, b = d)) %>%
        fromJSONstat() %>%
        expect_named(c("a", "b"))
    toJSONstat(list(a = d, a = d)) %>%
        fromJSONstat() %>%
        expect_named(c("a", "a (dataset2)"))
})

test_that("dataset names are correct for missing labels", {
    readLines("bundle.json")[-3] %>%
        fromJSONstat(naming = "label") %>%
        expect_named(c("dataset", "A dataset with object value"))
})

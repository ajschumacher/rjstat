context("Classes")

test_that("dataset responses work", {
    fromJSONstat("dataset.json") %>%
        expect_type("list") %>%
        expect_named("A dataset") %>%
        getElement(1) %>%
        expect_s3_class("data.frame") %>%
        expect_named(c("A dimension", "value")) %>%
        getElement("value") %>%
        expect_equal(1)
})

test_that("collection responses work", {
    fromJSONstat("collection.json") %>%
        expect_type("list") %>%
        expect_named("A collection") %>%
        getElement(1) %>%
        expect_type("list") %>%
        expect_named("A dataset") %>%
        getElement(1) %>%
        expect_s3_class("data.frame") %>%
        expect_named(c("A dimension", "value")) %>%
        getElement("value") %>%
        expect_equal(1)
})

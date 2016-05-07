context("Factors")

test_that("factors are factors", {
    fromJSONstat("bundle.json", naming = "label", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(1) %>%
        expect_s3_class("factor")
    fromJSONstat("bundle.json", naming = "id", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(1) %>%
        expect_s3_class("factor")

    fromJSONstat("bundle.json", naming = "label", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(2) %>%
        expect_s3_class("factor")
    fromJSONstat("bundle.json", naming = "id", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(2) %>%
        expect_s3_class("factor")

    fromJSONstat("bundle.json", naming = "label", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(3) %>%
        expect_s3_class("factor")
    fromJSONstat("bundle.json", naming = "id", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(3) %>%
        expect_s3_class("factor")
})

test_that("factor levels are correct", {
    fromJSONstat("bundle.json", naming = "label", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(1) %>%
        levels() %>%
        expect_equal(c("Category 11", "Category 12"))
    fromJSONstat("bundle.json", naming = "id", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(1) %>%
        levels() %>%
        expect_equal(c("testcategory11", "testcategory12"))

    fromJSONstat("bundle.json", naming = "label", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(2) %>%
        levels() %>%
        expect_equal(c("Category 21", "Category 22", "Category 23"))
    fromJSONstat("bundle.json", naming = "id", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(2) %>%
        levels() %>%
        expect_equal(c("testcategory21", "testcategory22", "testcategory23"))

    fromJSONstat("bundle.json", naming = "label", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(3) %>%
        levels() %>%
        expect_equal("Category 3")
    fromJSONstat("bundle.json", naming = "id", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(3) %>%
        levels() %>%
        expect_equal("testcategory3")
})

test_that("factor integer codes are correct", {
    fromJSONstat("bundle.json", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(1) %>%
        unclass() %>%
        expect_equivalent(c(1, 1, 1, 2, 2, 2))
    fromJSONstat("bundle.json", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(2) %>%
        unclass() %>%
        expect_equivalent(c(1, 2, 3, 1, 2, 3))
    fromJSONstat("bundle.json", use_factors = TRUE) %>%
        getElement(1) %>%
        getElement(3) %>%
        unclass() %>%
        expect_equivalent(c(1, 1, 1, 1, 1, 1))
})

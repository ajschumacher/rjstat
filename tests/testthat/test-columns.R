context("Columns")

test_that("column names are correct", {
    fromJSONstat("bundle.json", naming = "label") %>%
        getElement(1) %>%
        expect_named(c("A dimension with array index",
                       "A dimension with object index",
                       "A dimension without index", "value"))
    fromJSONstat("bundle.json", naming = "id") %>%
        getElement(1) %>%
        expect_named(c("testdimension1", "testdimension2",
                       "testdimension3", "value"))
    data.frame(V1 = "a", value = 1) %>%
        toJSONstat(value = "V1") %>%
        fromJSONstat() %>%
        expect_named(c("value", "value"))
})

test_that("columns are correct", {
    fromJSONstat("bundle.json", naming = "label") %>%
        getElement(1) %>%
        getElement(1) %>%
        expect_equal(c("Category 11", "Category 11",
                       "Category 12", "Category 12"))
    fromJSONstat("bundle.json", naming = "id") %>%
        getElement(1) %>%
        getElement(1) %>%
        expect_equal(c("testcategory11", "testcategory11",
                       "testcategory12", "testcategory12"))

    fromJSONstat("bundle.json", naming = "label") %>%
        getElement(1) %>%
        getElement(2) %>%
        expect_equal(c("Category 21", "Category 22",
                       "Category 21", "Category 22"))
    fromJSONstat("bundle.json", naming = "id") %>%
        getElement(1) %>%
        getElement(2) %>%
        expect_equal(c("testcategory21", "testcategory22",
                       "testcategory21", "testcategory22"))

    fromJSONstat("bundle.json", naming = "label") %>%
        getElement(1) %>%
        getElement(3) %>%
        expect_equal(c("Category 3", "Category 3",
                       "Category 3", "Category 3"))
    fromJSONstat("bundle.json", naming = "id") %>%
        getElement(1) %>%
        getElement(3) %>%
        expect_equal(c("testcategory3", "testcategory3",
                       "testcategory3", "testcategory3"))

    data.frame(V1 = as.raw(0:255), value = 0:255) %>%
        toJSONstat() %>%
        fromJSONstat() %>%
        getElement("V1") %>%
        expect_equal(as.character(as.raw(0:255)))
})

test_that("column names are correct for missing labels", {
    readLines("bundle.json")[-24] %>%
        fromJSONstat(naming = "label") %>%
        getElement(1) %>%
        expect_named(c("testdimension1", "A dimension with object index",
                       "A dimension without index", "value"))
})

test_that("columns are correct for missing labels", {
    readLines("bundle.json")[-31] %>%
        fromJSONstat(naming = "label") %>%
        getElement(1) %>%
        getElement(1) %>%
        expect_equal(c("testcategory11", "testcategory11",
                       "testcategory12", "testcategory12"))
    readLines("bundle.json")[-44] %>%
        fromJSONstat(naming = "label") %>%
        getElement(1) %>%
        getElement(2) %>%
        expect_equal(c("testcategory21", "testcategory22",
                       "testcategory21", "testcategory22"))
})

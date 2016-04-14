context("Attributes")

test_that("attributes are correct", {
    fromJSONstat("bundle.json") %>%
        getElement(1) %>%
        attr("source") %>%
        expect_equal("Random data")
    fromJSONstat("bundle.json") %>%
        getElement(1) %>%
        attr("updated") %>%
        expect_equal("2014-09-29")
    fromJSONstat("bundle.json") %>%
        toJSONstat() %>%
        expect_match("\"source\":\"Random data\"")
    fromJSONstat("bundle.json") %>%
        toJSONstat() %>%
        expect_match("\"updated\":\"2014-09-29\"")
    fromJSONstat("bundle.json") %>%
        getElement(2) %>%
        attr("source") %>%
        expect_null()
    fromJSONstat("bundle.json") %>%
        getElement(2) %>%
        attr("updated") %>%
        expect_null()
})

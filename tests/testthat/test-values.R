context("Values")

test_that("values are correct", {
    fromJSONstat("bundle.json") %>%
        getElement(1) %>%
        getElement("value") %>%
        expect_equal(c(1.23456789, 2.3456789, 3.456789, 4.56789, 5.6789, 6.789))
    fromJSONstat("bundle.json") %>%
        getElement(2) %>%
        getElement("value") %>%
        expect_equal(c(NA, 2, NA, 4))
    data.frame(V1 = rev(letters), value = 1:26) %>%
        toJSONstat() %>%
        fromJSONstat() %>%
        getElement("value") %>%
        expect_equal(26:1)
    data.frame(V1 = 1:26, value = letters) %>%
        toJSONstat() %>%
        fromJSONstat() %>%
        getElement("value") %>%
        expect_equal(letters)
    data.frame(V1 = 0:255, value = as.raw(0:255)) %>%
        toJSONstat() %>%
        fromJSONstat() %>%
        getElement("value") %>%
        expect_equal(as.character(as.raw(0:255)))
})

context("Constructor")

test_that("constructor works", {
    as.jsonstat("collection.json") %>%
        expect_class("json-stat") %>%
        expect_class("collection")
    as.jsonstat("dataset.json") %>%
        expect_class("json-stat") %>%
        expect_class("dataset")
    expect_error(as.jsonstat("notjsonstat.json"))

    expect_silent(as.jsonstat("oecd.json"))
    expect_silent(as.jsonstat("canada.json"))
    expect_silent(as.jsonstat("oecd-canada-col.json"))
    expect_silent(as.jsonstat("galicia.json"))
    expect_silent(as.jsonstat("order.json"))
    expect_silent(as.jsonstat("hierarchy.json"))
    expect_silent(as.jsonstat("us-gsp.json"))
    expect_silent(as.jsonstat("us-unr.json"))
    expect_silent(as.jsonstat("us-labor.json"))
    expect_silent(as.jsonstat("collection_sample.json"))
})

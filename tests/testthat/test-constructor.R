context("Constructor")

test_that("constructor works", {
    jsonstat("collection.json") %>%
        expect_class("json-stat") %>%
        expect_class("collection")
    jsonstat("dataset.json") %>%
        expect_class("json-stat") %>%
        expect_class("dataset")
    expect_error(jsonstat("notjsonstat.json"))

    expect_silent(jsonstat("oecd.json"))
    expect_silent(jsonstat("canada.json"))
    expect_silent(jsonstat("oecd-canada-col.json"))
    expect_silent(jsonstat("galicia.json"))
    expect_silent(jsonstat("order.json"))
    expect_silent(jsonstat("hierarchy.json"))
    expect_silent(jsonstat("us-gsp.json"))
    expect_silent(jsonstat("us-unr.json"))
    expect_silent(jsonstat("us-labor.json"))
    expect_silent(jsonstat("collection_sample.json"))
})

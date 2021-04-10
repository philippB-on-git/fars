context("main functions")

test_that("fars years are summarized correctly", {
    load("test_fys.RData")

    tmp.dir <- getwd()
    setwd(system.file("extdata", package = "fars"))
    fys <- fars_summarize_years(c(2014, 2013))
    setwd(tmp.dir)

    expect_identical(fys, test.fys)
})

test_that("fars map has no issues", {
    tmp.dir <- getwd()
    setwd(system.file("extdata", package = "fars"))
    expect_warning(fars_map_state(9, 2014), NA)
    expect_error(fars_map_state(9, 2014), NA)
    setwd(tmp.dir)
})

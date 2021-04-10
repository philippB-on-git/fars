context("helper functions")

test_that("filename is created correctly", {
    expect_equal(make_filename(2014), "accident_2014.csv.bz2")
    expect_equal(make_filename("2014"), "accident_2014.csv.bz2")
    expect_warning(make_filename("abc"), NULL)
})

test_that("fars_read works correctly", {
    expect_error(fars_read("abc"))

    rd <- fars_read(system.file("extdata",
                                "accident_2014.csv.bz2",
                                package = "fars"))

    expect_equal(nrow(rd), 30056)
    expect_equal(ncol(rd), 50)
    expect_is(rd, "tbl_df")
})

test_that("fars_read_years", {
    load("test_fy.RData")

    tmp.dir <- getwd()
    setwd(system.file("extdata", package = "fars"))
    fy <- fars_read_years(c(2014, 2013))
    setwd(tmp.dir)

    expect_is(fy, "list")
    expect_equal(length(fy), 2)
    expect_is(fy[[1]], "tbl_df")
    expect_is(fy[[2]], "tbl_df")
    expect_identical(fy, test.fy)
})

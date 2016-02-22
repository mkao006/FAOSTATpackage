context("Testing function geogr()")


test_that("Function geogr() generate correct value", {
    expect_that(geogr(1), gives_warning())
    expect_that(geogr(c(1, 1)), gives_warning())
    expect_equal(geogr(rep(1, 5)), c(NA, rep(0, 4)))
    expect_equal(geogr(c(1, 2, 4, 8, 16)), c(NA, rep(100, 4)))
    expect_equal(geogr(1:5), c(NA, 1/1:4) * 100)
})


test_that("Function geogr() gives correct output", {
    ## Testing class
    expect_that(geogr(1:5), is_a("numeric"))

    ## Testing whether the right number of NA is filled
    expect_equal(length(na.omit(geogr(1:5))), 4)
    expect_equal(length(na.omit(geogr(1:5, 2))), 3)
    expect_equal(length(na.omit(geogr(1:10, 5))), 5)
})

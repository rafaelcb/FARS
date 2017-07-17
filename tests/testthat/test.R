testthat::expect_that(FARS::fars_read_years(2013), testthat::is_a("list"))
testthat::expect_that(FARS::fars_read(make_filename(2013)), testthat::is_a("tbl_df"))

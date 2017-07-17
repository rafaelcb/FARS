# Test to check that a tibble is generated using the fars_read_years function

expect_that(fars_read_years(2010), is_a("tbl_df"))

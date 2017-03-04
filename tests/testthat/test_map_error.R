library(wk4project)
context("testing mapper error throws")

expect_that(fars_map_state(1000,2015), throws_error())
expect_that(fars_map_state(6,1823), throws_error())

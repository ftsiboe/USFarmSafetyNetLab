# # Unit tests for analyze_and_summarize_time_series using testthat
# if (requireNamespace("testthat", quietly = TRUE)) {
#   library(testthat)
#   library(data.table)
#   
#   dt <- data.table(
#     commodity_year = rep(2000:2002, 2),
#     disaggregate   = rep(c("A", "B"), each = 3),
#     outcome        = rep("X", 6),
#     value          = c(1, 2, 3, 2, 4, 6)
#   )
#   res <- analyze_and_summarize_time_series(dt, threshold = 50)
#   
#   test_that("returns exactly all expected columns", {
#     cols <- names(res[["X"]][[1]])
#     expected <- c(
#       "mean", "sd", "median", "iqr", "cv", "n_obs", "pct_missing",
#       "initial_year_value", "final_year_value", "year_with_max_value", "max_value",
#       "year_with_min_value", "min_value", "cumulative_change", "mean_annual_growth_rate",
#       "growth_initial_to_final", "first_diff_sd", "max_drawdown_pct", "trend_slope",
#       "trend_p_value", "trend_r_squared", "time_to_peak", "time_to_trough",
#       "notable_growth", "notable_growth_chron", "notable_decline", "notable_decline_chron",
#       "any_missing_years", "monotonic", "rank", "rank_among_outcomes"
#     )
#     expect_equal(sort(cols), sort(expected))
#   })
#   
#   test_that("computes correct statistics on simple data for level_ranked_as_1", {
#     a <- res[["X"]][[1]]
#     expect_equal(a$mean, mean(c(2,4,6)))
#     expect_equal(a$sd, sd(c(2,4,6)))
#     expect_equal(a$median, median(c(2,4,6)))
#     expect_equal(a$iqr, IQR(c(2,4,6)))
#     expect_equal(a$cv, sd(c(2,4,6)) / mean(c(2,4,6)))
#     expect_equal(a$n_obs, 3)
#     expect_equal(a$pct_missing, 0)
#     expect_equal(a$initial_year_value, 2)
#     expect_equal(a$final_year_value, 6)
#     expect_equal(a$year_with_max_value, 2002)
#     expect_equal(a$max_value, 6)
#     expect_equal(a$year_with_min_value, 2000)
#     expect_equal(a$min_value, 2)
#     expect_equal(a$cumulative_change, 4)
#     expect_equal(a$growth_initial_to_final, (6-2)/2*100)
#     expect_equal(a$mean_annual_growth_rate, (6/2)^(1/2)*100 - 100)
#     expect_equal(a$first_diff_sd, sd(c(100,50)))
#     expect_equal(a$max_drawdown_pct, 0)
#     expect_true(a$monotonic)
#     expect_equal(a$time_to_peak, 2)
#     expect_equal(a$time_to_trough, 0)
#     expect_match(a$notable_growth_chron, "2001: \\+100.0%; 2002: \\+50.0%")
#     expect_true(is.na(a$notable_decline_chron))
#     expect_equal(a$rank, 1)
#     expect_equal(a$rank_among_outcomes, 2)
#   })
#   
#   test_that("computes correct statistics on simple data for level_ranked_as_2", {
#     a <- res[["X"]][[2]]
#     expect_equal(a$mean, mean(c(1,2,3)))
#     expect_equal(a$sd, sd(c(1,2,3)))
#     expect_equal(a$median, median(c(1,2,3)))
#     expect_equal(a$iqr, IQR(c(1,2,3)))
#     expect_equal(a$cv, sd(c(1,2,3)) / mean(c(1,2,3)))
#     expect_equal(a$n_obs, 3)
#     expect_equal(a$pct_missing, 0)
#     expect_equal(a$initial_year_value, 1)
#     expect_equal(a$final_year_value, 3)
#     expect_equal(a$year_with_max_value, 2002)
#     expect_equal(a$max_value, 3)
#     expect_equal(a$year_with_min_value, 2000)
#     expect_equal(a$min_value, 1)
#     expect_equal(a$cumulative_change, 2)
#     expect_equal(a$growth_initial_to_final, 200)
#     expect_equal(a$mean_annual_growth_rate, (3/1)^(1/2)*100 - 100)
#     expect_equal(a$first_diff_sd, sd(c(100,50)))
#     expect_equal(a$max_drawdown_pct, 0)
#     expect_true(a$monotonic)
#     expect_equal(a$time_to_peak, 2)
#     expect_equal(a$time_to_trough, 0)
#     expect_match(a$notable_growth_chron, "2001: \\+100.0%; 2002: \\+50.0%")
#     expect_true(is.na(a$notable_decline_chron))
#     expect_equal(a$rank, 2)
#     expect_equal(a$rank_among_outcomes, 1)
#   })
# }
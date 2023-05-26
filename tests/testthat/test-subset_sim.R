test_that("subset works", {
  # test data
  test_sim_res <- readRDS(test_path("fixtures", "test_sim_res.rds"))
  test_from <- round(0.8 * test_sim_res$simulated_time)
  test_time_points <- 1:test_from

  # get results
  results_from <- subset(test_sim_res, from = test_from)
  results_time_points <- subset(test_sim_res, time_points = test_time_points)

  # tests
  expect_s3_class(results_from, "sim_results")
  expect_equal(dim(results_from$N_map)[3],
               test_sim_res$simulated_time - test_from + 1)
  expect_equal(results_from$simulated_time,
               test_sim_res$simulated_time - test_from + 1)

  expect_s3_class(results_time_points, "sim_results")
  expect_equal(dim(results_time_points$N_map)[3], test_from)
  expect_equal(results_time_points$simulated_time, test_from)

  expect_error(subset(test_sim_res))
  expect_error(subset(test_sim_res, from = 0))
  expect_error(subset(test_sim_res, from = -5))
  expect_error(subset(test_sim_res, from = test_sim_res$simulated_time + 5))
  expect_error(subset(test_sim_res, time_points = c(-5, 1:5)))
  expect_error(subset(test_sim_res, time_points = 0:5))
  expect_error(subset(test_sim_res,
                      time_points = c(1:5, test_sim_res$simulated_time + 5)))
  expect_error(subset(test_sim_res,
                      time_points = 1:test_sim_res$simulated_time))
})

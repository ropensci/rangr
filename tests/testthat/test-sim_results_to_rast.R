test_that("to_rast works", {
  # test data
  test_sim_data <- readRDS(test_path("fixtures", "test_sim_data.rds"))
  test_sim_res <- readRDS(test_path("fixtures", "test_sim_res.rds"))
  test_id_rast <- rast(test_path("fixtures", "test_id_rast.tif"))
  test_rast_no_template <- rast(test_sim_res$N_map[, , test_sim_res$simulated_time])

  # tests
  expect_s4_class(to_rast(
    test_sim_res, template = test_id_rast), "SpatRaster")
  expect_s4_class(to_rast(
    test_sim_res, 1:test_sim_res$simulated_time, test_id_rast),
    "SpatRaster")
  expect_equal(nlyr(
    to_rast(test_sim_res, 1:test_sim_res$simulated_time,
                          test_id_rast)), test_sim_res$simulated_time)
  expect_true(ext(to_rast(
    test_sim_res, 1:test_sim_res$simulated_time, test_id_rast)) ==
      ext(test_id_rast))

  op <- options()
  options(warn = -1)
  expect_equal(crs(to_rast(test_sim_res)),  crs(test_rast_no_template))
  expect_equal(dim(to_rast(test_sim_res)),  dim(test_rast_no_template))
  options(op)

  expect_warning(to_rast(test_sim_res),  regexp = "No template provided")
  expect_error(to_rast(test_sim_res, template = rast(ncol = 5, nrow = 5)))
})

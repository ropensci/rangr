test_that("update works", {
  # test data
  test_sim_data <- readRDS(test_path("fixtures", "test_sim_data.rds"))
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_id_rast <- rast(test_path("fixtures", "test_id_rast.tif"))
  test_sim_data$id <- test_id_rast
  test_sim_data$K_map <- test_rast
  test_sim_data$n1_map <- test_rast

  # get results
  test_sim_data_updated_01 <- update(test_sim_data, border = "reprising")
  test_sim_data_updated_02 <- update(test_sim_data, max_dist = 3000)
  test_sim_data_updated_03 <- update(test_sim_data, kernel_fun = "dcauchy")
  test_sim_data_updated_04 <- update(test_sim_data, rate = 1 / 200)

  test_sim_data_no_call <- test_sim_data
  test_sim_data_no_call$call <- NULL

  expect_equal(test_sim_data_updated_01$border, "reprising")
  expect_identical(test_sim_data_updated_01$dlist, test_sim_data$dlist)

  expect_equal(test_sim_data_updated_02$max_dist, 3000)
  expect_false(identical(test_sim_data_updated_02$dlist, test_sim_data$dlist))

  expect_equal(test_sim_data_updated_03$kernel_fun, "dcauchy")
  expect_null(test_sim_data_updated_03$call$rate)

  expect_false(identical(test_sim_data_updated_04$dlist, test_sim_data$dlist))
  expect_equal(test_sim_data_updated_04$call$rate, quote(1 / 200))

  expect_warning(update(test_sim_data))
  expect_true(is.call(
    update(test_sim_data, border = "reprising", evaluate = FALSE)), TRUE)

  expect_error(update(test_sim_data_no_call, border = "reprising"))

})

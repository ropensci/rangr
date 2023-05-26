test_that("get_observations works", {
  # test data
  test_sim_data <- readRDS(test_path("fixtures", "test_sim_data.rds"))
  test_sim_res <- readRDS(test_path("fixtures", "test_sim_res.rds"))
  test_id_rast <- rast(test_path("fixtures", "test_id_rast.tif"))
  test_points <- readRDS(test_path("fixtures", "test_points.rds"))
  test_sim_data$id <- test_id_rast
  test_random_time_steps <- sample(1:test_sim_res$simulated_time, 2)


  test_points["x.y"] <-
    paste(test_points$x, test_points$y, sep = ".")

  # get results
  sample_type_1 <- get_observations(test_sim_data, test_sim_res,
    type = "random_one_layer",
    prop = 0.1
  )
  sample_type_2 <- get_observations(test_sim_data, test_sim_res,
    type = "random_all_layers",
    prop = 0.15
  )
  sample_type_3 <- get_observations(test_sim_data, test_sim_res,
    type = "from_data",
    points = test_points
  )
  sample_type_4 <- get_observations(test_sim_data, test_sim_res,
    type = "monitoring_based",
    cells_coords = test_points[c("x", "y")],
    prob = 0.3
  )
  sample_type_5 <- get_observations(test_sim_data, test_sim_res,
    type = "from_data",
    points = test_points,
    sdlog = log(1.2)
  )

  # process results
  sample_type_4_points <- unique(sample_type_4[, c("x", "y")])
  sample_type_4_points <-
    paste(sample_type_4_points$x, sample_type_4_points$y, sep = ".")

  # tests
  # 1. random_one_layer
  expect_s3_class(sample_type_1, "data.frame")
  expect_true(all(sample_type_1$value >= 0, na.rm = TRUE))
  expect_equal(unique(sample_type_1$time_step), 1:test_sim_res$simulated_time)
  expect_true(all(sample_type_1[sample_type_1$time_step ==
                                  test_random_time_steps[1], c("x", "y")] ==
                  sample_type_1[sample_type_1$time_step ==
                                  test_random_time_steps[2], c("x", "y")]))
  expect_true(all(sample_type_1$x <= xmax(test_id_rast)) &&
    all(sample_type_1$x >= xmin(test_id_rast)) &&
    all(sample_type_1$y <= ymax(test_id_rast)) &&
    all(sample_type_1$y >= ymin(test_id_rast)))

  # 2. random_all_layers
  expect_s3_class(sample_type_2, "data.frame")
  expect_true(all(sample_type_2$value >= 0, na.rm = TRUE))
  expect_equal(unique(sample_type_2$time_step), 1:test_sim_res$simulated_time)
  expect_false(all(sample_type_2[sample_type_2$time_step ==
                                   test_random_time_steps[1], c("x", "y")] ==
                   sample_type_2[sample_type_2$time_step ==
                                   test_random_time_steps[2], c("x", "y")]))
  expect_true(all(sample_type_2$x <= xmax(test_id_rast)) &&
    all(sample_type_2$x >= xmin(test_id_rast)) &&
    all(sample_type_2$y <= ymax(test_id_rast)) &&
    all(sample_type_2$y >= ymin(test_id_rast)))


  # 3. from_data
  expect_s3_class(sample_type_3, "data.frame")
  expect_true(all(sample_type_3$value >= 0, na.rm = TRUE))
  expect_true(
    all(sample_type_3[, c("x", "y", "time_step")] ==
          test_points[, c("x", "y", "time_step")]))

  # 4. monitoring_based
  expect_s3_class(sample_type_4, "data.frame")
  expect_true(all(sample_type_4$value >= 0, na.rm = TRUE))
  expect_equal(ncol(sample_type_4), 5)
  expect_true(all(sample_type_4_points %in% test_points[["x.y"]]))

  # 5. noise
  expect_false(all(sample_type_5$value == sample_type_3$value))
})


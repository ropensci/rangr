test_that("get_observations works", {
  # test data
  test_sim_data <- readRDS(test_path("fixtures", "test_sim_data.rds"))
  test_sim_res <- readRDS(test_path("fixtures", "test_sim_res.rds"))
  test_id_rast <- rast(test_path("fixtures", "test_id_rast.tif"))
  test_points <- readRDS(test_path("fixtures", "test_points.rds"))
  test_sim_data$id <- test_id_rast
  test_random_time_steps <- sample(1:test_sim_res$simulated_time, 2)



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
    cells_coords = test_points[test_points$time_step == 1, c("x", "y")],
    prob = 0.3
  )
  sample_type_5 <- get_observations(test_sim_data, test_sim_res,
    type = "from_data",
    points = test_points,
    obs_error = "rlnorm",
    obs_error_param = log(1.2)
  )

  # process results
  sample_type_4_points <- unique(sample_type_4[, c("x", "y")])
  sample_type_4_points <-
    paste(sample_type_4_points$x, sample_type_4_points$y, sep = ".")

  test_points["x.y"] <-
    paste(test_points$x, test_points$y, sep = ".")

  #' @srrstats {G5.2, G5.2a, G5.2b} tests of errors and warnings (with messages)
  #' @srrstats {G5.8, G5.8a, G5.8b, G5.8c} edge condition tests:
  #' zero-length data, unsupported data types, NA fields

  # tests
  # input parameters
  expect_error(
    get_observations(test_sim_data, test_sim_res, type = "veewv"))
  expect_error(
    get_observations(test_sim_data, test_sim_res, type = c("veewv", "ala")))

  expect_error(
    get_observations(1, test_sim_res, type = "random_one_layer"),
    "sim_data does not inherit from class sim_data")

  expect_error(
    get_observations(test_sim_data, 1, type = "random_one_layer"),
    "sim_results does not inherit from class sim_results")

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "random_one_layer", obs_error_param = c(2, 4)),
    "parameter obs_error_param can be set either as NULL or as a single number",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "random_one_layer", obs_error_param = "1"),
    "parameter obs_error_param can be set either as NULL or as a single number",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "random_one_layer", prop = c(2, 4)),
    "length(prop) not equal to 1",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "random_one_layer", prop = "1"),
    "prop is not a numeric or integer vector",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "random_one_layer", prop = 0),
    "prop parameter must be greater than 0 but less than or equal to 1",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "random_one_layer", prop = 1.5),
    "prop parameter must be greater than 0 but less than or equal to 1",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "from_data", points = c(2, 5, 3)),
    "points is not a data frame or points is not a matrix",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "from_data", points =
                       data.frame(
                         x = numeric(),
                         y = numeric(),
                         time_step = numeric())),
    "nrow(points) not greater than 0",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "from_data", points = test_points[, c(1, 2)]),
    "ncol(points) not equal to 3",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "from_data", points =
                       data.frame(
                         X = test_points[, 1],
                         Y = test_points[, 2],
                         time = test_points[, 3])),
    "columns in points parameter should have the following names: \"x\", \"y\", \"time_step\"", #nolint
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "from_data", points =
                       data.frame(
                         x = c(test_points[1:(nrow(test_points) - 1), 1], "character"), #nolint
                         y = test_points[, 2],
                         time_step = test_points[, 3])),
    "some element of point are not numeric",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "monitoring_based", cells_coords = c(2, 5, 3)),
    "cells_coords is not a data frame or cells_coords is not a matrix",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "monitoring_based", cells_coords = test_points),
    "ncol(cells_coords) not equal to 2",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "monitoring_based", cells_coords =
                       data.frame(
                         x = numeric(),
                         y = numeric())),
    "nrow(cells_coords) not greater than 0",
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "monitoring_based", cells_coords =
                       data.frame(
                         X = test_points[, 1],
                         Y = test_points[, 2])),
    "columns in cells_coords parameter should have the following names: \"x\", \"y\"", #nolint
    fixed = TRUE)

  expect_error(
    get_observations(test_sim_data, test_sim_res,
                     type = "monitoring_based", cells_coords =
                       data.frame(
                         x = c(test_points[1:(nrow(test_points) - 1), 1], "character"), #nolint
                         y = test_points[, 2])),
    "some element of cells_coords are not numeric",
    fixed = TRUE)

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


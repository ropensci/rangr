test_that("K_get_interpolation works", {
  K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
                                       package = "rangr"))

  expect_s4_class(
    K_get_interpolation(
      K_small_changing, c(1, 10, 15), time = 15), "SpatRaster")
})


test_that("K_check works", {
  K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
                                       package = "rangr"))

  set.seed(123)
  test_K_checked_01 <- K_check(K_small_changing, c(1, 10, 15), 15)
  test_K_checked_02 <- K_check(subset(K_small_changing, 1:2), NULL, 15)


  expect_type(K_check(K_small_changing, c(1, 10, 15), time = 15), "list")
  expect_length(K_check(K_small_changing, c(1, 10, 15), time = 15), 2)

  #' @srrstats {G5.2, G5.2a, G5.2b} tests of errors and warnings (with messages)
  #' @srrstats {G5.5} correctness test with fixed random seed
  #' @srrstats {G5.8, G5.8a, G5.8b, G5.8c} edge condition tests:
  #' zero-length data, unsupported data types, NA fields

  expect_error(
    K_check(K_small_changing, NULL, NULL),
    "Either \"K_time_points\" or \"time\" must be specified")


  expect_error(
    K_check(subset(K_small_changing, 1:2), NULL, numeric()),
    "length(time) not equal to 1",
    fixed = TRUE)

  expect_error(
    K_check(K_small_changing, numeric(), NULL),
    "Incorrect number of elements in \"K_time_points\"")

  expect_error(
    K_check(K_small_changing, c(1, 10), NULL),
    "Incorrect number of elements in \"K_time_points\"")

  expect_error(
    K_check(K_small_changing, c(5, 10, 15), NULL),
    "First element of \"K_time_points\" should be equal to 1")

  expect_error(
    K_check(K_small_changing, c(1, 10, -12), NULL),
    "Elements of \"K_time_points\" must be positive integers")

  expect_error(
    K_check(K_small_changing, c(1, 5.5, 15), NULL),
    "Elements of \"K_time_points\" must be integers")

  expect_error(
    K_check(K_small_changing, c(1, 5, 15), 20),
    "Last element of \"K_time_points\" should be equal to \"time\"")


  expect_warning(
    K_check(K_small_changing, c(1, 5, 15), NULL),
    "Argument \"time\" is no specified - last number from \"K_time_points\" is used as \"time\"" #nolint
  )


  expect_equal(test_K_checked_01$time, 15)
  expect_equal(test_K_checked_02$time, 15)
})



test_that("K_interpolate works", {
  K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
                                       package = "rangr"))

  goal_K_interpolated <- rast(test_path(
    "fixtures", "test_interpolated_raster.tif"
  ))
  test_time_points <- c(1, 10, 15)

  test_K_interpolated <- K_interpolate(
    K_small_changing,
    test_time_points,
    test_time_points[length(test_time_points)]
  )

  expect_true(compareGeom(test_K_interpolated, goal_K_interpolated))
  expect_equal(
    res(test_K_interpolated),
    res(goal_K_interpolated)
  )
  expect_true(all(round(values(test_K_interpolated)) ==
                    round(values(goal_K_interpolated)), na.rm = TRUE))
})


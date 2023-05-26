test_that("K_get_interpolation works", {
  K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
                                       package = "rangr"))

  expect_s4_class(
    K_get_interpolation(K_small_changing, c(1, 10, 15), time = 15), "SpatRaster")
})

test_that("K_check works", {
  K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
                                       package = "rangr"))

  test_K_checked_01 <- K_check(K_small_changing, c(1, 10, 15), 15)
  test_K_checked_02 <- K_check(subset(K_small_changing, 1:2), NULL, 15)


  expect_type(K_check(K_small_changing, c(1, 10, 15), time = 15), "list")
  expect_length(K_check(K_small_changing, c(1, 10, 15), time = 15), 2)

  expect_error(K_check(K_small, NULL, NULL))
  expect_error(K_check(K_small_changing, NULL, NULL))
  expect_error(K_check(K_small_changing, c(1, 10), NULL))
  expect_error(K_check(K_small_changing, c(5, 10, 15), NULL))
  expect_error(K_check(K_small_changing, c(1, 10, -12), NULL))
  expect_error(K_check(K_small_changing, c(1, 5.5, 15), NULL))
  expect_error(K_check(K_small_changing, c(1, 5, 15), 20))


  expect_warning(
    K_check(K_small_changing, c(1, 5, 15), NULL),
    "Argument \"time\" is no specified - last number from \"K_time_points\" is used as \"time\""
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


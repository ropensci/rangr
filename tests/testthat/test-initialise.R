test_that("target ids precalculation works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_resolution <- 1000
  test_max_dist <- 2000
  test_data_table <- readRDS(test_path("fixtures", "test_data_table_mini.rds"))


  test_within_list <- !is.na(test_data_table[, "K"])
  test_id_within <- test_data_table[test_within_list, "id"]
  test_data <- cbind(test_data_table[, c("id", "x", "y")], dist = NA)

  dist_list_res <- readRDS(test_path("fixtures", "test_dlist_mini.rds"))

  test_sim_data <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    r = log(1.2),
    max_dist = 1000,
    calculate_dist = FALSE
  )

  expect_equal(calc_dist(
    TRUE, test_rast, test_data_table, test_resolution, test_id_within,
    test_max_dist, FALSE, TRUE, NULL), dist_list_res)
  expect_null(calc_dist(
    FALSE, test_rast, test_data_table, test_resolution, test_id_within,
    test_max_dist, FALSE, TRUE, NULL))
  expect_null(test_sim_data$dlist)

  expect_equal(
    dist_list(test_rast, test_data_table, test_resolution, test_id_within,
              test_max_dist, FALSE, NULL), dist_list_res)
  expect_equal(
    target_ids(1, raster::raster(test_rast), test_data, test_resolution,
               test_max_dist, test_id_within), dist_list_res[[1]])
})


test_that("ncell_in_circle works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  test_ncells_in_circle <-
    readRDS(test_path("fixtures", "test_ncells_in_circle_mini.rds"))

  expect_equal(ncell_in_circle(test_rast), test_ncells_in_circle)
})


test_that("get_initialise_call works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))

  test_sim_data_1 <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    r = log(1.2),
    max_dist = 1000
  )

  test_sim_data_2 <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    r = log(1.2),
    dlist = test_sim_data_1$dlist
  )

  expect_equal("dlist" %in% names(test_sim_data_1$call), FALSE)
  expect_equal("dlist" %in% names(test_sim_data_2$call), TRUE)
})


test_that("K_add_stochasticity works", {
  set.seed(5)

  test_rast <- rast(test_path("fixtures", "test_rast.tif"))

  test_rast_many_layers <- c(test_rast, test_rast)
  test_sd <- 2

  test_sim_data_1 <- initialise(
    n1_map = test_rast,
    K_map = test_rast,
    K_sd = test_sd,
    r = log(1.2),
    max_dist = 1000
  )

  test_sim_data_2 <- initialise(
    n1_map = test_rast,
    K_map = test_rast_many_layers,
    K_sd = test_sd,
    r = log(1.2),
    max_dist = 1000
  )


  expect_s4_class(test_sim_data_1$K_map, "SpatRaster")
  expect_s4_class(test_sim_data_2$K_map, "SpatRaster")
})

test_that("K_n1_map_check works", {
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))

  test_rast_invalid <- test_rast
  values(test_rast_invalid)[2] <- -1

  expect_error(K_n1_map_check(
    n1_map = test_rast,
    K_map = test_rast_invalid,
    FALSE
  ))
  expect_error(K_n1_map_check(
    n1_map = test_rast_invalid,
    K_map = test_rast,
    FALSE
  ))
  expect_error(K_n1_map_check(
    n1_map = test_rast_invalid,
    K_map = test_rast_invalid,
    FALSE
  ))
})


test_that("K_get_init_values works", {
  test_rast_layer1 <- rast((test_path("fixtures", "test_rast.tif")))

  test_rast_layer2 <- test_rast_layer1 + 6


  test_rast_many_layers <- c(test_rast_layer1, test_rast_layer2)
  test_values_layer1 <- values(test_rast_layer1)

  expect_equal(K_get_init_values(test_rast_many_layers, TRUE), test_values_layer1)
})

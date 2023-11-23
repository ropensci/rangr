test_that("one_dist_sq_disp works", {

  #' @srrstats {G5.5} correctness test with fixed random seed
  set.seed(123)

  test_j_01 <- 1 # distance
  test_j_02 <- 2 # distance
  test_j_03 <- 3 # distance

  test_id <- 1 # cell number
  test_dlist_pos_01 <-
    (readRDS(test_path("fixtures", "test_dlist_mini.rds")))[[test_id]]
  test_dlist_pos_01[[test_j_02]] <- test_dlist_pos_01[[test_j_02]][1]

  test_dlist_pos_02 <- test_dlist_pos_01
  test_dlist_pos_02[1] <- list(NULL)

  # dispersing individuals
  test_disp_dist_i_01 <- c(2, 0)
  test_disp_dist_i_02 <- c(2, 4)


  test_data_table_01 <-
    readRDS(test_path("fixtures", "test_data_table_mini.rds"))
  test_data_table_02 <- test_data_table_01
  test_data_table_02[, "K"] <- 0

  test_dens_dep_01 <- "K2N"
  test_dens_dep_02 <- "K"
  test_dens_dep_03 <- "none"

  test_ncells_in_circle <-
    readRDS(test_path("fixtures", "test_ncells_in_circle_mini.rds"))

  test_border_01 <- "absorbing"
  test_border_02 <- "reprising"


  # absorbing vs. reprising borders
  expect_lte(
    length(one_dist_sq_disp(
      test_j_01, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_01, test_dens_dep_01, test_ncells_in_circle, test_border_01 #nolint
    )),
    test_disp_dist_i_01[test_j_01])

  expect_equal(
    length(one_dist_sq_disp(
      test_j_01, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_01, test_dens_dep_01, test_ncells_in_circle, test_border_02 #nolint
    )),
    test_disp_dist_i_01[test_j_01])

  # no individuals to disperse
  expect_null(
    one_dist_sq_disp(
      test_j_02, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_01, test_dens_dep_01, test_ncells_in_circle, test_border_02 #nolint
    ))

  # no cell available
  expect_null(
    one_dist_sq_disp(
      test_j_03, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_01, test_dens_dep_01, test_ncells_in_circle, test_border_01 #nolint
    ))

  expect_equal(
    one_dist_sq_disp(
      test_j_01, test_id, test_dlist_pos_02, test_disp_dist_i_01,
      test_data_table_01, test_dens_dep_01, test_ncells_in_circle, test_border_02 #nolint
    ),
    rep(test_id, times = test_disp_dist_i_01[test_j_01]))

  # no dispersal if K == 0 in all target cells if dens dep ~ K
  # and dispersal if no dens dep
  expect_equal(
    one_dist_sq_disp(
      test_j_01, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_02, test_dens_dep_01, test_ncells_in_circle, test_border_02 #nolint
    ),
    rep(test_id, times = test_disp_dist_i_01[test_j_01]))

  expect_equal(
    one_dist_sq_disp(
      test_j_01, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_02, test_dens_dep_02, test_ncells_in_circle, test_border_01 #nolint
    ),
    rep(test_id, times = test_disp_dist_i_01[test_j_01]))

  expect_false(
    all(one_dist_sq_disp(
      test_j_01, test_id, test_dlist_pos_01, test_disp_dist_i_01,
      test_data_table_02, test_dens_dep_03, test_ncells_in_circle, test_border_02 #nolint
    ) ==
      rep(test_id, times = test_disp_dist_i_01[test_j_01])))


  # only one target cell available
  expect_equal(
    one_dist_sq_disp(
      test_j_02, test_id, test_dlist_pos_01, test_disp_dist_i_02,
      test_data_table_01, test_dens_dep_03, test_ncells_in_circle, test_border_02 #nolint
    ),
    rep(test_dlist_pos_01[[test_j_02]], times = test_disp_dist_i_02[test_j_02]))
})


test_that("target_ids_in_disp works", {

  #' @srrstats {G5.5} correctness test with fixed random seed
  set.seed(123)

  test_data_table_01 <-
    readRDS(test_path("fixtures", "test_data_table_mini.rds"))
  test_id <- 1 # cell number
  test_id_x_y <-
    test_data_table_01[test_data_table_01[, "id"] == test_id, c("id", "x", "y")]
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  # reclassify to remove NaNs (that were NAs before saving)
  test_rast <- classify(test_rast, cbind(NaN, NA))
  test_id_within <- test_data_table_01[!is.na(test_data_table_01[, "K"]), "id"]
  test_resolution <- res(test_rast)[1]

  test_dlist_pos_01 <-
    (readRDS(test_path("fixtures", "test_dlist_mini.rds")))[[test_id]]

  # no target cells
  expect_null(target_ids_in_disp(
    id_x_y = test_id_x_y,
    id = test_rast,
    id_within = test_id_within,
    resolution = test_resolution,
    min = 10,
    max = 12
  ))

  # targets for one distance
  expect_equal(target_ids_in_disp(
    id_x_y = test_id_x_y,
    id = test_rast,
    id_within = test_id_within,
    resolution = test_resolution,
    min = 1,
    max = 1
  ), test_dlist_pos_01[1])

  # targets for more distances
  expect_equal(target_ids_in_disp(
    id_x_y = test_id_x_y,
    id = test_rast,
    id_within = test_id_within,
    resolution = test_resolution,
    min = 1,
    max = 2
  ), test_dlist_pos_01)
})


test_that("sq_disp works", {
  test_i_01 <- 1
  test_i_03 <- 3
  test_disp_dist_01 <- list(c(2, 1), 0, c(4, 3, 0, 1), c(1, 1))
  test_data_table_01 <-
    readRDS(test_path("fixtures", "test_data_table_mini.rds"))
  test_id_within_01 <-
    test_data_table_01[!is.na(test_data_table_01[, "K"]), "id"]
  test_id_ok_01 <-
    test_data_table_01[
      !is.na(test_data_table_01[, "K"]) & test_data_table_01[, "N"] > 0, "id"]
  test_dlist_01 <- readRDS(test_path("fixtures", "test_dlist_mini.rds"))
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  # reclassify to remove NaNs (that were NAs before saving)
  test_rast <- classify(test_rast, cbind(NaN, NA))
  test_resolution <- res(test_rast)[1]

  test_dens_dep_01 <- "K2N"
  test_dens_dep_02 <- "K"
  test_dens_dep_03 <- "none"

  test_ncells_in_circle <-
    readRDS(test_path("fixtures", "test_ncells_in_circle_mini.rds"))

  test_border_01 <- "absorbing"
  test_border_02 <- "reprising"


  expect_lte(
    length(sq_disp(
      i = test_i_01,
      disp_dist = test_disp_dist_01,
      id_within = test_id_within_01,
      id_ok = test_id_ok_01,
      dlist = test_dlist_01,
      data_table = test_data_table_01,
      id = test_rast,
      resolution = test_resolution,
      dens_dep = test_dens_dep_01,
      ncells_in_circle = test_ncells_in_circle,
      border = test_border_01
    )),
    sum(test_disp_dist_01[[test_i_01]])
  )

  expect_length(
    sq_disp(
      i = test_i_03,
      disp_dist = test_disp_dist_01,
      id_within = test_id_within_01,
      id_ok = test_id_ok_01,
      dlist = test_dlist_01,
      data_table = test_data_table_01,
      id = test_rast,
      resolution = test_resolution,
      dens_dep = test_dens_dep_01,
      ncells_in_circle = test_ncells_in_circle,
      border = test_border_02
    ),
    sum(test_disp_dist_01[[test_i_03]])
  )
})


test_that("dists_tab works", {
  test_N_pos_01 <- c(4, 1, 20)
  test_N_pos_02 <- 1
  test_N_pos_03 <- 0.5
  test_N_pos_04 <- -5

  test_kernel_01 <- function(n) match.fun("rexp")(n, rate = 1 / 1e3)

  test_resolution <- 1e3

  expect_length(
    dists_tab(test_N_pos_01, test_kernel_01, test_resolution),
    length(test_N_pos_01))
  expect_length(
    dists_tab(test_N_pos_02, test_kernel_01, test_resolution),
    length(test_N_pos_02))
  expect_equal(
    unlist(dists_tab(test_N_pos_03, test_kernel_01, test_resolution)),
    0)
  expect_error(
    dists_tab(test_N_pos_04, test_kernel_01, test_resolution))
})



test_that("disp works", {
  test_data_table_01 <-
    readRDS(test_path("fixtures", "test_data_table_mini.rds"))
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  # reclassify to remove NaNs (that were NAs before saving)
  test_rast <- classify(test_rast, cbind(NaN, NA))
  test_id_rast <- rast(test_path("fixtures", "test_id_rast.tif"))
  test_N_t <- as.matrix(test_rast, wide = TRUE)
  test_kernel_01 <- function(n) match.fun("rexp")(n, rate = 1 / 1e3)

  test_dens_dep_01 <- "K2N"
  test_dens_dep_02 <- "K"
  test_dens_dep_03 <- "none"

  test_dlist_01 <- readRDS(test_path("fixtures", "test_dlist_mini.rds"))
  test_id_within_01 <-
    test_data_table_01[!is.na(test_data_table_01[, "K"]), "id"]
  test_within_mask <- !is.na(test_N_t)

  test_border_01 <- "absorbing"
  test_border_02 <- "reprising"

  test_resolution <- res(test_rast)[1]
  test_max_dist <- 3 * test_resolution

  test_ncells_in_circle <-
    readRDS(test_path("fixtures", "test_ncells_in_circle_mini.rds"))


  disp_res_01 <- disp(
    N_t = test_N_t,
    id = test_id_rast,
    data_table = test_data_table_01,
    kernel = test_kernel_01,
    dens_dep = test_dens_dep_01,
    dlist = test_dlist_01,
    id_within = test_id_within_01,
    within_mask = test_within_mask,
    border = test_border_01,
    max_dist = test_max_dist,
    resolution = test_resolution,
    ncells_in_circle = test_ncells_in_circle
  )

  disp_res_02 <- disp(
    N_t = test_N_t,
    id = test_id_rast,
    data_table = test_data_table_01,
    kernel = test_kernel_01,
    dens_dep = test_dens_dep_02,
    dlist = NULL,
    id_within = test_id_within_01,
    within_mask = test_within_mask,
    border = test_border_02,
    max_dist = test_max_dist,
    resolution = test_resolution,
    ncells_in_circle = test_ncells_in_circle
  )


  expect_true(all((test_N_t - disp_res_01$em + disp_res_01$im) >= 0,
                  na.rm = TRUE))
  expect_true(all((test_N_t - disp_res_02$em + disp_res_02$im) >= 0,
                  na.rm = TRUE))
})


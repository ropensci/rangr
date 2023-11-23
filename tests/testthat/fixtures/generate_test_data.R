#' @srrstats {G5.1} The test data sets can be easily reproduced using this file
#' available on github

# Warning: Use this file only with a stable package version! Make sure
# Ensure that the generated data is correct before you save it.

library(rangr)
library(terra)

n1_small <- rast(system.file("input_maps/n1_small.tif", package = "rangr"))
K_small <- rast(system.file("input_maps/K_small.tif", package = "rangr"))

set.seed(123)

nrow <- 5
ncol <- 6
vals <- matrix(sample(10:20, nrow * ncol, replace = TRUE),
               nrow = nrow, ncol = ncol)

vals_na <- c(7, 18, 29)
vals[vals_na] <- NA

vals_zero <- c(6, 16)
vals[vals_zero] <- 0


test_rast <- rast(
  nrows = nrow(vals),
  ncols = ncol(vals),
  exten = c(267000, 273000, 607000, 612000),
  vals = vals,
  crs = "EPSG:2180"
)

test_id_rast <- test_rast
values(test_id_rast) <- matrix(1:(nrow(test_rast)*ncol(test_rast)),
                               nrow(test_rast), ncol(test_rast))

K_small_changing <- rast(system.file("input_maps/K_small_changing.tif",
                                     package = "rangr"))
test_interpolated_raster <- K_get_interpolation(K_small_changing, c(1, 10, 15))

test_data_table <- as.matrix(data.frame(
  values(test_id_rast),
  xyFromCell(test_rast, cell = 1:ncell(test_rast)),
  values(test_rast),
  ifelse(values(test_rast) == 0, 0, values(test_rast) - 2)
))
colnames(test_data_table) <- c("id", "x", "y", "K", "N")
test_data_table <- test_data_table[order(test_data_table[, "id"]), ]


test_dlist <- rangr:::calc_dist(
  calculate_dist = TRUE,
  id = test_id_rast,
  data_table = test_data_table,
  resolution = res(test_id_rast)[1],
  id_within = test_data_table[!is.na(test_data_table[, "K"]), "id"],
  max_dist = 2000,
  progress_bar = TRUE,
  quiet = TRUE,
  cl = NULL
)

test_ncells_in_circle <- rangr:::ncell_in_circle(test_rast)

test_time <- 20
test_burn <- 2
test_sim_data <- initialise(
  n1_map = test_rast, K_map = test_rast, r = log(2), rate = 1 / 1e3)
test_sim_res <- sim(obj = test_sim_data, time = test_time, burn = test_burn)

test_points <- data.frame(
  x = rep(test_data_table[1:5, "x"], times = test_time - test_burn),
  y = rep(test_data_table[1:5, "y"], times = test_time - test_burn),
  time_step = rep(1:(test_time - test_burn), each = 5))




writeRaster(
  test_rast,
  filename = "tests/testthat/fixtures/test_rast.tif",
  overwrite = TRUE)
writeRaster(
  test_interpolated_raster,
  filename = "tests/testthat/fixtures/test_interpolated_rast.tif",
  overwrite = TRUE)
writeRaster(
    test_id_rast,
    filename = "tests/testthat/fixtures/test_id_rast.tif",
    overwrite = TRUE)
saveRDS(
  test_dlist,
  file = "tests/testthat/fixtures/test_dlist_mini.rds")
saveRDS(
  test_data_table,
  file = "tests/testthat/fixtures/test_data_table_mini.rds")
saveRDS(
  test_ncells_in_circle,
  file = "tests/testthat/fixtures/test_ncells_in_circle_mini.rds")
saveRDS(
  test_sim_data,
  file = "tests/testthat/fixtures/test_sim_data.rds")
saveRDS(
  test_sim_res,
  file = "tests/testthat/fixtures/test_sim_res.rds")
saveRDS(
  test_points,
  file = "tests/testthat/fixtures/test_points.rds")


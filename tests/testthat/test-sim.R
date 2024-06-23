test_that("sim works", {

  test_time <- 20
  test_time_error <- -5
  test_rast <- rast(test_path("fixtures", "test_rast.tif"))
  # reclassify to remove NaNs (that were NAs before saving)
  test_rast <- classify(test_rast, cbind(NaN, NA))
  test_rast_many_layers <- c(test_rast, test_rast, test_rast)

  test_initialised_obj <- initialise(test_rast, test_rast, r = 2)
  test_initialised_obj_many_layers <-
    initialise(test_rast, test_rast_many_layers, r = 2)
  test_initialised_obj_ext <- initialise(test_rast, test_rast, r = log(0))

  test_sim_res <- sim(test_initialised_obj, time = test_time)
  test_sim_res_many_layers_ok <-
    sim(test_initialised_obj_many_layers, time = nlyr(test_rast_many_layers))
  test_sim_res_ext <- sim(test_initialised_obj_ext, time = test_time)


  #' @srrstats {G5.2, G5.2a, G5.2b} tests of errors and warnings (with messages)
  # input parameters
  expect_error(
    sim("test_initialised_obj_ext", time = test_time),
    regexp = "obj does not inherit from class sim_data"
    )

  expect_error(
    sim(test_initialised_obj, time = c(test_time, 5)),
    regexp = "length\\(time\\) not equal to 1"
    )

  expect_error(
    sim(test_initialised_obj, time = "test_time"),
    regexp = "time is not a numeric or integer vector"
    )

  expect_error(
    sim(test_initialised_obj, time = test_time_error),
    regexp = "time not greater than 1"
    )


  expect_error(
    sim(test_initialised_obj, time = test_time, burn = c(test_time, 5)),
    regexp = "length\\(burn\\) not equal to 1"
    )

  expect_error(
    sim(test_initialised_obj, time = test_time, burn = "test_time"),
    regexp = "burn is not a numeric or integer vector"
    )

  expect_error(
    sim(test_initialised_obj, time = test_time, burn = -1),
    regexp = "burn not greater than or equal to 0"
    )


  #' @srrstats {G5.4, G5.4a} correctness tests: trivial cases
  # correct
  expect_length(test_sim_res, 3)
  expect_equal(dim(test_sim_res$N_map)[3], test_time)
  expect_true(all(test_sim_res$N >= 0, na.rm = TRUE) &&
                all(test_sim_res$N == round(test_sim_res$N), na.rm = TRUE))

  # correct (with changing environment)
  expect_length(test_sim_res_many_layers_ok, 3)
  expect_equal(dim(test_sim_res_many_layers_ok$N_map)[3],
               nlyr(test_rast_many_layers))


  # correct (extinction)
  expect_true(test_sim_res_ext$extinction)
  expect_lt(dim(test_sim_res_ext$N)[3], test_time)
  expect_message(sim(test_initialised_obj_ext, time = test_time, quiet = FALSE),
   "^Population extinct after \\d+/\\d+ time steps \\(including burned: \\d+/\\d+\\)\\s*$" #nolint
  )

  # incorrect number of layers/time steps (with changing environment)
  expect_error(
    sim(test_initialised_obj_many_layers, time = test_time),
    regexp = "Number of layers in \"K_map\" and \"time\"  are not equal")

  # incorrect burn
  expect_error(
    sim(test_initialised_obj_ext, time = test_time, burn = 15),
    regexp = "Simulation failed to reach specified time steps treshold \\(by \"burn\" parameter\\) - nothing to return." #nolint
    )

  #' @srrstats {G5.3} test for NaNs in sim_results
  # NaN in abundances
  expect_true(all(!is.nan(test_sim_res_many_layers_ok$N_map)))

})

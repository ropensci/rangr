# rangr 1.0.4 (2024-05-30)

### Major improvements

-   Added support for lon/lat rasters as input maps

### Minor improvements

-   Default value of `max_dist` in `initialise()` is now equal to 0.99 quantile of `kernel_fun` instead of 0.9

-   Added examples of lon/lat rasters to package data

-   Input maps (`K_map` and `n1_map`) are now wrapped in `sim_data` object

# rangr 1.0.3 (2024-01-23)

### Minor improvements

-   Remove `print()` from the vignette

-   All messages produced during `initialise()` are now turned off by default (by `quiet` parameter)

-   Changed the appearance of the progress bar in the `sim()` function to match that in `initialise()`

-   In `summary.sim_data()` the `r` parameter is now rounded to the 4th decimal place

# rangr 1.0.2 (2024-01-16)

### Major improvements

-   `rangr` is now based on the `terra` package

-   Added binomial distribution to `get_observation()` (new distribution defining the observation process)

### Minor improvements

-   Added more default values for `initialize()`'s parameters

-   Improved documentation for `get_observation()`

-   Improved documentation - titles formatting

-   Added more examples to `plot.sim_results()`

# rangr 1.0.1 (2023-09-04)

### Minor improvements

-   Improved documentation for `get_observations()`.

-   Added functionality description for `get_observations()` to the vignettes

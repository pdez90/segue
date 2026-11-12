# Synthetic data for testing
synth_data <- sf::st_sf(
  unit_id = paste0("cell_", 1:10),
  group_a = c(20, 10, 0, 30, 20, 10, 0, 30, 20, 10),
  group_b = c(30, 20, 10, 10, 30, 20, 10, 10, 30, 20),
  group_c = c(50, 20, 30, 10, 50, 20, 30, 10, 50, 20),
  geometry = sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(
      c(xmin = 92000, ymin = 435000,
        xmax = 93000, ymax = 437500),
      crs = sf::st_crs(28992) # CRS of the Netherlands
    )),
    cellsize = 500
  )
)

#https://erc-segue.nl/tdd-r-pkg/practical.html
# Tests:
# Test 1: the shares calculated by the function, calculate_group_shares are correct
testthat::test_that("group shares are calculated correctly", {

  result<- calculate_group_shares(synth_data)
  #Shares are calculated correctly
  testthat::expect_equal(result$share_a, c(0.2, 0.2, 0, 0.6, 0.2, 0.2, 0, 0.6, 0.2, 0.2))
  testthat::expect_equal(result$share_b, c(0.3, 0.4, 0.25, 0.2, 0.3, 0.4, 0.25, 0.2, 0.3, 0.4))
  testthat::expect_equal(result$share_c, c(0.5, 0.4, 0.75, 0.2, 0.5, 0.4, 0.75, 0.2, 0.5, 0.4))

})

#Test 2: function rejects invalid group counts (negative or missing)
testthat::test_that("invalid group counts are rejected", {

  #Copy data to modify it
  negative_data<-synth_data
  negative_data$group_a[1]<- -1

  #Expected error from the function: function should flag negative values
  testthat::expect_error(
    calculate_group_shares(negative_data),
    "non-negative"
  )

  #Copy data to modify it
  missing_data<-synth_data
  missing_data$group_a[1]<-NA

  testthat::expect_error(
    calculate_group_shares(missing_data),
    "missing"
  )

})

#Test 3: class and geometry in output are preserved
testthat::test_that("class and geometry are preserved in output", {
    result<- calculate_group_shares(synth_data)
    testthat::expect_s3_class(result, "sf")
    testthat::expect_equal(sf::st_geometry(result), sf::st_geometry(synth_data))

})

#devtools::test() run in console
#goes to test folder and runs tests

# Make as many copies of the test block as you need
testthat::test_that(" Is the vartype supplied by the user really supported by the data", {
	# Test to be added here
})

# Make as many copies of the test block as you need
testthat::test_that(" Is the data spatial, when the user says it is", {
  # Test to be added here
})


testthat::test_that("Have the users provided the three required inputs for the function seg_index", {
  # Test to be added here
})

testthat::test_that("If the user enters any of all inputs do we get the full table", {
  # Test to be added here
})

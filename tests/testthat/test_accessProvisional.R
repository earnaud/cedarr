if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  # True positive
  test_that("Accessing all provisional classes is valid.", {
    expect_equal(
      accessProvisional(my_api_key, output_mode = "full")$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  # True positive
  test_that("Accessing all provisional classes is valid.", {
    expect_equal(
      accessProvisional(my.api.key, output.mode = "full")$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

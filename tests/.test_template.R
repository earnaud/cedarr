if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  test_that("<test title> is valid / returns error.", {
    # test
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)


  # True positive
  # TEMPORARILY UNAVAILABLE

  # True negative
  test_that("Accessing undefined relation is valid", {
    sapply(list("patate", NULL), \(.id){
      expect_error(
        accessRelation(
          api_key = my_api_key,
          id = .id,
          output_mode = "full"
        )
      )
    })
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

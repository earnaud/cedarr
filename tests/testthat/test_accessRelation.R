if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)


  # True positive
  # TEMPORARILY UNAVAILABLE

  # True negative
  test_that("Accessing undefined relation is valid", {
    sapply(list("patate", NULL), \(.id){
      expect_error(
        accessRelation(
          api.key = my.api.key,
          id = .id,
          output.mode = "full"
        )
      )
    })
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

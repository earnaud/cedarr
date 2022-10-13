if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  test_that("Accessing 'biome' in ENVO is valid.", {
    expect_equal(
      accessValues(
        my.api.key,
        "ENVO",
        id = "http://purl.obolibrary.org/obo/ENVO_00000428"
      )$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

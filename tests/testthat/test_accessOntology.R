if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  # True positive
  test_that("Access Ontology without target is valid", {
    expect_equal(
      accessOntology(
        api_key = my_api_key,
        output_mode = "full"
      )$status_code,
      200
    )
  })

  test_that("Access Ontology with target or NA is valid", {
    sapply(list("classes", NA_character_) , \(.item) {
      expect_equal(
        accessOntology(
          my_api_key,
          "ENVO",
          item = .item,
          sub = "roots",
          output_mode = "full"
        )$status_code,
        200
      )
    })
  })

  # True negative
  test_that("Access Ontology with misformed or missing target is invalid", {
    sapply(list("patate", NULL) , \(.item) {
      expect_error(
        accessOntology(
          my_api_key,
          "ENVO",
          item = .item,
          sub = "roots",
          output_mode = "full"
        )
      )
    })
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  # True positive
  test_that("Access Ontology without target is valid", {
    expect_equal(
      accessOntology(
        api.key = my.api.key,
        output.mode = "full"
      )$status_code,
      200
    )
  })

  test_that("Access Ontology with target or NA is valid", {
    sapply(list("classes", NA_character_) , \(.item) {
      expect_equal(
        accessOntology(
          my.api.key,
          "ENVO",
          item = .item,
          sub = "roots",
          output.mode = "full"
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
          my.api.key,
          "ENVO",
          item = .item,
          sub = "roots",
          output.mode = "full"
        )
      )
    })
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

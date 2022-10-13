if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  # True Positive
  test_that("Accessing the term \"biome\" in ENVO is valid.", {
    expect_equal(
      accessClass(
        api.key = my.api.key,
        ontology = "ENVO",
        id = "http://purl.obolibrary.org/obo/ENVO_00000428",
        output.mode = "full"
      )$status_code,
      200
    )
  })

  # True Negative
  test_that("Accessing a non-url term in ENVO is invalid", {
    sapply(list("biome", NA_character_, NULL), \(.id)
      expect_error(
        accessClass(
          api.key = my.api.key,
          ontology = "ENVO",
          id = "biome",
          output.mode = "full"
        )
      )
    )
  })

  test_that("Accessing a non-existent term in ontology is invalid", {
    expect_error(
      accessClass(
        api.key = my.api.key,
        ontology = "ENVO",
        # 'is about' from Information Artifact Ontology
        id = "http://purl.obolibrary.org/obo/IAO_0000136",
        output.mode = "full"
      )
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

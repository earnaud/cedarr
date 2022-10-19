if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  # True Positive
  test_that("Accessing the term \"biome\" in ENVO is valid.", {
    expect_equal(
      accessClass(
        api_key = my_api_key,
        ontology = "ENVO",
        id = "http://purl.obolibrary.org/obo/ENVO_00000428",
        output_mode = "full"
      )$status_code,
      200
    )
  })

  # True Negative
  test_that("Accessing a non-url term in ENVO is invalid", {
    sapply(list("biome", NA_character_, NULL), \(.id)
      expect_error(
        accessClass(
          api_key = my_api_key,
          ontology = "ENVO",
          id = "biome",
          output_mode = "full"
        )
      )
    )
  })

  test_that("Accessing a non-existent term in ontology is invalid", {
    expect_error(
      accessClass(
        api_key = my_api_key,
        ontology = "ENVO",
        # 'is about' from Information Artifact Ontology
        id = "http://purl.obolibrary.org/obo/IAO_0000136",
        output_mode = "full"
      )
    )
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

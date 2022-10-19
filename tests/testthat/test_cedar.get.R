if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  # True positive
  test_that("Accessing 'terrestrial biome' in ENVO is valid.", {
    expect_equal(
      cedar.get(
        my.api.key,
        paste0(
          "https://terminology.metadatacenter.org/bioportal/ontologies/ENVO/classes/",
          URLencode("http://purl.obolibrary.org/obo/ENVO_00000446", reserved = TRUE)
        ),
        output.mode = "full"
      )$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

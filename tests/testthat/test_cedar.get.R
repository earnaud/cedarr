if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  # True positive
  test_that("Accessing 'terrestrial biome' in ENVO is valid.", {
    expect_equal(
      cedarGet(
        my_api_key,
        paste0(
          "https://terminology.metadatacenter.org/bioportal/ontologies/ENVO/classes/",
          URLencode("http://purl.obolibrary.org/obo/ENVO_00000446", reserved = TRUE)
        ),
        output_mode = "full"
      )$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  # True positive
  test_that("Accessing root properties in IAO is valid", {
    expect_equal(
      accessProperty(
        my_api_key,
        "IAO",
        id = "roots",
        output_mode ="full"
      )$status_code,
      200
    )
  })
  test_that("Accessing target property in IAO is valid", {
    expect_equal(
      accessProperty(
        my_api_key,
        "IAO",
        id = "http://purl.obolibrary.org/obo/IAO_0000118",
        output_mode ="full"
      )$status_code,
      200
    )
  })

  # False positive
  test_that("Accessing 'alternative term' in ENVO is invalid (wrong ontology)", {
    expect_length(
      accessProperty(
        my_api_key,
        "ENVO",
        id = "http://purl.obolibrary.org/obo/IAO_0000118",
        sub = "parents",
        output_mode ="content"
      ),
      0
    )
  })

  # True negative
  test_that("Accessing unknown term in ENVO is invalid", {
    expect_error(
      accessProperty(
        my_api_key,
        "ENVO",
        id = "patate",
        sub = "parents",
        output_mode ="full"
      )
    )
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  # True positive
  test_that("Accessing root properties in IAO is valid", {
    expect_equal(
      accessProperty(
        my.api.key,
        "IAO",
        id = "roots",
        output.mode ="full"
      )$status_code,
      200
    )
  })
  test_that("Accessing target property in IAO is valid", {
    expect_equal(
      accessProperty(
        my.api.key,
        "IAO",
        id = "http://purl.obolibrary.org/obo/IAO_0000118",
        output.mode ="full"
      )$status_code,
      200
    )
  })

  # False positive
  test_that("Accessing 'alternative term' in ENVO is invalid (wrong ontology)", {
    expect_length(
      accessProperty(
        my.api.key,
        "ENVO",
        id = "http://purl.obolibrary.org/obo/IAO_0000118",
        sub = "parents",
        output.mode ="content"
      ),
      0
    )
  })

  # True negative
  test_that("Accessing unknown term in ENVO is invalid", {
    expect_error(
      accessProperty(
        my.api.key,
        "ENVO",
        id = "patate",
        sub = "parents",
        output.mode ="full"
      )
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

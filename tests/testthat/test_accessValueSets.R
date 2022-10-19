if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  test_that("Accessing CEDARVS is valid.", {
    expect_equal(
      accessValueSets(
        my_api_key,
        vs.collection = "CEDARVS",
        output_mode = "full"
      )$status_code,
      200
    )
    expect_equal(
      accessValueSets(
        my_api_key,
        vs.collection = "CEDARVS",
        id = "http://www.semanticweb.org/jgraybeal/ontologies/2015/7/cedarvaluesets#Study_File_Type",
        output_mode = "full"
      )$status_code,
      200
    )
  })

  test_that("Accessing ENVO is invalid", {
    expect_error(
      accessValueSets(
        my_api_key,
        vs.collection = "ENVO"
      )
    )
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

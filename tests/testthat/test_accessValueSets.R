if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  test_that("Accessing CEDARVS is valid.", {
    expect_equal(
      accessValueSets(
        my.api.key,
        vs.collection = "CEDARVS",
        output.mode = "full"
      )$status_code,
      200
    )
    expect_equal(
      accessValueSets(
        my.api.key,
        vs.collection = "CEDARVS",
        id = "http://www.semanticweb.org/jgraybeal/ontologies/2015/7/cedarvaluesets#Study_File_Type",
        output.mode = "full"
      )$status_code,
      200
    )
  })

  test_that("Accessing ENVO is invalid", {
    expect_error(
      accessValueSets(
        my.api.key,
        vs.collection = "ENVO"
      )
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

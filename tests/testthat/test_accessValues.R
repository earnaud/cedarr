if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  # IGNORED: not functional in original API
  # test_that("Accessing 'Study file type' in CEDARVS is valid.", {
  #   expect_equal(
  #     accessValues(
  #       my.api.key,
  #       "CEDARVS",
  #       id = "http://www.semanticweb.org/jgraybeal/ontologies/2015/7/cedarvaluesets#Study_File_Type",
  #       output.mode = "full"
  #     )$status_code,
  #     200
  #   )
  # })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

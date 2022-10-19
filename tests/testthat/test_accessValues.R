if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  # IGNORED: not functional in original API
  # test_that("Accessing 'Study file type' in CEDARVS is valid.", {
  #   expect_equal(
  #     accessValues(
  #       my_api_key,
  #       "CEDARVS",
  #       id = "http://www.semanticweb.org/jgraybeal/ontologies/2015/7/cedarvaluesets#Study_File_Type",
  #       output_mode = "full"
  #     )$status_code,
  #     200
  #   )
  # })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

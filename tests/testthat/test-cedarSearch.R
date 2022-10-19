if(exists("my_api_key", envir = .GlobalEnv)){
  my_api_key <- get("my_api_key", envir = .GlobalEnv)

  test_that("Query: term = \"habitat\" returns error", {
    expect_error(
      cedarr::cedarSearch(
        my_api_key,
        "habitat",
        output_mode = "full"
      )
    )
  })

  test_that("Query: query = \"habitat\" sources = \"ENVO\" is valid", {
    expect_equal(
      cedarr::cedarSearch(
        api_key = my_api_key,
        query = "habitat",
        sources = "ENVO",
        output_mode = "full"
      )$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my_api_key\" for tests purposes.")
}

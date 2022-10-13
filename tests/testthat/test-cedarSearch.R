if(exists("my.api.key", envir = .GlobalEnv)){
  my.api.key <- get("my.api.key", envir = .GlobalEnv)

  test_that("Query: term = \"habitat\" returns error", {
    expect_error(
      cedarr::cedarSearch(
        my.api.key,
        "habitat",
        output.mode = "full"
      )
    )
  })

  test_that("Query: query = \"habitat\" sources = \"ENVO\" is valid", {
    expect_equal(
      cedarr::cedarSearch(
        api.key = my.api.key,
        query = "habitat",
        sources = "ENVO",
        output.mode = "full"
      )$status_code,
      200
    )
  })
} else {
  stop("Set a variable called \"my.api.key\" for tests purposes.")
}

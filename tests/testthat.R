library(testthat)
library(cedarr)

assign("my.api.key", "apiKey 8fe5be450c83f1db53a91fe97b64cb87bff3e46aa382e658a3c85fe32cf3c5bc", envir = .GlobalEnv)

test_local()

remove("my.api.key", envir = .GlobalEnv)

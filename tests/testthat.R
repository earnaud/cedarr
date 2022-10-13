library(testthat)
library(cedarr)

# apiKey of contact.pndb@mnhn.fr
assign("my.api.key", "apiKey f7470472214b8b49f83760e57753caddbc5d73164e247878ca3c46852211d36b", envir = .GlobalEnv)

# Most queries are tested with 'output.mode = full' to obtain status codes.
test_check("cedarr")

# remove("my.api.key", envir = .GlobalEnv)

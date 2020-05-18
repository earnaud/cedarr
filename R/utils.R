#' CEDAR HTTP get method
#'
#' Internal function wrapping the GET method.
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite validate fromJSON
#' @importFrom dplyr %>%
cedar.get <- function(api.key, url, ..., output.mode = "content"){
  #TODO add validity checks
  result <- GET(
    url,
    ...,
    add_headers(Authorization = paste("apiKey", cedar.client@api.key))
  )
  if(is.raw(result$content))
    result$content <- result$content %>% rawToChar
  if(validate(result$content))
    result$content <- result$content %>% fromJSON
  message("* Request status:", result$status_code)

  # Output ====
  if(output.mode == "full" || result$status_code != "200")
    return(result)
  else
    return(result$content)
}

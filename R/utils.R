#' CEDAR HTTP get method
#'
#' Internal function wrapping the GET method.
#'
#' @param api.key character. An API Key is required to access any
#' API call. It is used within {cedarr} as a header for http
#' requests. An API key is linked to a CEDAR account
#' (https://cedar.metadatacenter.org/profile)
#' @param url character. An URL to the CEDAR resource to query data from.
#' @param ... arguments to input in the request body.
#' @param output.mode character. "full" will return the whole
#' response object (from {httr}) or "content" will fetch the
#' interest values from the response object. Getting the whole
#' object might be interesting to have a look at system metadata,
#' or in case of error to debug the connection. (defaults to
#' "content")
#'
#' @return
#'
#' If `output.mode = "full"`, the whole http response object (see httr::response).
#' It is structured as a list with response metadata wrapping the `content` item
#' which contains the wanted result.
#'
#' If `output.mode = "content"`, the `content` item is directly returned, containing
#' database metadata and the interesting information in the `collection` subitem.
#'
#' @importFrom httr GET add_headers
#' @importFrom jsonlite validate fromJSON
#' @importFrom dplyr %>%
cedar.get <- function(api.key, url, ..., output.mode = "content"){
  if(missing(api.key))
    stop("No API key provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(url))
    stop("No URL provided: required to target resources.")
  if(isFALSE(is.character(output.mode) && output.mode %in% c("full","content")))

  message(url)

  result <- GET(
    url,
    ...,
    add_headers(Authorization = paste("apiKey", api.key))
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

#' Correct arguments length.
#'
#' @param ... list. A list of arguments. Length of each item in this list will be
#' shortened to 1.
#' @param check ArgCheck. An ArgCheck object from package {ArgumentCheck}.
#' @param env environment. The environment in which  ... is being evaluated.
#'
#' @details
#'
#' This function will perform side-effects actions, shortening excessively long
#' items to length-one items.
#'
#' @return
#'
#' The `check` argument.
#'
#' Check for each given argument in `...` that their length > 1.
checkLength <- function(..., check, env = .GlobalEnv){
  args <- as.list(...)

  sapply(args, function(arg, .env = env){
    arg.value <- get(arg, envir = .env)
    if(length(arg.value) > 1){
      assign(arg, arg.value, envir = .env)
      addWarning(
        msg="`",arg,"` argument had length>1: only the first element is used.",
        argcheck = check
      )
    }
  })

  return(check)
}

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
#' @importFrom dplyr %>%
#' @importFrom httr GET add_headers
#' @importFrom jsonlite validate fromJSON
#' @importFrom ArgumentCheck addWarning addError
cedar.get <- function(api.key, url, ..., output.mode = "content"){
  if(missing(api.key))
    stop("No API key provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(url))
    stop("No URL provided: required to target resources.")

  result <- httr::GET(
    url,
    ...,
    httr::add_headers(Authorization = paste("apiKey", api.key))
  )

  if(is.raw(result$content))
    result$content <- result$content %>% rawToChar
  if(jsonlite::validate(result$content))
    result$content <- result$content %>% jsonlite::fromJSON
  message("* Request status:", result$status_code)

  # Output ====
  if(output.mode == "full" || result$status_code != "200")
    return(result)
  else
    return(result$content)
}

#' Correct arguments length.
#'
#' @param ... list. A list of arguments to check for length.
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
#'
#' @noRd
#' @importFrom ArgumentCheck newArgCheck addError finishArgCheck
checkLength <- function(..., check, env = .GlobalEnv){
  args <- as.list(...)

  sapply(args, function(arg, .env = env){
    arg.value <- get(arg, envir = .env)
    if(length(arg.value) > 1){
      assign(arg, arg.value, envir = .env)
      ArgumentCheck::addWarning(
        msg= paste0("`",arg,"` argument had length>1: only the first element is used."),
        argcheck = check
      )
    }
  })

  return(check)
}

#' These checks are found in every function of {cedarr}.
#'
#' @noRd
#' @importFrom ArgumentCheck newArgCheck addError finishArgCheck
constantCheck <- function(..., check, env = .GlobalEnv){
  args <- as.list(...)

  sapply(args, function(arg, .env = env){
    arg.value <- get(arg, envir = .env)

    # Check type
    if(switch(arg,
      api.key = !is.character(arg.value),
      output.mode = !is.character(arg.value),
      page.index = !is.numeric(arg.value),
      page.size = !is.numeric(arg.value),
      FALSE
    ))
      ArgumentCheck::addError(
        msg = paste("Invalid type for", arg),
        argcheck = check
      )

    # Check value
    if(switch(arg,
      output.mode = !arg.value %in% c("full", "content"),
      page.index = as.integer(arg.value) == 0,
      page.size = as.integer(arg.value) == 0,
      FALSE
    ))
      ArgumentCheck::addError(
        msg = paste("Invalid value for", arg),
        argcheck = check
      )

  })

  return(check)
}

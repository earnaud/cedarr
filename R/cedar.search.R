#' @include cedar.client-class.R
NULL

# G: search =====

#' Search in CEDAR
#'
#' S4 Methods to query terms, classes or retrieve full ontologies from the CEDAR terminology
#' metadata center. These mthods relies on simple HTTP requests wrapped into these convenience
#' methods.
#'
#' @usage
#' ## Generic method
#' cedar.search()
#'
#' @param cedar.client a cedar.client for BioPortal. See cedar.client.
#' @param query character. Input query as a text.
#' @param scope character. Which search scopes shall be investigated. Accepted values are 1-length vector:
#' "all" (default), "classes", "value_sets", "values".
#' @param output.mode character. "full" will return the whole response object (from {httr}) or "content" will
#' fetch the interest values from the response object. Getting the whole object might be interesting to
#' have a look at system metadata, or in case of error to debug the connection. (defaults to "content")
#' @param ontologies character. IDs of ontologies to query, aka sources. (e.g. "ENVO").
#' @param vs_collection character. Collection name from which the target value set originates.
#' Accepted values are 1-length vector: "CEDARVS", "CADSR-VS", "NLMVS".
#' @param vs_id character. Target value set ID. (default to NA_character_).
#' @param suggest logical. Will perform a search specifically geared towards type-ahead suggestions
#' (defaults to FALSE).
#TODO @param subtree_root_id character. Class identifier that limits the search to the branch rooted on that class.
# It must be URL encoded.
# @param source character. Ontology for which the subtree search will be performed.
# @param maxDepth integer. Subtree depth.
#' @param page integer. Index of the page to be returned (defaults to 1st page).
#' @param pageSize integer. Number of results per page, capped at 50. (defaults to 50).
#'
#' @return
#'
#' A response object (see \code{\link[httr]{response}}). Its content varies accordingly to
#' the provided argument, thus referring to different methods. Also, if *output.mode* is
#' set to "full", then the most interesting part is located at response/content; if
#' *output.mode* is set to "complete", this interesting part is directly returned.
#'
#' In any case, here is a quick reminder about http status codes often met:
#' \itemize{
#'   \item{200: successful operation}
#'   \item{400: bad request}
#'   \item{401: unauthorized. In most of the cases, this occur after a typo in the API key.}
#'   \item{404: not found. Your request targets a non-existent resource. This is different
#'   from getting an empty result.}
#'   \item{500: internal server error.}
#' }
#'
#' @examples
#' # Set up a CEDAR client providing you API key
#' # (execute this code line by line)
#' my.api.key <- readline()
#' cc <- cedar.client(api.key = my.api.key)
#'
#' @seealso [cedar.client()]
setGeneric(
  "cedar.search",
  function(
    cedar.client,
    query,
    scope = "all",
    output.mode = "content",
    ontologies = NA_character_,
    vs_collection = NA_character_,
    vs_id = NA_character_,
    suggest = FALSE,
    page = 1,
    page.size = 50
  )
    standardGeneric("cedar.search")
)

# M: values =====

#' @describeIn cedar.search
#'
#' Search for ontology classes, value sets, and values.
#'
#' @return
#'
#' *If cedar.client and query are provided:*
#'
#' Response contains database pages information (page number, page
#' size, page count, links to next and previous pages) and a *collection* list containing
#' the queried information in a data.frame.
#'
#' @examples
#' ## 1 - search a term in an ontology
#' res <- cedar.search(
#'   cedar.client = cc,
#'   query = "community",
#'   ontologies = "ENVO"
#' )
#'
#' # Access the result of the query
#' View(res$collection)
#'
#' @importFrom httr GET add_headers
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom ArgumentCheck newArgCheck addError addWarning finishArgCheck
setMethod(
  "cedar.search",
  signature = c(
    cedar.client = "cedar.client",
    query = "character"
  ),
  function (
    cedar.client,
    query,
    scope = "all",
    output.mode = "content",
    ontologies = NA_character_,
    suggest = FALSE,
    page = 1,
    page.size = 50
  ) {
    # Missing ====
    if(missing(cedar.client))
      stop("No API client provided: see ?cedar.client.")
    if(missing(query))
      stop("No query provided.")

    # Invalid ====
    check <- newArgCheck()

    if(isFALSE(is(cedar.client, "cedar.client") &&
      !is.na(cedar.client@api.key)))
      addError(
        msg = "Invalid API key: check your CEDAR account.",
        check = check
      )
    if(isFALSE(is(query, "character") &&
        query != ""))
      addError(
        msg = "Invalid query: must be at least one word length.",
        check = check
      )
    if(isFALSE(is(scope, "character") &&
        length(scope) > 0 &&
        scope %in% c("all", "classes", "value_sets", "values")))
      addError(
        msg = "Invalid value for `scope`.",
        check = check
      )
    if(isFALSE(is(output.mode, "character") &&
        length(output.mode) > 0 &&
        output.mode %in% c("full", "content")))
      addError(
        msg = "Invalid value for `output.mode`.",
        check = check
      )
    if(isFALSE(is(ontologies, "character")))
      addError(
        msg = "Invalid type for `ontologies`.",
        check = check
      )
    if(isFALSE(is(suggest, "logical") &&
        (isTRUE(suggest) || isFALSE(suggest))))
      addError(
        msg = "Invalid value for `suggest`.",
        check = check
      )
    if(isFALSE(is(page, "numeric")  &&
        page > 0))
      addError(
        msg = "Invalid value for `page`.",
        check = check
      )
    if(isFALSE(is(page.size, "numeric") &&
        page.size > 0))
      addError(
        msg = "Invalid value for `page.size`.",
        check = check
      )

    # Correction ====
    if(length(scope) > 1){
        scope <- scope[1]
        addWarning(
          msg = "`scope` argument had length > 1: only the first element is used.",
          check = check
        )
    }
    if(length(output.mode) > 1){
        output.mode <- output.mode[1]
        addWarning(
          msg = "`output.mode` argument had length > 1: only the first element is used.",
          check = check
        )
    }
    if(is.na(ontologies))
        ontologies <- NULL
    if(length(suggest) > 1){
        suggest <- suggest[1]
        addWarning(
          msg = "`suggest` argument had length > 1: only the first element is used.",
          check = check
        )
    }
    if(length(page) > 1){
        page <- page[1]
        addWarning(
          msg = "`page` argument had length > 1: only the first element is used.",
          check = check
        )
    }
    if(length(page.size) > 1){
        page.size <- page.size[1]
        addWarning(
          msg = "`page.size` argument had length > 1: only the first element is used.",
          check = check
        )
    }

    finishArgCheck(check)

    # Request ====
    result <- GET(
      "https://terminology.metadatacenter.org/bioportal/search?",
      query = list(
        q = query,
        scope = scope,
        sources = ontologies,
        page = page,
        page_size = page.size
      ),
      add_headers(Authorization = paste("apiKey ", cedar.client@api.key))
    )

    result$content <- if(is.raw(result$content))
      rawToChar(result$content)
    else
      result$content
    .res <- try(fromJSON(result$content), silent = TRUE)
    if(class(.res) != "try-error")
      result$content <- .res

    # Output ====
    if(result$status_code == 200){
      if(identical(result$content$collection, list()))
        warning("No result match your query in the selected ontologies.")
      if(output.mode == "full")
        return(result)
      else
        return(result$content)
    }
    else{
      message("Error ", result$status_code, ": ",
        gsub("^.*<p>(.*)</p>.*$", "\\1", result$content)
        %>% gsub("\n|</?pre>", "", .)
      )
      return(result)
    }
  }
)

# M: values_sets collections ======

#' @describeIn cedar.search
#'
#' Find all value set collections.
#'
#' @return
#'
#' *If only cedar.client is provided:*
#'
#' Response contains a data.frame referencing the value set collections of CEDAR
#' terminology metadatacenter.
#'
#' @examples
#' ## 2 - fetch values sets collections
#' res <- cedar.search(
#'   cedar.client = cc
#' )
#'
#' # Access the result of the query
#' View(res)
#'
#' @importFrom httr GET add_headers
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
setMethod(
  "cedar.search",
  signature = c(
    cedar.client = "cedar.client"
  ),
  function (
    cedar.client,
    output.mode = "content"
  ) {
    # Missing ====
    if(missing(cedar.client))
      stop("No API client provided: see ?cedar.client.")

    # Invalid ====
    check <- newArgCheck()

    if(is.na(cedar.client@api.key))
      addError(
        msg = "Invalid BioOntology API key. You can verifiy it in your account settings.",
        check = check
      )

    finishArgCheck(check)

    # Request ====
    result <- GET(
      "https://terminology.metadatacenter.org/bioportal/vs-collections",
      add_headers(Authorization = paste("apiKey", cedar.client@api.key))
    )

    result$content <- if(is.raw(result$content))
      rawToChar(result$content)
    else
      result$content
    .res <- try(fromJSON(result$content), silent = TRUE)
    if(class(.res) != "try-error")
      result$content <- .res

    # Output ====
    if(result$status_code == 200){
      if(identical(result$content$collection, list()))
        warning("No result match your query in the selected ontologies.")
      if(output.mode == "full")
        return(result)
      else
        return(result$content)
    }
    else{
      message("Error ", result$status_code, ": ",
        gsub("^.*<p>(.*)</p>.*$", "\\1", result$content)
        %>% gsub("\n|</?pre>", "", .)
      )
      return(result)
    }
  }
)

# M: values_sets ======

#' @describeIn cedar.search
#'
#' Get all value sets in a value set collection.
#'
#' @return
#'
#' *If cedar.client and vs_collection are provided:*
#'
#' A data.frame containing references to access all the value sets for this collection.
#'
#' *If cedar.client, vs_collection and vs_id are provided:*
#'
#' A list matching the entry for this value set ID in the value sets collection.
#'
#' @examples
#' ## 3 - fetch values sets from a value sets collection
#' res <- cedar.search(
#'   cedar.client = cc,
#'   vs_collection = "CEDARVS"
#' )
#'
#' # Access the result of the query
#' View(res$collection)
#'
#' ## 4 - fetch values sets from a target value set
#' res <- cedar.search(
#'   cedar.client = cc,
#'   vs_collection = "CEDARVS",
#'   vs_id = "803d8340-6a1e-0138-64d8-005056010073"
#' )
#'
#' # Access the result of the query
#' # BEWARE: the result is a list
#' View(res)
#'
#' @importFrom httr GET add_headers
#' @importFrom dplyr %>%
#' @importFrom jsonlite fromJSON
setMethod(
  "cedar.search",
  signature = c(
    cedar.client = "cedar.client",
    query = "ANY",
    scope = "ANY",
    output.mode = "ANY",
    ontologies = "ANY",
    vs_collection = "character"
  ),
  function (
    cedar.client,
    output.mode = "content",
    vs_collection = "CEDARVS",
    vs_id = NA_character_,
    page = 1,
    page.size = 50
  ) {
    # Missing ====
    if(missing(cedar.client))
      stop("No API client provided: see ?cedar.client.")
    if(missing(vs_collection))
      stop("No values sets collection provided: try `cedar.search(<your.cedar.client>)`
        to fetch their list.")

    # Invalid ====
    check <- newArgCheck()

    if(is.na(cedar.client@api.key))
      addError(
        msg = "Invalid BioOntology API key. You can verifiy it in your account settings.",
        check = check
      )
    .vsc.list <- try(cedar.search(cedar.client = cedar.client))
    if(is.na(vs_collection) ||
        !is(vs_collection, "character") ||
        !vs_collection %in% unlist(.vsc.list["id"])){
      if(class(.vsc.list) != "try-error")
        addError(
          msg = paste0(
            "Invalid value for `vs_collection`, must be one of: ",
            paste(.vsc.list$id, collapse = ", ")
          ),
          check = check
        )
      else
        addError(
          msg = "Invalid value for `vs_collection`: try `cedar.search(<your.cedar.client>)`.",
          check = check
        )
    }
    if(isFALSE(is(vs_id, "character") ||
        is.na(vs_id)))
      addError(
        msg = "Invalid value for `vs_id`: try `cedar.search(<your.cedar.client>, vs_collection)`.",
        check = check
      )
    if(isFALSE(is(page, "numeric")  &&
        page > 0))
      addError(
        msg = "Invalid value for `page`.",
        check = check
      )
    if(isFALSE(is(page.size, "numeric") &&
        page.size > 0))
      addError(
        msg = "Invalid value for `page.size`.",
        check = check
      )

    # Correct ====
    if(is.na(vs_id))
      vs_id <- NULL
    if(length(page) > 1){
      page <- page[1]
      addWarning(
        msg = "`page` argument had length > 1: only the first element is used.",
        check = check
      )
    }
    if(length(page.size) > 1){
      page.size <- page.size[1]
      addWarning(
        msg = "`page.size` argument had length > 1: only the first element is used.",
        check = check
      )
    }

    finishArgCheck(check)

    # Request ====
    baseURL <- paste0(
      "https://terminology.metadatacenter.org/bioportal/vs-collections/",
      vs_collection,
      "/value-sets/"
    )
    if(!is.null(vs_id))
      baseURL <- paste0(baseURL, vs_id)
    result <- GET(
      baseURL,
      query = list(
        page = page,
        page_size = page.size
      ),
      add_headers(Authorization = paste("apiKey", cedar.client@api.key))
    )

    result$content <- if(is.raw(result$content))
      rawToChar(result$content)
    else
      result$content
    .res <- try(fromJSON(result$content), silent = TRUE)
    if(class(.res) != "try-error")
      result$content <- .res

    # Output ====
    if(result$status_code == 200){
      if(identical(result$content$collection, list()))
        warning("No result match your query in the selected ontologies.")
      if(output.mode == "full")
        return(result)
      else
        return(result$content)
    }
    else{
      message("Error ", result$status_code, ": ",
        gsub("^.*<p>(.*)</p>.*$", "\\1", result$content)
        %>% gsub("\n|</?pre>", "", .)
      )
      return(result)
    }
  }
)

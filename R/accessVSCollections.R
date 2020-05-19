accessValueSets <- function(
  api.key,
  vs.collection = NA_character_,
  id = NA_character_,
  sub = NA_character_,
  output.mode = "content",
  page = 1,
  page.size= 50
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(vs.collection))
    stop("No VS collection ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(vs.collection) || is.na(vs.collection))
    addError(
      msg = "Invalid VS collection name: must be a length-one character.",
      argcheck = check
    )
  if(!is.character(id) &&
      !is.na(id))
    addError(
      msg = "Invalid value for `id`: must be either NA or a character.",
      argcheck = check
    )
  else if(isFALSE(is.character(sub) || is.na(sub)) &&
      !sub %in% c(NA, NA_character_, "tree", "values"))
    addError(
      msg = "Invalid value for `sub`: must be either NA or a character.",
      argcheck = check
    )
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )
  if(!is.numeric(page) || page == 0)
    addError(
      msg = "Invalid value for `page`.",
      argcheck = check
    )
  if(!is.numeric(page.size) || page.size == 0)
    addError(
      msg = "Invalid value for `page.size`.",
      argcheck = check
    )

  # Correction ====
  if(is.na(id)){
    id <- NULL
    sub <- NULL
  }
  else {
    if(is.na(sub))
      id <- paste0("/", id)
    else if(sub == "tree")
      id <- paste0("/", id, "/tree")
    else if(sub == "values")
      id <- paste0("/", id, "/values")
  }
  sapply(c("vs.collection", "id", "sub", "output.mode", "page","page.size"), function(arg){
    if(length(get(arg)) > 1){
      assign(arg, get(arg)[1])
      addWarning(
        msg = "`",arg,"` argument had length > 1: only the first element is used.",
        argcheck = check
      )
    }
  })

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    paste0(
      "https://terminology.metadatacenter.org/bioportal/vs-collections/",
      vs.collection,
      "/value-sets",
      id
    ),
    query = ifelse(
      is.null(id) || (!is.null(id) && sub == "values"),
      list(
        page = page,
        page_size = page.size
      ),
      NULL
    ),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}

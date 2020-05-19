accessProperty <- function(
  api.key,
  ontology,
  id = NA_character_, # NA "roots" or ID
  sub = NA_character_, #
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(ontology))
    stop("No ontology ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(ontology) || is.na(ontology))
    addError(
      msg = "Invalid type for `ontology`. (string starting by \"ontologies\" are a reserved
      term)",
      argcheck = check
    )
  if(!is.character(id) && !is.na(id))
    addError(
      msg = "Invalid value for `sub`.",
      argcheck = check
    )
  else if(is.character(id) &&
      id != "roots" &&
      !sub %in% c(NA, NA_character_, "tree", "children", "descendants", "parents")) {
        addError(
          msg = "Invalid value for `sub`.",
          argcheck = check
        )
  }
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )

  # Correction ====
  sapply(c("api.key", "ontology", "output.mode", "id", "sub"), function(arg){
    if(length(get(arg)) > 1){
      assign(arg, get(arg)[1])
      addWarning(
        msg = "`",arg,"` argument had length > 1: only the first element is used.",
        argcheck = check
      )
    }
  })
  if(is.na(id))
    id <- NULL
  else if(id == "roots")
    id <- "/roots"
  else {
    if(is.na(sub))
      sub <- NULL
    id <- paste0(c("", id, sub), collapse = "/")
  }

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    paste0(
      "https://terminology.metadatacenter.org/bioportal/ontologies/",
      ontology,
      "/properties",
      id
    ),
    query = ifelse(is.null(ontology),
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



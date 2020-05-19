accessRelation <- function(
  api.key,
  id = NA_character_,
  output.mode = "content"
){
  # Missing ====
  if(missing(api.key))
    stop("No API client provided: see https://cedar.metadatacenter.org/profile.")
  if(missing(id))
    stop("No relation ID provided.")

  # Invalid ====
  check <- newArgCheck()

  if(!is.character(api.key))
    addError(
      msg = "Invalid API key: must be a length-one character.
      See https://cedar.metadatacenter.org/profile.",
      argcheck = check
    )
  if(!is.character(id) || is.na(id))
    addError(
      msg = "Invalid value for `id`: must be either a length-one character.",
      argcheck = check
    )
  if(!is.character(output.mode) ||
      length(output.mode) == 0 ||
      !output.mode %in% c("full", "content"))
    addError(
      msg = "Invalid value for `output.mode`. Must be one of 'full' or 'content'.",
      argcheck = check
    )

  # Correction ====
  if(length(id) > 1){
    assign(id, id[1])
    addWarning(
      msg = "`id` argument had length > 1: only the first element is used.",
      argcheck = check
    )
  }

  finishArgCheck(check)

  # Request ====
  result <- cedar.get(
    api.key,
    paste0("https://terminology.metadatacenter.org/bioportal/relations/", id),
    output.mode = output.mode
  )

  # Output ====
  return(result)
}

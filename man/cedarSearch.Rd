% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cedarSearch.R
\name{cedarSearch}
\alias{cedarSearch}
\title{Search values in CEDAR}
\usage{
cedarSearch(
  api.key,
  query,
  sources = NA_character_,
  scope = "all",
  suggest = FALSE,
  output.mode = "content",
  subtree.root.id = NA_character_,
  subtree.source = NA_character_,
  maxDepth = 1,
  page.index = 1,
  page.size = 50
)
}
\arguments{
\item{api.key}{character. An API Key is required to access any
API call. It is used within {cedarr} as a header for http
requests. An API key is linked to a CEDAR account
(https://cedar.metadatacenter.org/profile)}

\item{query}{character. Input query as a text.}

\item{sources}{character. Either value sets collection names
or ontologies names in which to get the results of the query.}

\item{scope}{character. Which search scopes shall be
investigated. Accepted values are 1-length vector: "all"
(default), "classes", "value_sets", "values".}

\item{suggest}{logical. Will perform a search specifically
geared towards type-ahead suggestions (defaults to FALSE).}

\item{output.mode}{character. "full" will return the whole
response object (from {httr}) or "content" will fetch the
interest values from the response object. Getting the whole
object might be interesting to have a look at system metadata,
or in case of error to debug the connection. (defaults to
"content")}

\item{subtree.root.id}{character. URL for the class identifier
that limits the search to the branch rooted on that class.}

\item{subtree.source}{character. URL for the ontology for which the
subtree search will be performed. Not evaluated if
\code{subtree.root.id} is not provided.}

\item{maxDepth}{integer. Subtree depth.Not evaluated if
\code{subtree.root.id} is not provided.}

\item{page.index}{integer. Index of the page to be returned
(defaults to 1st page).}

\item{page.size}{integer. Number of results per page, capped
at 50. (defaults to 50).}
}
\value{
A list or data.frame with detailed information on the queried information and
how to access it.

If \code{output.mode = "full"}, the whole http response object (see httr::response).
It is structured as a list with response metadata wrapping the \code{content} item
which contains the wanted result.

If \code{output.mode = "content"}, the \code{content} item is directly returned, containing
database metadata and the interesting information in the \code{collection} subitem.
}
\description{
Search for terms, classes or retrieve full ontologies
from the CEDAR terminology metadata center.
(https://terminology.metadatacenter.org/api/#/).
}
\details{
This function matches the following query from the Swagger UI
(https://terminology.metadatacenter.org/api/#/):

\itemize{
\item{\verb{/search}, for both classes and values}
}
}
\examples{
\dontrun{
my.api.key <- readline()

result <- cedarr::search(
  my.api.key,
  "habitat",
  "ENVO"
)

View(result)
}

}

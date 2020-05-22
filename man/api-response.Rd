\name{API response}
\alias{API response}

\title{CEDAR API response}

The {cedarr} package uses the {httr} `GET()` function to retrieve information
from the CEDAR API. This API returns a result wrapped within the `content` part
of the http response. This part is structured as follow:

\describe {
  \label {page:}{which page is returned to you. The default is the first page.}
  \label {pageCount:}{how many pages containing results maching your criterion
  have been found in CEDAR.}
  \label {pageSize:}{how many results are present onto the current page (capped
  at 50).}
  \label {totalCount:}{how many results maching your criterion have been found
  in CEDAR.}
  \label {prevPage:}{number of the previous page. You can browse pages with calls
  to function with the `page.index` argument.}
  \label {nextPage:}{number of the next page. You can browse pages with calls
  to function with the `page.index` argument.}
  \label {collection:}{this is the core result (which might be organised in a
  data.frame table or in a list):
    \describe {
      \label {id:}{a litteral identifier given to the result.}
      \label {@id:}{a url-format identifier given to the result. This is the one
      to use if you want to call this result as an argument of functions (e.g.
      within the `id` argument.)}
      \label {@type:}{a url-format identifier of `@type`.}
      \label {type:}{which is the type of the result (class, value, value-set,
      vs-collection, ontology.)}
      \label {prefLabel:}{a human-legible title given to the result.}
      \label {notation:}{a short way to note the result.}
      \label {definition:}{the definition of the result within its ontology.}
      \label {source:}{an url of the original resource in which the result can
      be found.}
    }
  }
}

CEDAR uses two approaches of the terminology resources: the value sets collections
and the ontologies. The first one is structured as value sets collections, parents
of value sets, parents of values. The second one is structured as ontologies,
which are hierarchies of classes. The "lowest" ontological classes are equivalent
to the values.

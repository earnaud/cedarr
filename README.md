# cedarr
CEDAR R package for API linking in an R interface.

## Installation

You can install this package with the following command:

```
devtools::install_github("earnaud/cedarr", dependencies = TRUE)
```

## Usage.

The features developped in this package are convenience functions for any R user
to query the [CEDAR API](https://terminology.metadatacenter.org/api/).

All functions address different part of the API. However, one can use `cedarGet()` 
as a bottleneck function since this one is called from any other. It gathers 
all arguments possible to build any query. See `?cedarr::cedarGet`.

## Additional details

You might access to a detail of the way http responses are structured with:

```
help("API response", package = "cedarr")
# ?cedarr::`API response`
```

## Intellectual Properties Owner

{cedarr} is accessible under [CC-BY 2.0](https://creativecommons.org/licenses/by/2.0/fr/deed.en).
It relies on CEDAR and NCBO Bioportal services.

**Bioportal**

Whetzel PL, Noy NF, Shah NH, Alexander PR, Nyulas C, Tudorache T, Musen MA. BioPortal: enhanced functionality via new Web services from the National Center for Biomedical Ontology to access and use ontologies in software applications. Nucleic Acids Res. 2011 Jul;39(Web Server issue):W541-5. Epub 2011 Jun 14.

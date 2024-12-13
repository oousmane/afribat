#'  Bat records as simple features.
#'
#'
#'  Occurrence records of 266 bat species from 53 genera and 12 families
#'  (order Chiroptera, class Mammalia) from sub-Saharan Africa.
#'
#' @format A tibble with 17255 rows and 12 variables:
#' \describe{
#'   \item{family}{a character denoting family of bat record }
#'   \item{genus}{a character denoting genus of bat record}
#'   \item{species}{a character denoting species of bat record}
#'   \item{museum}{ a character denoting museum accession number of bat record}
#'   \item{date}{a date denoting date on which the bat was collected or otherwise recorded}
#'   \item{year}{an integer denoting body mass (grams)}
#'   \item{country}{a character denoting country in which the bat was recorded}
#'   \item{location}{a character denoting name of the location or locality that the bat was recorded}
#'   \item{reference}{a character denoting source of the record, mostly papers}
#'   \item{holotype}{a character denoting whether the record is a type specimen}
#'   \item{checked}{a character denoting whether the bat specimen was examined by the authors}
#'   \item{geometry}{ the geometry list column, crs is EPSG:4326}

#' }
#' @source {Originally published in: Monadjem, A., Montauban, C., Webala, P.W. et al. African bat database: curated data of occurrences, distributions and conservation metrics for sub-Saharan bats. Sci Data 11, 1309 (2024). https://doi.org/10.1038/s41597-024-04170-71}
"afribats_sf"

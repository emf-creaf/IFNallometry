#' Example datasets
#'
#' Data sets given to illustrate input data structures.
#'
#' @format
#' Tree data corresponding to three forest plots. Data frame with 40 rows (tree records) and columns:
#' \itemize{
#'   \item{\code{ID}: Forest plot code, defined as the combination of province code and "estadillo".}
#'   \item{\code{Species}: Tree species code.}
#'   \item{\code{N}: Tree density (individuals per hectare) that the record represents.}
#'   \item{\code{DBH}: Tree diameter at breast height (cm).}
#'   \item{\code{H}: Tree height (m)}
#' }
#'
#' @name example_tree_data
#' @aliases example_tree_data
#' @docType data
#' @author Miquel De Cáceres
#' @keywords data
#' @references Project reference.
NULL


#' Spanish provinces
#'
#' Data set of Spanish provinces.
#'
#' @format A data frame with province names and their inclusion in autonomous communities
#' \itemize{
#'   \item{\code{Provincia}: Province name.}
#'   \item{\code{CCAA}: Autonomous community where province belongs.}
#' }
#'
#' @name provincias
#' @aliases provincias
#' @docType data
#' @author Miquel De Cáceres
#' @keywords data
#' @references Project reference.
NULL

#' IFN species
#'
#' Data set of recognized species names
#'
#' @format A data frame with species names and their code in IFN
#' \itemize{
#'   \item{\code{IFNcode}: Numeric code used in IFN.}
#'   \item{\code{Species}: Species name.}
#' }
#'
#' @name species_ifn
#' @aliases species_ifn
#' @docType data
#' @author Miquel De Cáceres
#' @keywords data
#' @references Project reference.
NULL

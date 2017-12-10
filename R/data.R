#' Cartogram coordinates and 2013 census data
#'
#' Data subsetted from Peter Ellis's nzelect package
#' and uses his cartogram view of NZ based on population
#'
#' @format A data frame with 6812 rows with the following variables:
#' \itemize{
#'   \item long: Longitude.
#'   \item lat: Latitude
#'   \item order, hole, piece, id, group: components to draw polygons
#'   \item Name: Name of the region
#'   \item REGC: id of region
#'   \item PropUnemploymentBenefit2013: Proportion of population on unemployment benefit
#'   \item Prop65AndOlder_2013: Proportion older than 65
#'   \item PropDoctorate2013: Proportion with doctorate
#'   \item MedianIncome2013: Median income
#'   }
"nz_cart_census"

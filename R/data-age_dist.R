#' Age frequency of San Francisco Estuary-based White Sturgeon.
#'
#' @description A dataset with 20 ages (0-19) and corresponding frequency (see
#'   Details) of White Sturgeon (\emph{Acipenser transmontanus}).
#'
#' @format A data frame with 20 rows and 2 variables:
#' \describe{
#'   \item{age}{numeric age from 0-19}
#'   \item{freq}{number (frequency) at each age}
#' }
#'
#' @details Frequency for ages 3-19 proportional to starting abundance of
#'    48,000. Age-1 & age-2 frequency predicted from linear regression.
#'    Age-0 frequency estimated using age-specific fecundity and percent of
#'    females spawning annually (in this case 15\%).
#'
#' @source Model development input data (CDFW; Univ. of Idaho).
#' @name age_dist
NULL

#' Number of eggs by age for San Francisco Estuary-based White Sturgeon.
#'
#' @description A dataset for White Sturgeon (\emph{Acipenser transmontanus})
#'    with 20 ages (0-19) and corresponding egg count with associated error.
#'
#' @format A data frame with 20 rows and 3 variables: \describe{
#'    \item{age}{numeric age from 0-19}
#'    \item{count}{number of eggs at each age}
#'    \item{se}{error associated with count}
#' }
#'
#' @details Based on linear regression using data from DeVore et al. 1995.
#'    DeVore equation \eqn{0.072 * len^{2.94}} to get number of eggs.
#'
#' @references DeVore, J. D., B. W. James, C. A. Tracy, and D. A. Hale.  1995.
#'    Dynamics and potential production of White Sturgeon in the Columbia River
#'    downstream from Bonneville Dam.
#'    Transactions of the American Fisheries Society 124:845–856.
#'
#' @source Model development input data (CDFW; Univ. of Idaho).
#' @name number_eggs
NULL

# ******************************************************************************
# Created: 14-Aug-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: file contains functions and (or) methods for simulating model data
#          using some functions from the popbio package
# Source:  S. Blackburn (Univ of Idaho) code & Master's thesis
#          (corrected_transient_midfecund_current.R)
# ******************************************************************************

# use popbio::betaval() for this but need to vectorize for ages (e.g., 0-19)
#' @keywords internal
#' @rdname spopmodel-internals
bval <- Vectorize(
  FUN = popbio::betaval,
  vectorize.args = c("mn", "sdev")
)

#' @keywords internal
#' @rdname spopmodel-internals
sbval <- Vectorize(
  FUN = popbio::stretchbetaval,
  vectorize.args = c("mn", "std", "minb", "maxb")
)

# pop projections ---------------------------------------------------------

#' @keywords internal
#' @rdname spopmodel-internals
pp <- popbio::pop.projection

# vectorized simulation functions -----------------------------------------

#' @keywords internal
#' @rdname spopmodel-internals
BetavalSims <- function(mn, sdev, iterations = 10) {

  # slow when n is large, may want to try using vapply
  replicate(n = iterations, expr = bval(mn = mn, sdev = sdev))
}
# end BetavalSims

#' @keywords internal
#' @rdname spopmodel-internals
SBetavalSims <- function(mn, sdev, minb = 0, maxb = NULL, iterations = 10) {

  if (is.null(maxb)) {
    # m <- deparse(substitute(mn))
    maxb <- 3 * mn
    warning("`maxb` set at 3 * data[[prob]]", call. = FALSE)
  }

  replicate(
    n = iterations,
    expr = sbval(
      mn = mn,
      std = sdev,
      minb = minb,
      maxb = maxb,
      fx = runif(n = 1)
    )
  )
}
# end SBetavalSims

#' @keywords internal
#' @rdname spopmodel-internals
SampleSims <- function(sims, n, span = 5, seed = NULL) {

  # used to change first row of simulated probabilities of survival

  if (is.list(sims)) sims <- unlist(sims, use.names = FALSE)

  if (!is.numeric(sims))
    stop("`sims` must be numeric.", call. = FALSE)

  if (!is.numeric(n))
    stop("`n` must be a positive integer.", call. = FALSE)

  tms <- n * (span - 1)
  zeroes <- rep(0, times = tms)

  set.seed(seed = seed)
  sample(c(zeroes, sims), size = n, replace = TRUE)
}
# end SampleSims

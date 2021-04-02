# ******************************************************************************
# Created: 16-Aug-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: file contains functions and (or) methods for simulating model data
#          using functions in model_sims.R; functions (or methods) herein
#          to be called directly by user.
# Source:  S. Blackburn (Univ of Idaho) code & Master's thesis
#          (corrected_transient_midfecund_current.R)
# ******************************************************************************

#' Generate simulations for stochastic modeling of population trajectory.
#'
#' @description Simulate environmental stochasticity for surivial, spawning, and
#'   number of eggs using age-specific empirical data. Simulated data are used
#'   by \code{PopProjections} to calculate population trajections (lambda).
#'
#' @param data Dataframe containing age-specific probabilities for survival,
#'   spawning, or number of eggs along with standard deviations (errors)
#' @param prob Column name in data containing age-specific probabilities or
#'   count.
#' @param std Column name in data containing error of probabilities.
#' @param recruitment Used only for "survival" option. Value (default = 5)
#'   denotes period of successful recruitment.
#' @param minb Used only for "numeggs" option. Value passed on to
#'   \code{\link{stretchbetaval}}; default = 0.
#' @param maxb Used only for "numeggs" option. Value passed on to
#'   \code{\link{stretchbetaval}}; default = NULL; if NULL value will be 3 times
#'   data[[prob]]
#' @param iters Scalar integer. Number of iterations to perform. Default 10;
#'   function will slow with higher (i.e., > 5000) value.
#' @param seed Numeric value passed to \code{set.seed} to ensure repeatability.
#' @param type Scalar character as one of three options: "survival"; "spawning";
#'   or "numeggs". Choose one based on desired simulation. Default = "survival".
#'
#' @return A matrix of model simulations (varies depending on argument
#'    supplied to \code{type}). Matrix is of class \code{simulations}.
#' @export
#'
#' @examples
#' # code here
Simulations <- function(
  data, prob, std, recruitment = 5, minb = 0, maxb = NULL, iters = 10,
  seed = NULL, type = c("survival", "spawning", "numeggs")) {

  # TODO: perhaps add input validation
  # TODO: add R documentation
  # TODO: perhaps add output validation (not all NA)
  # TODO: add warning for survival first row replace assumed age0
  # TODO: looking into adding set.seed in BetavalSims & SBetavalSims
  # TODO: add print method for this function
  # TODO: add necessary comments below

  type <- match.arg(type)

  p <- as.character(substitute(prob))
  e <- as.character(substitute(std))

  sims <- matrix(nrow = nrow(data), ncol = iters)

  switch (
    type,

    # survival simulations *********************************
    "survival" = {
      sims <- BetavalSims(
        mn = data[[p]],
        sdev = data[[e]],
        iterations = iters
      )

      # resetting seed for looping when needed
      # set.seed(seed = NULL)

      V <- SampleSims(
        sims = sims[1, ],
        n = iters,
        span = recruitment,
        seed = seed
      )

      sims[1, ] <- V
    },

    # spawning simulations *********************************
    "spawning" = {
      sims <- BetavalSims(
        mn = data[[p]],
        sdev = data[[e]],
        iterations = iters
      )
    },

    # number of eggs simulations ***************************
    "numeggs"  = {
      sims <- SBetavalSims(
        mn = data[[p]],
        sdev = data[[e]],
        minb = minb,
        maxb = maxb,
        iterations = iters
      )
    }
  ) # end switch

  attr(sims, which = "iterations") <- iters

  class(sims) <- "simulations"

  sims
}
# end Simulations

#' Get population projection over defined period (years).
#'
#' @description \code{PopProjections} is essentially a wrapper for
#'   \code{\link{pop.projection}}. It will run \code{pop.projection} for every
#'   simulation (i.e., column) in the simulation matrices (passed to
#'   \code{fSims} & \code{sSims}). Function creates the Leslie Matrix (*A*)
#'   required by \code{pop.projection}.
#'
#' @param fSims A matrix of fecundity simulations, likely the product of
#'   simulated number of eggs x simulated spawning probabilities x sex
#'   ratio (expressed as a proportion, e.g., 0.5).
#' @param sSims A matrix of survival simulations, likely one matrix for each
#'   level of exploitation (mu) being tested.
#' @param mn Scalar numeric value as survival probability of oldest fish (i.e.,
#'   max age in \code{ageFreq})
#' @param sdev Scalar numeric value as corresponding error for \code{mn}.
#' @param ageFreq Numeric vector of age frequency passed on to \code{n} in
#'   \code{\link{pop.projection}}.
#' @param period Scalar integer given number of years to project. Passed on to
#'   \code{iterations} in \code{\link{pop.projection}}.
#'
#' @return A large matrix with rows as 'Value' output in
#'   \code{link{pop.projection}} and number of columns = number of simulations.
#' @export
#'
#' @examples
#' # code here
PopProjections <- function(fSims, sSims, mn, sdev, ageFreq, period) {

  # TODO: may be slow with increased number of sims; tried mapply, apply, &
  # outer for LeslieMatrix but to no avail; for now this will work but may need
  # to improve speed in future versions (20-Aug-2018)

  if (!is.matrix(fSims) && !is.matrix(sSims))
    stop("Sims must be matrices with equal dims.", call. = FALSE)

  if (!identical(dim(fSims), dim(sSims)))
    stop("Sim dimensions must be equal.", call. = FALSE)

  # likely will be a large matrix
  sims <- fSims * sSims

  lmat <- lapply(seq_len(ncol(sims)), FUN = function(i) {
    LeslieMatrix(sims = sims[, i], survSims = sSims[, i], mn = mn, sdev = sdev)
  })

  # for use in vapply below
  v <- list(
    lambda = 0,
    stable.stage = 0,
    stage.vector = 0,
    pop.sizes = 0,
    pop.changes = 0
  )

  res <- vapply(lmat, FUN = pp, FUN.VALUE = v, n = ageFreq, iterations = period)

  res
}
# end PopProjections

#' @keywords internal
#' @rdname spopmodel-internals
LeslieMatrix <- function(sims, survSims, mn, sdev) {

  # n <- dim(sims)[1]
  # print(length(sims))
  n <- length(sims)

  # variable to hold the Leslie matrix (where nrows = ncols)
  A <- matrix(data = 0, nrow = n, ncol = n)

  # sims is fecundSims * survSims
  A[1, ] <- sims





  # survival rates for ages
  # diag(A[-1, ]) <- survSims[1:(n - 1), ]
  diag(A[-1, ]) <- survSims[1:(n - 1)]

  # last age does not die
  A[n, n] <- bval(mn = mn, sdev = sdev)

  A
}
# end LeslieMatrix

#' @keywords internal
#' @rdname spopmodel-internals
MeanLogLambda <- function(popChanges) {
  mean(log(popChanges))
}
# end MeanLogLambda

#' Calculate Mean Population Growth Rate.
#'
#' @description Given population changes over specified period, \code{Lambda}
#'   calculates mean lambda (population growth rate) with lower & upper bounds.
#'   Lambda = 1 indicates a stable population.
#'
#' @param popChanges A numeric vector providing population changes. Likely the
#'   output from \code{PopProjections}.
#' @param selectCI Numeric value of either 0.90, 0.95, or 0.99. Denotes
#'   confidence interval (to \code{probs} argument) using sample quantiles
#'   \code{\link[stats]{quantile}}. Default is NULL, which is the same as 0.95.
#'
#' @return A dataframe with seven fields, including mean lambda.
#' @export
#'
#' @examples
#' # coming soon
Lambda <- function(popChanges, selectCI = NULL) {

  # added 04-Dec-2019 to allow for CI selection rather than hardcoding 95%;
  # allows for 0.9 & 0.99 in addtion to 0.95 using quantiles
  if (is.null(selectCI)) {
    ci <- 0.95
  } else {
    ci <- match.arg(
      arg = as.character(selectCI),
      choices = c(0.95, 0.99, 0.90),
      several.ok = FALSE
    )
  }

  # to get lower & upper quantile probabilities
  quant <- (1 - ci) / 2
  probs <- c(quant, ci + quant)

  mll <- vapply(popChanges, FUN = MeanLogLambda, FUN.VALUE = numeric(1L))

  bounds <- exp(quantile(mll, probs = probs, names = FALSE))

  # Quant025 and Quant975 changed 04-Dec-2019 to Low & Upp for more general
  # naming; functions relying on 025 & 0975 naming should be updated accordingly

  # function output
  data.frame(
    MuLevel = "TBD",
    NumSims = length(mll),
    MeanLambda = exp(mean(mll)),
    MedLambda = exp(median(mll)),
    VarLambda = exp(var(mll)),
    QuantLow = bounds[1],
    QuantUpp = bounds[2],
    stringsAsFactors = FALSE
  )
}
# end Lambda

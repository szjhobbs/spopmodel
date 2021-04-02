# ******************************************************************************
# Created: 27-Aug-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to create probability of
#          spawning when such emperical data do not exist. Resulting probs
#          are based on literature studies (see Source below) & can be adjusted
#          for percent of mature spawners
# Source:  Chapman et al. 1996, table 2 (change of maturity) [though I believe
#             this should be Chapman 1989, thesis]
#          DeVore et al. 1995 equation for eggs (eggs = 0.072 * FL(cm) ^ 2.94)
#          S. Blackburn (2018)
# ******************************************************************************

#' Suite of functions to generate model data inputs.
#'
#' @description \code{spopmodel} requires specific inputs to operate. In the
#'    absence of such inputs, functions herein can generate dataframes to
#'    satisfy model requirements. If such data already exists, however, then
#'    it's best to use those data. NOTE: Functions herein default for use with
#'    San Francisco Estuary-based White Sturgeon. So do keep that in mind when
#'    employing these functions.
#'
#' @section \code{SpawningProb}
#'
#' @description Uses \code{glm} to predict spawning probability
#'    based on probability of maturity at length \emph{i}. Spawning probability
#'    is then adjusted for fraction of females spawning annually.
#'
#' @param pMat A dataframe with two fields: length & probability of maturity
#'    for each length. Default uses internal \code{SpawningProbWST}
#'    (for White Sturgeon) & from Chapman 1989 (?).
#' @param len Numeric vector of lengths supplied as new data to \code{predict}.
#' @param age Numeric vector of ages, where each age is appropriate for given
#'    length supplied in \code{len}. Vector must be same length as \code{len}.
#' @param mature Fraction of females spawning in a given year. Default 0.15.
#'
#' @section \code{EggCount}
#'
#' @description Uses \code{lm} to predict number of eggs given
#'    length \emph{i}.
#'
#' @param numEggs A dataframe with two fields: length & number of eggs.
#'    for each length. Default uses internal \code{FecundityWST}
#'    (for White Sturgeon) & from Devore 19??, using \eqn{0.072 * len^2.94}.
#'
#' @section \code{AgeDist}
#'
#' @description Uses age frequency to provide by age (1) estimated
#'    abundance & (2) estimated abundance of females. Uses arguments supplied
#'    to \code{abund} & \code{fracFemale} to achieve this.
#'
#' @param ageFreq A numeric vector of age frequency (like that derived from
#'    age-length key or direct ageing).
#' @param abund A numeric scalar given estimated overall abundance for species
#'    being modeled. Default 48,000 (as estimated by CDFW).
#' @param fracFemale A numerice scalar given fraction of females in population.
#'    Default 0.5 from Chapman et al. 1996.
#'
#' @section \code{SurvivalProb}
#'
#' @param ... Passed to internal function \code{CreateSPDataframe}.
#' @param mu A numeric vector giving exploitation (harvest rate;
#'    between 0 and 1). Default 0.01.
#' @param agesMu A numeric vector of ages on which exploitation applies
#'    (i.e., age group subject to harvest). Default 10:15.
#' @param estS A numeric scalar given estimated survival rate of (typically)
#'    adult fish. Derived using \code{ChapmanRobson}. Default 0.8132 from
#'    S. Blackburn thesis work.
#' @param estMu A numeric scalar given estimated exploitation rate of
#'    harvestable fish. Derived using \code{Exploitation}. Default 0.1364 from
#'    S. Blackburn thesis work using 2014-2016 CDFW mark-recapture data.
#' @param methodSB Logical. Default (TRUE) denotes using S. Blackburn method to
#'    calculate \code{F} using \code{Z} & \code{A} calculated from \code{estS}
#'    and not from \code{S0}. Change to FALSE to use \code{S0}.
#'
#' @section \code{CreateSPDataframe}
#' @param ages A numeric vector of ages (e.g., 0:19).
#' @param sRate A numeric vector giving survival rate (between 0 and 1).
#' @param sRateErr A numeric vector given standard error of \code{sRate}.
#'
#' @note Ideally \code{ages} vector supplied from age distribution data (e.g.,
#'    \code{age_dist}). Vector should contain age-0 sequentially through oldest
#'    age. \code{sRate} and \code{sRateErr} need not match \code{ages} in
#'    length, just not longer. If less than, the last value (e.g.,
#'    \code{sRate[length(sRate)]} is repeated to complete the dataframe.
#'    \code{sRate[length(sRate)]} must be S0, that is survival rate considering
#'    only natural mortality.
#'
#' @name input
NULL

#' @rdname input
#' @export
SpawningProb <- function(pMat = NULL, len, age, mature = 0.15) {

  if (is.null(pMat)) {
    pMat <- SpawningProbWST
    warning("Using `SpawningProbWST` for `pMat`.", call. = FALSE)
  }

  # for ease of use in glm()
  cn <- colnames(pMat)
  colnames(pMat) <- c("l", "p", cn[-c(1, 2)])

  # still getting warning (as of 29-Aug-2018) "In eval(family$initialize) :
  # non-integer #successes in a binomial glm!"
  mod <- glm(
    formula = p ~ l,
    family = binomial(link = "logit"),
    data = pMat
  )

  nd <- data.frame(l = len, Age = age)

  pred <- stats::predict(
    object = mod,
    newdata = nd,
    type = "response",
    se.fit = TRUE
  )

  # desired output with age
  nd$Prob <- pred[["fit"]] * mature
  nd$Err <- nd[["Prob"]] * 0.20

  # output 'new data' dataframe
  nd
}
# end SpawningProb

#' @rdname input
#' @export
EggCount <- function(numEggs = NULL, len, age) {

  # note: not sure why using lm(); Devore eqn is not linear
  # Devore eqn: 0.072 * len^2.94

  if (is.null(numEggs)) {
    numEggs <- FecundityWST
    warning("Using `FecundityWST` for `numEggs`.", call. = FALSE)
  }

  # for using Devore eqn (if / when time comes)
  # data <- data.frame(
  #   len = len,
  #   eggs = 0.072 * len^2.94
  # )

  # for ease of use in lm()
  cn <- colnames(numEggs)
  colnames(numEggs) <- c("l", "e", cn[-c(1, 2)])

  mod <- lm(e ~ l, data = numEggs)

  nd <- data.frame(l = len, Age = age)

  res <- stats::predict(
    object = mod,
    newdata = nd,
    se.fit = TRUE,
    type = "response"
  )

  # desired output with age
  nd$Count <- res[["fit"]]
  nd$Err <- res[["se.fit"]]

  # output 'new data' dataframe
  nd
}
# end EggCount

#' @rdname input
#' @export
AgeDist <- function(ageFreq, abund = 48e+3, fracFemale = 0.5) {

  # TODO: provide age-0 count by using num eggs / female, number of females per
  # age class, and probability of spawning; summation of product

  # variales to hold calculations
  total <- sum(ageFreq)

  prop <- ageFreq / total

  est_abund <- prop * abund

  # output list
  list(
    EstAgeAbund = est_abund,
    CountFemByAge = est_abund * fracFemale,
    TotalN = total,
    OverallAbund = abund,
    FracFemale = fracFemale
  )
}
# end AgeDist

#' @rdname input
#' @export
SurvivalProb <- function(
  ..., mu = 0.01, agesMu = 10:15, estS = 0.8132,
  estMu = 0.1364, methodSB = TRUE) {

  # TODO:
  # (1) need to improve error checking for `vals` variable
  # (2) need to improve method for getting S0 error rate; 0.045 developed using
  #     S. Blackburn data of S0 = 0.94576 & error = 0.04281, not sure how
  #     error of 0.04281 was calculated (02-Aug-2019)

  # uncomment if needed for error checking
  # dots <- objects(list(...))

  # primarily for length(mu) > 1 so each element is named with appropriate mu
  # level in returned list of dataframes (see lapply() below)
  names(mu) <- paste("mu", mu, sep = "_")

  # to get Z, A, & M from estimated survival rate (on adult fish, i.e., age 9+)
  # & estimated exploitation (mu) on that subset of the population
  fp <- FishingParams(S = estS, mu = estMu)
  M <- fp[["M"]]

  # at mu = 0, Z = M, so S0 = exp(-M) which represents survival rate given no
  # fishing mortality; this value will be used for non-harvestable ages
  S0 <- exp(-M)

  # empty variables for if statement below
  zz <- NA
  aa <- NA

  # with option to configure like method used by S. Blackburn, using values
  # calculated from supplied values of estS & estMu (see fp results) rather than
  # using values given no fishing mortality
  if (methodSB) {
    zz <- fp[["Z"]]
    aa <- fp[["A"]]
  } else {
    zz <- M
    aa <- 1 - S0
  }

  # to calculate F (instantaneous mortality rate from fishing, Ricker p 10)
  f <- (mu * zz) / aa

  # not likely but just in case
  if (any(is.na(f)))
    stop("Fishing mortality cannot be NA. Check arguments.", call. = FALSE)

  # to calculate "new" Z & S given fishing pressure & natural mortality (M)
  z <- f + M # instantaneous total mortality rate
  s <- exp(-z)       # survival rate for fish within harvestable age

  # ages, sRate, sRateErr
  vals <- list(...)

  if (length(vals) == 0)
    stop("Must supply `ages`, `sRate`, & `sRateErr` to ...", call. = FALSE)

  # establishes dataframe for output
  d <- CreateSPDataframe(
    ages = vals[["ages"]],
    sRate = c(vals[["sRate"]], S0),
    sRateErr = c(vals[["sRateErr"]], S0 * 0.045)
  )

  # to "apply" the "new" S to only desired ages (i.e., ages deemed susceptible
  # to fishing mortality)
  bool <- d[["Ages"]] %in% agesMu #& mu != 0

  # for compatibility with initial function design where multiple mus outputted
  # as a list of dataframes (Map() needed over lapply() to check mu = 0)
  out <- Map(f = function(S, m) {
    e <- 0.45
    if (m == 0) e <- 1
    d[bool, "Prob"] <- S
    d[bool, "Err"] <- d[bool, "Err"] * e
    d
  }, s, mu)

  # so output is not a list with only 1 item (i.e., length(mu) = 1)
  if (length(out) == 1) out <- out[[1]]

  # just FIO (ideally values should be very close)
  msg <- "Natural mortality (M): %.4f | S0 from natural mortality: %.4f"
  attr(out, which = "metadata") <- sprintf(fmt = msg, M, S0)

  out
}
# end SurvivalProb

#' @rdname input
#' @keywords internal
CreateSPDataframe <- function(ages, sRate, sRateErr) {
  # function used to create & return a dataframe given user inputs of ages,
  # survvial rate (sRate), & survival rate error (sRateErr); all inputs need to
  # be numeric & length(ages) can be >= length(sRate | sRateErr) but not <

  # for input validation & proper subsetting
  nA <- length(ages)
  nS <- length(sRate)
  nE <- length(sRateErr)

  # for input validation **************************************************
  if (!identical(x = nS, y = nE))
    stop("Lengths must be equal: `sRate` & `sRateErr`.", call. = FALSE)

  if (nA < nS)
    stop("More `sRate` values than ages.", call. = FALSE)
  # ***********************************************************************

  # just create & return dataframe if all inputs of equal length
  if (nA == nS) {
    d <- data.frame(Ages = ages, Prob = sRate, Err = sRateErr)
    return(d)
  }

  # for completing dataframe d as output
  ss <- sRate[nS]
  ee <- sRateErr[nE]
  i <- nA - (nA - nS)

  # for messaging *********************************************************
  aa <- paste0(range(ages[i:nA]), collapse = "-")
  msg <- "`sRate` %.4f & `sRateErr` %.4f used as initial values for ages %s."
  warning(sprintf(fmt = msg, ss, ee, aa), call. = FALSE)
  # ***********************************************************************

  # for proper subsetting & repeating below
  ii <- i - 1
  rr <- nA - ii

  # for output
  d <- data.frame(
    Ages = ages,
    Prob = c(sRate[1:ii], rep(ss, times = rr)),
    Err = c(sRateErr[1:ii], rep(ee, times = rr))
  )

  d
}
# end CreateSPDataframe

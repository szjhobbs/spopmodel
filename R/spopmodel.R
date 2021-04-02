#' spopmodel: A package for modeling fish population dynamics.
#'
#' @description \code{spopmodel} employs a female-based Leslie Matrix to
#'    evaluate population dynamics (specifically population growth
#'    rate, \eqn{\lambda}) over varying exploitation (\eqn{\mu}). Model inputs
#'    expects age-specific probalities of spawning & survival, age-specific
#'    fecundity, and age distribution proportional to some estimated abundance.
#'    \code{spopmodel}'s creation stems from the collaborative efforts
#'    of University of Idaho and CA Department of Fish & Wildlife.
#'
#' @section Age Distribution:
#'    Age frequency scaled to some absolute abundance estimate (see example
#'    below). Model requires young (i.e., age-0; age-1) fish, so values from
#'    literature (or other sources) may be necessary to complete data gaps.
#'
#'    Example:
#'       \deqn{freq_{age_{i}} \times \hat{abund}}
#'
#' @section Survival Probability:
#'    Survival at age (as assigned in Age Distribution). Again values can be
#'    filled in where needed. Probability to be adjusted downward for ages
#'    affected by exploitation (\eqn{\mu}). In the case of White Sturgeon for
#'    example, survival estimated at ~0.94, and for ages 10-15 adjusted downard
#'    by ~0.13. These ages vunerable to fishing mortality.
#'
#' @section Spawning Probability:
#'    Probability based on first age of maturity. For White Sturgeon (as an
#'    example), this is ~age-10. Again, probability should be in-line with
#'    ages set in Age Distribution, and this may require setting to 0 age-0
#'    through age-<first age maturity>-1.
#'
#' @section Fecundity:
#'    Number of eggs per age, starting with first age of maturity. If data are
#'    lacking, may need to consult literature for species in question.
#'
#' @note For probability datasets & fecundity, some measure of error (i.e.,
#'    standard deviation) per value is required. These values are used in the
#'    simulation process.
#'
#' @docType package
#' @name spopmodel
NULL

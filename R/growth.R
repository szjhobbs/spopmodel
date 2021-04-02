# ******************************************************************************
# Created: 23-Ju1-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to calculate growth given
#          age and length and (or) mean length at age.
# Source:  most functions herein use von Bertalanffy growth model as described
#          in Ricker 1975, eqn. 9.9; we use VBGM to get length given age
# ******************************************************************************

#' @keywords internal
#' @rdname spopmodel-internals
GetVBGM <- function(Linf, K, t0) {

  #  t = age or ages
  fn <- function(t) {

    if (!is.numeric(t))
      stop("`age` must be numeric.", call. = FALSE)

    # calculate length at age (laa)
    laa <- Linf * (1 - exp(-K * (t - t0)))

    # output laa with parameters
    list(
      Linf = Linf,
      K = K,
      t0 = t0,
      LAA = laa
    )
  }
  # end fn

  # return function
  fn
}
# end GetVBGM

#' Fits von Bertalanffy growth model (VBGM) using \code{stats::nls}.
#'
#' @description \code{FitVBGM} using Ricker 1975, eqn. 9.9 to calculate
#'    length at age.\eqn{l_t = L_{inf}(1 - e^{-K(t-t_0)})}
#'
#' @param data Dataframe with length and age fields.
#' @param len Numeric length field within data.
#' @param age Numeric age field within data.
#' @param start Starting parameters passed to \code{stats::nls}. Must be
#'    a named list with values for "Linf", "K", and "t0".
#' @param ... Further arguments passed to \code{\link{nls}}.
#'
#' @return Model fit object of type \code{\link{nls}}.
#' @export
#'
#' @references Ricker, W.E. (1975). Computation and Interpretation of
#'    Biological Statistics of Fish Populations. Bulletin of the Fisheries
#'    Research Board of Canada, Bulletin 191, Ottawa.
#'
#' @examples
#' # start_vals from Cal Fish and Game, Kohlhorst et al 1980
#' start_vals <- list(Linf = 261.2, K = 0.04027, t0 = 3.638)
#' catch_data <- subset(trammel_catch, !is.na(Age))
#' # below example giving error...need to troubleshoot
#' \dontrun{
#' FitVBGM(
#'   data = subset(trammel_catch, !is.na(Age)),
#'   len = FL,
#'   age = Age,
#'   start = start_vals
#' )
#' }
FitVBGM <- function(data, len, age, start = list(), ...) {

  y <- as.character(substitute(len))
  x <- as.character(substitute(age))

  # exception handling ***********************************************
  if (!all(c(x, y) %in% colnames(data)))
    stop(x, "|", y, " fields not found in data.", call. = FALSE)

  if (!is.list(start))
    stop("`start` must be a list.", call. = FALSE)

  params <- c("Linf", "K", "t0")

  if (is.null(names(start)) || !all(names(start) %in% params)) {
    msg <- "`start` must be a named list, use names: "
    stop(msg, paste0(params, collapse = "; "), call. = FALSE)
    rm(msg)
  }

  if (!is.numeric(unlist(start, use.names = FALSE)))
    stop("all `start` values must be numeric.", call. = FALSE)
  # ******************************************************************

  rhs <- paste0("Linf * (1 - exp(-K * (", x, " - t0)))")
  frmla <- as.formula(paste(y, rhs, sep = " ~ "))

  fit <- stats::nls(formula = frmla, data = data, start = start, ...)

  fit
}
# end FitVBGM

#' Uses Dahl-Lea method to back-calculate length at age.
#'
#' @description \code{BackCalcLength} employs the Dahl-Lea algorithm to
#'    back-calculate length at age given specific data from the analysis
#'    of hard parts (e.g., scales). Assumes for now first age is 1.
#'
#' @param data Dataframe where each row is a single fish & contains
#'    fields below.
#' @param lCap Field name in data containing (numeric) length at capture.
#' @param rCap Field name in data containing (numeric) scale radius at capture.
#' @param rAtAge Field (column) names or column numbers (easier) in \code{data}
#'    containing scale radius at each age \emph{i}.
#'
#' @return A dataframe with two numeric fields: \code{len} & \code{age}. NA
#'    values have been removed.
#' @export
#'
#' @references Lea, E. 1910. On the methods used in the herring-investigations.
#'             Publ. Circonst. Cons. perm. int. Explor. Mer 108(1):14–22.
#'
#'             Ogle, D.H. 2016. Introductory Fisheries Analyses with R. Chapman
#'             & Hall/CRC, Boca Raton, FL.
#'
#' @examples
#' # code here
BackCalcLength <- function(data, lCap, rCap, rAtAge) {

  l <- as.character(substitute(lCap))
  r <- as.character(substitute(rCap))

  # assumes for now first rAtAge is age 1
  d <- data[rAtAge]
  colnames(d) <- 1:ncol(d)

  dahl_lea <- (d * data[[l]]) / data[[r]]

  out <- reshape2::melt(
    data = dahl_lea,
    # id.vars = ,
    measure.vars = 1:ncol(dahl_lea),
    variable.name = "age",
    na.rm = TRUE,
    value.name = "len"
  )

  # need age as numeric, not factor
  out$age <- as.numeric(as.character(out[["age"]]))
  rownames(out) <- NULL
  out
}
# end BackCalcLength

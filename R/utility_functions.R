# ******************************************************************************
# Created: 20-Ju1-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains other functions used by other functions within
#          this package or (some) to be used by the user
# ******************************************************************************

#' @keywords internal
#' @rdname spopmodel-internals
ToCM <- function(x, unit = c("in", "ft", "mm", "m", "cm")) {

  # input validation
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  unit <- match.arg(arg = unit)

  # variable to hold function output
  res <- rep(NA, times = length(x))

  # convert to cm depending on input
  switch(
    EXPR = unit,
    "in" = res <- x * 2.54,
    "ft" = res <- (x * 12) * 2.54,
    "mm" = res <- x / 10,
    "m"  = res <- x * 100,
    "cm" = res <- x
  )

  if (any(is.na(res))) {
    w <- sprintf("Values NA: %d. Returning NA.", sum(is.na(res)))
    warning(w, call. = FALSE)
  }

  # return x converted to cm accordingly
  res
}
# end ToCM

#' @keywords internal
#' @rdname spopmodel-internals
CreateLenBins <- function(
  len, lenBreaks, breakLabs = NULL, numericBin = FALSE, ...) {

  if (!is.numeric(len))
    stop("`len` must be numeric.", call. = FALSE)

  b <- cut(
    len,
    breaks = lenBreaks,
    labels = breakLabs,
    include.lowest = FALSE,
    right = FALSE,
    ...
  )

  if (is.null(breakLabs) && numericBin) {
    # assures replacement of (for example) "[210,215)" with coercible numeric
    # (i.e., "210") using min value only (value left of comma)

    # removes L/R brackets & L/R parentheses from e.g., [100,105)
    b <- gsub(pattern = "\\[|\\]|\\(|\\)", replacement = "", x = b)

    b <- strsplit(b, split = ",")

    # get min value from each bin (i.e., left value in example [100,105))
    b <- vapply(b, FUN = '[', 1, FUN.VALUE = character(1L))

    # convert to numeric for output
    b <- as.numeric(b)
  }

  if (any(is.na(b[!is.na(len)])))
    warning("Some bins are NA. Suggest different breaks.", call. = FALSE)

  attr(b, which = "metadata") <- "bins closed right"
  b
}
# end CreateLenBins

#' @keywords internal
#' @rdname spopmodel-internals
CheckBin <- function(x) {
  # x really could be any vector but using this funtion to check length bins
  # after employing split(); so likely use CheckBin within vapply()

  # check if empty (i.e., length = 0)
  if (length(x) == 0) return("empty")

  # check if NA
  if (all(is.na(x))) return("all NA")

  # otherwise return NA
  NA_character_
}
# end CheckBin

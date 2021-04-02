#' Survival probability by age for San Francisco Estuary-based White Sturgeon.
#'
#' @description A dataset for White Sturgeon (\emph{Acipenser transmontanus})
#'    with 20 ages (0-19) and corresponding survival probability
#'    with associated error.
#'
#' @format A data frame with 20 rows and 3 variables: \describe{
#'    \item{age}{numeric age from 0-19}
#'    \item{prob}{probability of survival at each age}
#'    \item{se}{error associated with prob}
#' }
#'
#' @details Survival rate estimated using \code{ChapmanRobson()}. Value then
#'    assigned for ages 3-19. Rate adjusted for exploitation (using
#'    \code{Exploitation()}) for ages within the slot limit (for this dataset
#'    that is ages 10-15, which represents the current slot of 40-60 inches FL).
#'    Exploitation for current slot ~13\% given 2007-2015 CDFW
#'    mark-recapture data.
#'
#'    \itemize{
#'      \item Age 0 survival from Caroffino et al. (2010).
#'      \item Age 1 survival from Pine et al. (2001).
#'      \item Age 2 survival from Ireland et al. (2002).
#'    }
#'
#' @references Caroffino D. C., T. M. Sutton, R. R. Elliott, and M. C. Donofrio.
#'    2010. Early life stage mortality rates of Lake Sturgeon in the
#'    Peshtigo River, Wisconsin.
#'    North American Journal of Fisheries Management 30:295-304.
#'
#'    Pine, W. E., M. S. Allen, and V. J. Dreitz.  2001.
#'    Population viability of the Gulf of Mexico Sturgeon:
#'    inferences from capture-recapture and age-structuted models.
#'    Transactions of the American Fisheries Society 130:1164-1174.
#'
#'    Ireland, S., C. R. C. Beamesderfer, V. L. Paragamian, V. D. Wakkinen,
#'    and J. T. Siple.  2002. Success of hatchery-reared juvenile White Sturgeon
#'    (Acipenser transmontanus) following release in the Kootenai River,
#'    Idaho USA. Journal of Applied Ichthyology 18:642-650.
#'
#' @source Model development input data (CDFW; Univ. of Idaho).
#' @name prob_survival
NULL

# ******************************************************************************
# Created: 06-Aug-2018
# Author:  J. DuBois (this file), R. Millar imported code
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: file contains functions and (or) methods for gear selectivity. herein
#          we use source code from R. Millar not contained in R package. code
#          used for net efficiency analytics (or adjusting catch based on size
#          selectivity of gear)
# Source:  https://www.stat.auckland.ac.nz/~millar/selectware/RNext/
#
# ******************************************************************************

#' Download R. Millar gear selectivity code.
#'
#' @description \code{LoadMillarCode} downloads to user's computer
#'    gear selectivity source code written by Russel Millar. Files
#'    downloaded currently are NextGeneration.R & SelnCurveDefinitions.R,
#'    the main two used in this package. Files download to root directory.
#'
#' @note Only run this code if interested in seeing source files. Other-
#'    wise, gear selectvity functions within this package will suffice.
#'
#' @return Message giving file path to where files were downloaded.
#' @export
#'
#' @references https://www.stat.auckland.ac.nz/~millar/selectware/RNext/
#'
#'             https://www.stat.auckland.ac.nz/people/rmil013
#'
#'             Millar and Holst (1997). Estimation of gillnet and hook
#'             selectivity using log-linear models
#'             ICES J. Mar. Sci. 54: 471-477.
#'
#' @examples
#' \dontrun{
#' LoadMillarCode()
#' }
LoadMillarCode <- function() {

  millar_page <- "https://www.stat.auckland.ac.nz/~millar/selectware/RNext/"

  # files needed from Millar page
  files_millar <- list(
    NextGen = "NextGeneration.R",
    SelectCurves = "SelnCurveDefinitions.R"
  )

  # create directory at root level for convenience
  new_dir <- path.expand("~/source-millar")

  if (dir.exists(new_dir))
    stop("'", new_dir, "'", " already exists. Please verify.", call. = FALSE)

  dir.create(path = new_dir)

  # download files to temp directory
  lapply(files_millar, FUN = function(fl) {
    fname <- fl
    fl <- file.path(millar_page, fl)
    download.file(fl, destfile = file.path(new_dir, fname))
    invisible()
  })

  # clean up & output to user
  rm(millar_page, files_millar)
  cat("Files downloaded at:\n", new_dir, "\n")
}
# end LoadMillarCode

#' Apply Millar's \code{NetFit} to catch data using gill net or trammel net.
#'
#' @param data A dataframe with at least length and net mesh (size) fields.
#' @param len Field name within data containing (numeric) length measurement.
#' @param mesh Field name within data containing size of net mesh. Field data
#'    can be character but must be "coerceable" to numeric.
#' @param meshUnit Scalar character containing unit of measure of net. Choices
#'    are "in" (inches), "ft" (feet), "mm" (millimeters), "m" (meter), and
#'    "cm" (centimeters). \code{ApplyNetFit} will convert to cm.
#' @param relPower The fishing power of each net size. Leave NULL (default)
#'    if only single mesh or all meshes are fished equally. Otherwise, supply
#'    numeric vector (length = number of mesh sizes) representing net
#'    configuration effort. For example, if net mesh sizes are 6", 7", & 8" and
#'    the 8" mesh is fished at twice the effort, relPower = c(1, 1, 2) [for
#'    6", 7", & 8"].
#'
#' @return A list of model fits (see details).
#' @export
#'
#' @references https://www.stat.auckland.ac.nz/~millar/selectware/RNext/
#'
#'             https://www.stat.auckland.ac.nz/people/rmil013
#'
#'             Millar and Holst (1997). Estimation of gillnet and hook
#'             selectivity using log-linear models
#'             ICES J. Mar. Sci. 54: 471-477.
#'
#' @details \code{ApplyNetFit} fits 5 Millar models: \code{norm.loc};
#'    \code{norm.sca}; \code{lognorm}; \code{binorm.sca}; and \code{bilognorm}.
#'    Each one of these models is a list with 12 items (explained below).
#'
#' @section Value:
#'
#' \code{ApplyNetFit} returns a list of class "NetFit" each list item
#'    containing the components below.
#'
#' \code{par}         see **Value** in \code{\link{optim}}
#'
#' \code{value}       see **Value** in \code{\link{optim}}
#'
#' \code{counts}      see **Value** in \code{\link{optim}}
#'
#' \code{convergence} see **Value** in \code{\link{optim}}
#'
#' \code{message}     see **Value** in \code{\link{optim}}
#'
#' \code{hessian}     see **Value** in \code{\link{optim}}
#'
#' \code{Deviance}    model deviance (value)
#'
#' \code{deviance}    model deviance (function); not sure of purpose
#'
#' \code{rtype}       the name (as character) of the model
#'
#' \code{rel.power}   values supplied to relPower, relative fishing power
#'                    of the gear
#'
#' \code{Meshsize}    mesh size (in centimeters)
#'
#' \code{Data}        length frequency by mesh size (raw count data)
#'
#' @examples
#' ApplyNetFit(
#'   data = trammel_catch,
#'   len = FL,
#'   mesh = MeshSize,
#'   meshUnit = "in",
#'   relPower = c(1, 1, 2)
#' )
ApplyNetFit <- function(
  data, len, mesh, meshUnit = "in", relPower = NULL) {

  l <- as.character(substitute(len))
  m <- as.character(substitute(mesh))

  # because mesh needs to be converted to numeric
  if (any(grepl(pattern = "[^[:digit:]]", x = data[[m]])))
    stop("`mesh` must only contain numbers.", call. = FALSE)

  if (is.null(relPower))
    warning(
      "`relPower` not supplied. Default used for each mesh size: 1",
      call. = FALSE,
      noBreaks. = TRUE
    )

  # get length frequency by mesh; data need to be in dataframe format for NetFit
  freq <- reshape2::dcast(
    data = data.frame(table(data[[l]], data[[m]])),
    formula = Var1 ~ Var2,
    value.var = "Freq"
  )

  # for data clean up after usine dcast
  freq$Var1 <- as.numeric(as.character(freq[["Var1"]]))
  colnames(freq)[1] <- l

  # NetFit requires mesh to be in centimeters (or at least metric)
  msize <- ToCM(as.numeric(colnames(freq)[-1]), unit = meshUnit)

  # NetFit needs starting values
  X0 <- GetX0(svals = StartVals(len = data[[l]], mesh = data[[m]]))

  # apply NetFit() for all rtypes
  fit <- mapply(
    FUN = NetFit,
    rtype = names(X0),
    x0 = X0,
    MoreArgs = list(
      Data = freq,
      Meshsize = msize,
      rel.power = relPower
    ),
    SIMPLIFY = FALSE
  )

  class(fit) <- "NetFit"

  # function output
  fit
}
# end ApplyNetFit

#' @keywords internal
#' @rdname spopmodel-internals
StartVals <- function(len, mesh, FUN = NULL) {

  # function list for convenience of applying all functions to len
  fn <- list(
    N = length,
    Min = min,
    Max = max,
    Median = median,
    Mean = mean,
    SD = sd,
    LogMean = function(x) mean(log(x)),
    LogSD = function(x) sd(log(x))
  )

  # for convenience when applying functions in function list above
  if (any(is.na(len))) {
    n <- sum(is.na(len))
    len <- len[!is.na(len)]
    warning("NA values removed from `len`: ", n, call. = FALSE)
  }

  # for outer vapply (loop) below
  n <- length(unique(Filter(function(x) !is.na(x), mesh)))

  # get values by mesh for starting point
  svals <- vapply(fn, FUN = function(f) {
    vapply(split(len, f = mesh), FUN = f, FUN.VALUE = numeric(1L))
  }, FUN.VALUE = numeric(n))

  svals
}
# end StartVals

#' @keywords internal
#' @rdname spopmodel-internals
GetX0 <- function(svals) {

  # 5th parameter value for binormal and bilognorm curves
  val <- 0.65 # 0.65 chosen arbitrarily
  fifth_param <- log(val / (1 - val))

  loc <- unname(colMeans(svals[, c("Mean", "SD")]))
  lgn <- unname(colMeans(svals[, c("LogMean", "LogSD")]))

  # not sure about this but need to get second values for bi... models
  m1 <- c(1.95, 1) # increase 95%
  m2 <- c(1.20, 1) # increase 20%

  list(
    norm.loc = loc,
    norm.sca = loc,
    lognorm = lgn,
    binorm.sca = c(loc, loc * m1, fifth_param),
    bilognorm = c(lgn, lgn * m2, fifth_param)
  )
}
# end GetX0

# methods: DeviancePlots --------------------------------------------------

#' Generate Millar's deviance plots for gear selectivity.
#'
#' @description We choose a selectivity model based on deviance. The lower the
#'   deviance, the better the fit. Deviance plots display residuals (by mesh
#'   size) in a "bubble-plot" format. (describe 'bubbles' and size here) Plot is
#'   a side effect of Millar's \code{Summary} function. Function returns a
#'   list, with number of items equal to the number of models (i.e., 5). See
#'   Values for output.
#'
#' @param x Generic parameter for S3 method. Likely a model fit
#'    of class "NetFit".
#' @param ... Passed to other methods.
#'
#' @return If \code{x} is a list, output is a list with same number of
#'   elements, each element is a matrix. Otherwise output is a matrix with one
#'   column. See Values.
#' @export
#'
#' @note \code{DeviancePlots} is essentially a convenient wrapper for Millar's
#'    \code{Summary} function.
#'
#' @section Values:
#'
#' \describe{
#'   \item{null.l}{doc needed here}
#'   \item{model.l}{doc needed here}
#'   \item{full.l}{doc needed here}
#'   \item{Deviance}{model deviance}
#'   \item{Pearson.chisq}{doc needed here}
#'   \item{d.o.f}{degrees of freedom}
#' }
#'
#' @examples
#' # for method .NetFit
#' net_fit <- ApplyNetFit(
#'   data = trammel_catch,
#'   len = FL,
#'   mesh = MeshSize,
#'   meshUnit = "in",
#'   relPower = c(1,1,2)
#' )
#' DeviancePlots(net_fit)
DeviancePlots <- function(x, ...) {
  UseMethod("DeviancePlots")
}

#' @rdname DeviancePlots
#' @export
#' @description Default S3 method.
DeviancePlots.default <- function(x, ...) {

  Summary(fit = x, ...)
}
# end DeviancePlots.default

#' @rdname DeviancePlots
#' @export
#' @description S3 method for class NetFit.
DeviancePlots.NetFit <- function(x, ...) {

  lbls <- paste("Deviance residuals:", names(x))

  Map(f = Summary, x, label = lbls, xlabel = "Length")
}
# end DeviancePlots.NetFit

# methods: RelativeRetention ----------------------------------------------

#' Get gear relative retention based on net fit model output.
#'
#' @description \code{RelativeRetention} is a re-write of Millar's
#'    \code{PlotCurves} for convenient use within this package. It
#'    generates (among other items) a dataframe relative retention values
#'    for each gear type (columns) by each length (or length bin; rows).
#'    Out put can then be used to 'adjust' catch accordingly.
#'
#' @param x Generic for S3 methods. Model output from \code{NetFit}.
#' @param ... Passed to other methods.
#'
#' @return List of objects. See Values.
#'
#' @section Values:
#'
#' \describe{
#'   \item{Data}{dataframe of relative retention values}
#'   \item{Freq}{raw frequency data (i.e., recorded catch)}
#'   \item{RelPower}{relative power of gear type}
#'   \item{MeshSize}{mesh size of each gear}
#'   \item{Deviance}{measure of 'fit' for each model}
#'   \item{ColNames}{column names of \code{Data}}
#'   \item{Model}{name of net fit model}
#'   \item{Standardized}{true or false, is relative retention standardized}
#' }
#'
#' @export
#'
#' @examples
#' # for .NetFit method
#' net_fit <- ApplyNetFit(
#'   data = trammel_catch,
#'   len = FL,
#'   mesh = MeshSize,
#'   meshUnit = "in",
#'   relPower = c(1,1,2)
#' )
#' RelativeRetention(net_fit)
RelativeRetention <- function(x, ...) {
  UseMethod("RelativeRetention")
}

#' @rdname RelativeRetention
#' @description Default S3 method.
#'
#' @param standardize Boolean. If \code{TRUE} (default), relative
#'    retention values are divided by \code{max(relative retention)}.
#'    If \code{FALSE}, not such division occurs.
#' @export
RelativeRetention.default <- function(x, ..., standardize = TRUE) {

  # function copied from Millar's PlotCurves(); slight changes tailored to needs
  # of this package

  # variables from fit list
  plot_lens <- x[["Data"]][[1]]
  mesh_size <- x[["Meshsize"]]
  r <- selncurves(x[["rtype"]])
  param <- x[["par"]]
  rel_power <- x[["rel.power"]]

  # create matrix of relative retention values
  rmatrix <- outer(X = plot_lens, Y = mesh_size, FUN = r, param)
  rmatrix <- t(t(rmatrix) * rel_power)

  # make standardization possible
  if(standardize) rmatrix <- rmatrix / max(rmatrix)

  # output as dataframe for eventual plotting
  res <- data.frame(
    plot_lens,
    rmatrix
  )

  # for consistency in naming
  colnames(res) <- colnames(x[["Data"]])

  out <- list(
    Data = res,
    Freq = x[["Data"]][, -1],
    RelPower = rel_power,
    MeshSize = x[["Meshsize"]],
    Deviance = x[["Deviance"]],
    ColNames = colnames(res),
    Model = x[["rtype"]],
    Standardized = standardize
  )

  class(out) <- "RelativeRetention"

  # function output
  out
}
# end RelativeRetention.default

#' @rdname RelativeRetention
#' @export
#' @description S3 method for class \code{NetFit}.
RelativeRetention.NetFit <- function(x, ...) {
  Map(f = RelativeRetention, x, ...)
}
# end RelativeRetention.NetFit

#' @keywords internal
#' @rdname spopmodel-internals
print.RelativeRetention <- function(x, ...) {
  out <- sprintf("model deviance: %.3f", x[["Deviance"]])
  cat("   ", out, "\n")
}
# end print.RelativeRetention

#' Plot relative retention values for each gear size.
#'
#' @description This is an S3 method for \code{plot} of class
#'    \code{RelativeRetention}. It allows the user to visually
#'    display relative retentions of each gear size and to
#'    view on each plot model deviance (for ease of deciding best fit).
#'
#' @param x Object of class \code{RelativeRetention}.
#' @param ... Passed on to other methods.
#'
#' @return Nothing. But does display (base R) line plot of relative renetions
#'    as a function of measured length (e.g., fork length). Plot title
#'    contains model name, model deviance, and true or false for
#'    "is standardized?". See default method
#'    \code{\link{RelativeRetention}}.
#' @export
#'
#' @examples
#' net_fit <- ApplyNetFit(
#'   data = trammel_catch,
#'   len = FL,
#'   mesh = MeshSize,
#'   meshUnit = "in",
#'   relPower = c(1,1,2)
#' )
#' rr <- RelativeRetention(net_fit)
#' plot(rr$norm.loc)
plot.RelativeRetention <- function(x, ...) {

  cn <- x[["ColNames"]]

  xdat <- x[["Data"]][[cn[1]]]
  ydat <- x[["Data"]][cn[-1]]

  xrng <- range(xdat)
  yrng <- range(ydat)

  cols <- 1:length(cn[-1])

  # for informative plot title
  ttl <- sprintf(
    "model: %s | deviance: %.3f | standardized: %s",
    x[["Model"]],
    x[["Deviance"]],
    tolower(x[["Standardized"]])
  )

  plot(
    x = xrng,
    y = yrng,
    type = "n",
    las = 1,
    xlab = cn[1],
    ylab = "Relative retention",
    main = ttl
  )

  # add lines & legend to plot
  mapply(FUN = lines, ydat, col = cols, MoreArgs = list(x = xdat, lwd = 2))
  legend(
    "topright",
    legend = cn[-1],
    col = cols,
    lty = 1,
    lwd = 2,
    # text.width = 0.25,
    y.intersp = 0.5,
    x.intersp = 0.25,
    title = "Mesh",
    seg.len = 0.5
  )
}
# end plot.RelativeRetention

# methods: adjusted frequency ---------------------------------------------

#' Get adjusted frequency based on relative retention results.
#'
#' @description Provides a convenient way to adjust catch overall given
#'   results of gear selectivity (i.e., \code{NetFet} model results).
#'
#' @param x Generic for S3 methods.
#' @param ... Passed to other methods.
#'
#' @return A dataframe of catch and adjusted catch per length (row).
#' @export
#'
#' @examples
#' # code here
AdjustedFreq <- function(x, ...) {
  UseMethod("AdjustedFreq")
}

#' @rdname AdjustedFreq
#' @export
AdjustedFreq.default <- function(x, ...) {
  "currently no default method"
}

#' @rdname AdjustedFreq
#' @export
#' @description S3 method for class \code{RelativeRetention}.
AdjustedFreq.RelativeRetention <- function(x, ...) {

  freq <- x[["Freq"]]
  relret <- x[["Data"]]

  adj_freq <- freq / relret[, -1]

  out <- data.frame(
    relret[1],
    Freq = rowSums(freq),
    AdjFreq = round(rowSums(adj_freq), digits = 0),
    row.names = NULL
  )

  out
}
# end AdjustedFreq.RelativeRetention

#
#Imports:
#   Rcpp (>= 0.12.5)
#LinkingTo: Rcpp
#
#' @title Fault reactivation
#' @description The functions in this package can be used to calculate various properties
#' of a fault in idealized rock, including frictional coefficients, stability angles, and
#' stress ratio, etc.
#' @docType package
#' @author A.J. Barbour
#' @name sibson
#' @importFrom graphics axis box layout legend lines par plot segments text
#' @references Sibson, R. H. (1985),
#' A note on fault reactivation,
#' \emph{Journal of Structural Geology}, \strong{7}(6),
#' 751-754
#' \url{http://dx.doi.org/10.1016/0191-8141(85)90150-6}
#' @seealso \code{\link{Sibson.fric}}
Set1 <- kook::brew.set1()
Set1l <- kook::brew.set1(TRUE)

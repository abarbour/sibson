#' @export
#' @title Principal stress ratio
#' @description Calculate the ratio of principal \emph{effective} stresses
#' @details The ratio of principal effective stresses, \eqn{R}, is given by
#' \deqn{R=\frac{\sigma_1 - P}{\sigma_3 - P}=\frac{\sigma_1'}{\sigma_3'}=\frac{1 + \mu/\tan{\theta}}{1 - \mu \tan{\theta}}}
#' where \eqn{P} is the pore fluid pressure. This function calculates \eqn{R} as given above
#' and in Equation 4 from Sibson (1985).
#' @param mu numeric; the static coefficient(s) of friction
#' @param theta numeric; optional; the orientation(s) of the fault with respect to the principal compressive stress
#' @param thresh numeric; an arbitrary clipping level for the stress ratio
#' @param in.deg logical; is \code{theta} given in degrees?
#' @seealso \code{\link{Sibson.fric}}, and
#' \code{\link{Stability}}, which calculates the minimum \eqn{R} for reactivation.
#' @references Sibson (1995; Equation 4)
StressRatio <- function(mu, theta, thresh=NULL, in.deg=TRUE){
  if (in.deg) theta <- theta * pi /180
  R <- (1 + mu * cotan(theta))/(1 - mu * tan(theta))
  if (!is.null(thresh)){
    R[abs(R) > as.numeric(thresh)] <- NA
  }
  return(R)
}

#' @export
#' @title Static friction coefficient
#' @description Calculate frictional coefficients for a fault with a given orientation
#' @inheritParams StressRatio
#' @param ... additional parameter sent to \code{\link{.Mu.opt}}
#' @seealso \code{\link{Sibson.fric}}
Mu <- function(theta, ...){
  theta <- lower.saw(upper.saw(theta, 90), 45)
  apply(matrix(theta,1), 2, .Mu.opt, ...)
}

#' @export
#' @rdname Mu
#' @param delta.theta numeric; the uncertainty in theta, used to calculate uncertainty in the frictional coefficient
#' @param precision integer; the numerical precision used to \code{\link{round}} the frictional coefficient
.Mu.opt <- function(theta, delta.theta=0, in.deg=TRUE, precision=getOption("digits")){
  if (in.deg){
    theta <- theta * pi / 180
    delta.theta <- delta.theta * pi / 180
  }
  mu <- round(cotan(theta), precision)
  if (length(delta.theta) > 1 & (length(delta.theta) != length(theta))) warning('uncertainties recycled -- check length')
  
  # found by differentiation
  delta.mu <- delta.theta^2 * sqrt(tan(theta)^2 + cotan(theta)^2)
  
  if (in.deg){
    theta <- theta * 180 / pi
  }
  return(c(theta=theta, mu=mu, delta.mu=delta.mu))
}

#' @export
#' @title Calculate stability conditions for a frictional coefficient
#' @description Calculate stability conditions for a frictional coefficient
#' @inheritParams StressRatio
#' @param ... additional parameter sent to\code{\link{.StressRatio.opt}} and \code{\link{.Theta.opt}}
#' @seealso \code{\link{Sibson.fric}}
Stability <- function(mu, ...){
  return(c(.StressRatio.opt(mu), .Theta.opt(mu, ...)))
}

#' @export
#' @rdname Stability
.StressRatio.opt <- function(mu){
  # Sibson (1985, Equation 5)
  return(c(`R.opt`=(sqrt(1 + mu^2) + mu)^2))
}

#' @export
#' @rdname Stability
.Theta.opt <- function(mu, in.deg=TRUE){
  # Sibson (1985, just after Equation 5)
  theta <- atan(1/mu)/2
  if (in.deg) theta <- theta * 180 / pi
  return(c(theta=theta, `2*theta`=2*theta))
}

#' @export
#' @title Calculate failure criterion for a given set of frictional coefficients
#' @description Calculate failure criterion for a given set of frictional coefficients
#' @inheritParams StressRatio
#' @param verbose logical; should the optimal parameters be printed?
#' @return An object with class \code{"sibson.fric"}
#' @seealso \code{\link{StressRatio}} and \code{\link{Stability}}; \code{\link{plot.sibson.fric}}
#' @examples
#' # Use default values (Byerlee's law)
#' S <- Sibson.fric()
#' print(str(S))
#'
#' # Use your own values
#' S <- Sibson.fric(0.5)
#' # and multiple values
#' S <- Sibson.fric(c(0.15,0.6))
#'
#' # Plot the result
#' plot(S)
Sibson.fric <- function(mu, theta, thresh, verbose=TRUE){

  if (missing(mu)) mu <- 0.6
  if (missing(thresh)) thresh <- 20
  if (missing(theta)) theta <- seq(0,90,length.out=1001)

  mu <- as.vector(mu)
  mu.names <- sprintf("mu[S]==%s", mu)
  nmu <- length(mu)
  mu <- matrix(mu, 1)

  R <- apply(mu, 2, StressRatio, theta=theta, thresh=thresh)
  colnames(R) <- mu.names

  opts <- apply(mu, 2, Stability)
  colnames(opts) <- mu.names
  if (verbose) print(t(opts))

  sib <- list(mu=mu, thresh=thresh, R=R, T=theta, Opt=opts, degrees=TRUE)
  class(sib) <- "sibson.fric"
  return(sib)
}

#' @export
#' @title Plot stability conditions
#' @description Plot conditions for fault stability
#' @method plot sibson.fric
#' @param x An object with class \code{"sibson.fric"}
#' @param add logical; should the information be added to the current device
#' @param opt logical; should guidelines be added?
#' @param opt.col color of the guidelines (if \code{opt==TRUE})
#' @param ... additional paramters sent to the main plotting function(s)
#' @seealso \code{\link{Sibson.fric}}
plot.sibson.fric <- function(x, add=FALSE, opt=TRUE, opt.col='grey', ...){
  if (!add){
    plot(0, col=NA, ylim=c(-10,15), xlim=c(-5,99), xaxs='i', xaxt='n', frame=FALSE, xlab='', ylab=expression(R))
    segments(0,0,90,0)
    ticks <- seq(0,90,by=10)
    segments(ticks, 0.15, ticks, -0.15)
    text(ticks, 1.2*as.numeric(ticks > 50)-0.6, as.character(ticks), cex=0.8)
    text(max(ticks)+1, 0, expression(theta), pos=4)
  }
  mus <- x[['mu']]
  thresh <- x[['thresh']]
  opts <- x[['Opt']]
  to <- opts['theta',]
  to2 <- opts['2*theta',]
  ro <- opts['R.opt',]
  if (opt){
    # asymptotes
    segments(to2, -thresh, to2, thresh, lty=3, col=opt.col)
    # optimal R
    segments(0, ro, to2, ro, lty=2, col=opt.col)
    # optimal angle
    segments(to, 0, to, ro, lty=5, col=opt.col)
    text(to, ro, parse(text=sprintf("(list(theta[R]==%s, hat(R)==%s))", round(to,1), round(ro,1))), col=opt.col, pos=3, cex=0.8)
    text(to, -2.2, parse(text=sprintf("bold(mu[S]==%s)", mus)), col=opt.col, pos=1, cex=0.8, srt=90)
  }
  graphics::matplot(x[['T']], x[['R']], type='l', add=TRUE, ...)

}

#' @export
#' @rdname plot.sibson.fric
#' @method lines sibson.fric
lines.sibson.fric <- function(x, ...){
  plot(x, add=TRUE, ...)
}

#' @export
#' @title Plot optimal angles and stress ratios for fault failure
#' @description Plot optimal angles and stress ratios for fault failure
#' @details  Plots the variation in the optimum reactivation angle and minimum
#' (positive) stress ratio for reactivation as a function of friction.
#' Note that the stress ratio if calculated with \code{\link{StressRatio}}.
#' @param x object to plot
#' @inheritParams plot.sibson.fric
#' @seealso \code{\link{Sibson.fric}}
#' @examples
#' optplot() # Reproduces Sibson (1985; Figure 2) by default
optplot <- function(x, ...) UseMethod("optplot")
#' @export
#' @method optplot default
optplot.default <- function(x, ...){
  if (missing(x)) x <- Sibson.fric(seq(0,1,by=0.05), verbose=FALSE)
  stopifnot(inherits(x, 'sibson.fric'))
  optplot(x, ...)
}
#' @export
#' @method optplot sibson.fric
optplot.sibson.fric <- function(x, add=FALSE, opt=FALSE, opt.col='grey', ...){

  mus <- x[['mu']]
  thresh <- x[['thresh']]
  opts <- x[['Opt']]
  to <- opts['theta',]
  to2 <- opts['2*theta',]
  ro <- opts['R.opt',]

  layout(matrix(1:2), heights=c(1.5,1))
  par(mar=c(0.1,3,1,1), las=1, mgp=c(1.7,0.3,0), tcl=0.3)

  plot(0, col=NA, ylim=c(0,90), xlim=c(0,1), yaxs="i", xaxs='i', xaxt='n', frame=FALSE, xlab='', ylab=expression(theta))
  axis(1, labels=FALSE)
  axis(2, at=seq(10,90,by=20), tcl=0.2, labels=FALSE)
  box()
  if (opt){
    # asymptotes
    segments(to2, -thresh, to2, thresh, lty=3, col=opt.col)
    # optimal R
    segments(0, ro, to2, ro, lty=2, col=opt.col)
    # optimal angle
    segments(to, 0, to, ro, lty=5, col=opt.col)
    text(to, ro, parse(text=sprintf("(list(theta[R]==%s, hat(R)==%s))", round(to,1), round(ro,1))), col=opt.col, pos=3, cex=0.8)
    text(to, -3, parse(text=sprintf("bold(mu[S]==%s)", mus)), col=opt.col, pos=1, cex=0.8, srt=90)
  }
  lines(mus, to, ...)
  #segments(0.1, min(to2), 0.9, min(to2), lty=2)
  lines(mus, to2, ...)
  text(c(0.7,0.6), c(mean(to), mean(to2)), c(expression(theta[R]),expression(2*theta[R])))

  par(mar=c(3,3,0.1,1))
  plot(0, col=NA, ylim=c(0,5.99), xlim=c(0,1), xaxs='i', yaxs='i', xlab=expression(mu[S]), ylab=expression(R))
  if (opt){
    # asymptotes
    segments(to2, -thresh, to2, thresh, lty=3, col=opt.col)
    # optimal R
    segments(0, ro, to2, ro, lty=2, col=opt.col)
    # optimal angle
    segments(to, 0, to, ro, lty=5, col=opt.col)
    text(to, ro, parse(text=sprintf("(list(theta[R]==%s, hat(R)==%s))", round(to,1), round(ro,1))), col=opt.col, pos=3, cex=0.8)
    text(to, -3, parse(text=sprintf("bold(mu[S]==%s)", mus)), col=opt.col, pos=1, cex=0.8, srt=90)
  }
  lines(mus, ro, ...)
  text(0.4, mean(ro), expression(hat(R)))

  layout(matrix(1))
}

#' @export
#' @title Plot friction-estimation curves with uncertainty
#' @description Plot friction-estimation curves with uncertainty
#' @param dT numeric; the uncertainty in the lockup angle(s)
#' @param Angs numeric; lockup angle(s)
#' @param Sib. \code{"sibson.fric"} object
#' @param error.thresh numeric; the threshold of error estimates (in percent) to clip figure to
#' @seealso \code{\link{Sibson.fric}} and \code{\link{Mu}}
#' @return A data.frame of the values shown (or not shown, depending on \code{thresh}), invisibly
#' @examples
#' fricplot()
fricplot <- function(dT=1, Angs, Sib., error.thresh=100){

  if (missing(Angs)) Angs <-  seq(45,90,length.out=1001)
  if (missing(Sib.)) Sib. <- Sibson.fric(seq(0,1,by=0.05), verbose=FALSE)

  mus <- Mu(Angs, delta.theta=dT)

  xl <- c(45,90)
  thresh <- error.thresh

  opts <- cbind(Mu=t(Sib.[['mu']]), as.data.frame(t(Sib.[['Opt']])))
  Df <- as.data.frame(t(mus))

  layout(matrix(1:2), heights=c(1.5,1))
  par(las=1, mgp=c(1.7,0.3,0), tcl=0.3, lend='square')

  par(mar=c(0.1,3,1,1))
  ymin <- 0
  plot(0, col=NA, xlim=xl, ylim=c(ymin,1), yaxs='i', xaxs='i', axes=FALSE, frame=FALSE,
       xlab='', ylab=expression(mu[S]))
  with(Df, {
    X <- c(theta, rev(theta))
    Y <- c(pmax(mu - delta.mu, 0), rev(pmin(mu + delta.mu, 1)))
    Y2 <- c(pmax(mu - 2*delta.mu, 0), rev(pmin(mu + 2*delta.mu, 1)))
    polygon(X, Y, col='grey90', border=NA)
    polygon(X, Y2, col=NA, border='grey', lty=2)
  })
  lines(Mu ~ `2*theta`, opts)
  legend('bottomleft', c("Estimate",expression(1*sigma),expression(2*sigma)), lty=c(1,1,2), lwd=c(1.5,8,1.5), col=c(1,'grey','grey'), title=parse(text=sprintf("delta * theta[L] == %s*degree", dT)), bty='n')
  axis(1, labels=FALSE)
  axis(2, at=seq(0.1,1,by=0.1))
  box()

  par(mar=c(3,3,0.1,1))
  ymin <- 1
  plot(0, 1, xlim=xl, ylim=c(ymin, thresh), col=NA, log='y', xaxs='i', yaxs='i',
       xlab=expression(theta[L] == 2*theta[R]), ylab=expression(delta * mu[S] / mu[S] ~~ "[%]"),
       axes=FALSE)
  with(Df, {
    relerr <- 100*delta.mu/mu
    relerr[relerr > thresh] <- thresh
    X <- c(theta, max(xl), min(xl), min(xl))
    Y <- c(relerr, ymin, ymin, min(relerr))
    Y2 <- c(2*relerr, ymin, ymin, 2*min(relerr))
    polygon(X, Y, col='grey90', border=NA)
    polygon(X, Y2, col=NA, border='grey', lty=2)
  })
  axis(1)
  axis(2)
  box()

  layout(matrix(1))

  return(invisible(Df))
}

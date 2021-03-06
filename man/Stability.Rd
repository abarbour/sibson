% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/friction.R
\name{Stability}
\alias{Stability}
\alias{.StressRatio.opt}
\alias{.Theta.opt}
\title{Calculate stability conditions for a frictional coefficient}
\usage{
Stability(mu, ...)

.StressRatio.opt(mu)

.Theta.opt(mu, in.deg = TRUE)
}
\arguments{
\item{mu}{numeric; the static coefficient(s) of friction}

\item{...}{additional parameter sent to\code{\link{.StressRatio.opt}} and \code{\link{.Theta.opt}}}

\item{in.deg}{logical; is \code{theta} given in degrees?}
}
\description{
Calculate stability conditions for a frictional coefficient
}
\seealso{
\code{\link{Sibson.fric}}
}

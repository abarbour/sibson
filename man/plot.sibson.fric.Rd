% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/friction.R
\name{plot.sibson.fric}
\alias{plot.sibson.fric}
\alias{lines.sibson.fric}
\title{Plot stability conditions}
\usage{
\method{plot}{sibson.fric}(x, add = FALSE, opt = TRUE, opt.col = "grey",
  ...)

\method{lines}{sibson.fric}(x, ...)
}
\arguments{
\item{x}{An object with class \code{"sibson.fric"}}

\item{add}{logical; should the information be added to the current device}

\item{opt}{logical; should guidelines be added?}

\item{opt.col}{color of the guidelines (if \code{opt==TRUE})}

\item{...}{additional paramters sent to the main plotting function(s)}
}
\description{
Plot conditions for fault stability
}
\seealso{
\code{\link{Sibson.fric}}
}

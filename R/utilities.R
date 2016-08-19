#' @title Utility functions
#' @description Various utility functions
#' @name Utilities
#' @rdname sib.utils
#' @param x vector of values
#' @param lim numeric; the limit for sawtoothin'
#' @param rad numeric; angle in radians
lower.saw <- function(x, lim=min(x, na.rm=TRUE)){
  # reduce a vector to a sawtooth
  #lim <- abs(lim)
  while (any(x < lim)){
    message('lower saw...')
    inds <- x < lim
    x[inds] <- x[inds] + lim
  }
  return(x)
}
#' @rdname sib.utils
upper.saw <- function(x, lim=max(x, na.rm=TRUE)){
  # reduce a vector to a sawtooth
  #lim <- abs(lim)
  while (any(x > lim)){
    message('upper...')
    inds <- x >= lim
    x[inds] <- x[inds] - lim
  }
  return(x)
}
#' @rdname sib.utils
cotan <- function(rad){
  1/tan(rad)
}

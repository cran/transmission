################################################################################
# Copyright 2011 University of Utah
# This file constitutes portions of ongoing research.
# NOT FOR EXTERNAL DISTRIBUTION
#
# PROJECT INFO
# Project Name: Epicenter, Transmission Modeling - transsim package
# File Author: Andrew Redd
# Date: 11/17/2011
#
# Description of File:
# Time formatting
#
################################################################################

#' formats proc_time as a "hh:mm:ss" time string
#' 
#' @param x number of seconds.
#' 
#' @seealso print.proc_time, proc.time
#' @export
formatTime <- function(x){
  x <- as.numeric(x)
  h <- x %/% 3600L
  m <- (x %% 3600L) %/%60L
  s <- x %% 60L
  ifelse(is.na(x), NA, sprintf("%02d:%02d:%2.2f",h,m,s))
}

#' Prints out time in an intelligent manner.
#' @method print proc_time
#' @param x object of class proc_time
#' @param ... ignored
#' 
#' @seealso system.time proc.time
#' @export print.proc_time
print.proc_time<-function(x, ...){
  times <- formatTime(x)
  names(times) <- names(x)
  args<-list(...)
  if (!("quote" %in% names(args))) args$quote=FALSE
  args$x=times[!is.na(times) & (c(T,T,T,F,F) | x>0)]
  do.call(print,args)
  invisible(x)
}

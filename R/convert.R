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
# Conversion function for aluns java format
# Also function for coverting to the format expected by cooper's code.
#
################################################################################

#' converts to format for Alun's raw Java program.
#' 
#' @param data data to convert from or to.  See details.
#' @param .include.infection.time  include infection time or not.
#' 
#' @rdname convert
.convertForAlun <-
function(data, .include.infection.time=TRUE){
  pid<-admit<-discharge<-infection<-NULL
  pat.columns <- c('pid','admit','discharge','infection')
  patients <- mutate(data$patients[, pat.columns], infection = ifelse(is.na(infection), discharge, infection))
	p<-melt(
    patients
    ,'pid')
	p<-transform(p, 
               event = factor(as.character(variable), levels= event.levels),
               time = value)[,c('time','pid','event')]
  p<-subset(p, !is.na(time))
	s<-transform(data$tests, event = factor(ifelse(result,'positive','negative'), 
              levels=event.levels))[,c('time','pid','event')]
	x<-rbind(p,s)
	x<-transform(arrange(x, pid, time, event), event=as.integer(event)-1L)
  if (.include.infection.time) {
    return(x)
  } else {
    return(subset(x, event <4))
  }
}

#' converts data from Alun's flat format.
#'
#' converts from Alun's flat format.  \code{data} should be of the form
#' time, patient id, event.  where event can be a factor with levels equal to 
#' \code{\link{event.levels}} or an integer ranging from 0 to 4.  The 
#' integers are index of the \code{\link{event.levels}} minus one.
#'
#' @section Details:
#' converts from the flat file format that Alun uses in the standalone Java 
#' programs. 
#' 
#' @rdname convert
.convertFromAlun <- function(data){
  names(data) <- c('time', 'pid', 'event')
  data$event <- factor(data$event)
  levels(data$event) <- event.levels
  list(
    patients = reshape2::dcast(
      subset(data, event %in% c('admit', 'discharge', 'infection')), 
      pid ~ event, value.var = "time"),
    tests = mutate(
      subset(data, event %in% c('negative', 'positive')),
      result = (event=='positive'))[, c('pid', 'time', 'result')]
  )
}

#' event levels for consistency accross functions.
event.levels <- c('admit','negative','positive','discharge','infection')



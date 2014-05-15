################################################################################
# Copyright 2011 University of Utah
# This file constitutes portions of ongoing research.
# NOT FOR EXTERNAL DISTRIBUTION
#
# PROJECT INFO
# Project Name: Epicenter, Transmission Modeling - transmission package
# File Author: Andrew Redd
# Date: 7/12/2012
#
# Description of File:
# Utilities for handling the MCMC chains.
#
################################################################################

#' Create a plot of the chains of parameters.
#' 
#' @param run output from run method of a \code{\link{cont.inf.model}} object.
#' @param vars variables to plot.
#'
#' @keywords plot
#' @export
mcmc_chain_plot <- function(run, vars=c('transmission', 'importation', 'false.neg')){
    .n <- transmission <- NULL
    run <- transform(run, .n=seq_along(transmission))
    g <- ggplot(data=run, aes(x=.n))
    l <- sapply(sapply(vars, .mcmc_make_names) , geom_line)
    Reduce(`+`, l, g) + labs(colour = 'Variable', x='MCMC Chain Index', y=NULL)
}

#' @import ggplot2
.mcmc_make_names <- 
function(vars=c('transmission', 'importation', 'false.neg')){
    lapply(vars, function(name){
        name=force(name)
        structure(c( eval(substitute(aes(col=name)))
                   , aes_string(y=name)), class='uneval')
    })
}

#' Compute the total number of patient days.
#' 
#' @param patients a data frame with columns admission and discharge
#' @param cpp a C++ referenc object of class \code{\link{cont.inf.model}}
#' 
#' @export
compute_patient_days <- function(patients=.patients_from_events(cpp$events)
                                 , cpp){
    sum(patients$discharge - patients$admission)    
}


#' @name iutils
#' @title Internal Utilities
#' Recast event data to a patient data frame.
#' @importFrom reshape2 dcast melt
#' @keywords internal
.patients_from_events <- function(events){
    pid <- event <- NULL
    e2 <- subset(events, event %in% c(0, 7, 8))
    rename(dcast(e2, pid~event, value.var='time'), .event.to.patient.map)
}

#' @rdname iutils
.tests_from_events <- function(events){
    e2 <- subset(events, events$event %in% c(2, 3))
    rename(dcast(e2, pid~event, value.var='time'), .event.to.patient.map)    
}

#' @rdname iutils
.event.to.patient.map <- c(
    '0' = 'admission'
    , '1' = 'negative.test'    
    , '2' = 'positive.test'
    , '3' = 'discharge'
    , '4' = 'infection'
)

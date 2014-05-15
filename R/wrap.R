################################################################################
# Copyright 2011 University of Utah
# This file constitutes portions of ongoing research.
# NOT FOR EXTERNAL DISTRIBUTION
#
# PROJECT INFO
# Project Name: Epicenter, Transmission Modeling - transmission package
# File Author: Andrew Redd
# Date: 11/17/2011
#
# Description of File:
# Wrap Rcpp classes
#
################################################################################

#' Continuous Time Infectious Disease Modeling on Individuals
#' @rdname continuous
#' @name cont.inf.model
#' @aliases cont.inf.model-class
#'
# @usage cont.inf.model$new(df)
#        cont.inf.model$new(patients, tests)
#' @param df an event formatted data.frame.
#' @param patients an data frame of patient information with admission, discharge
#'                 and optionally infection times
#' @param tests    an data frame of test information with test time, patient id, and result.
#'
#' @field run(n)                   run the mcmc chain for n steps.
#' @field runll(n)                 run the mcmc chain for n steps with returning the log likelihood included.
#' @field load <- by <- events(df) load data by events
#' @field load(patients, tests)    load by patients and tests as separate data frames.
#' @field load_by_list(list)       load data with a list containing patient and test data frames.
#'
#' @field events	           Retrieve a list of the events
#' @field nEvents                  Number of events
#' @field nPatients                Number of patients
#' @field using_slope              are we using a slope model or not.
#' @field transmission             The transmission parameter
#' @field importation              The probability of a patient entering infected.
#' @field false.neg                The probability of a test being a false negative.
#' @field false.pos                The probability of a test being a false positive.
#' @field logLik                   The log Likelihood
#' @field nPatients                The number of patients in the population
#' @field nSamples                 The number of Samples
#' @field doStates                 Take a step updating states
#' @field doTransmission           Take a transmission update step
#' @field doImportation            Take an importation update step
#' @field doFN	                   Take a false negative update step
#' @field doFP                     Take a false positive update step
#' @field doSlope	           Take a slope update step.
#'
#' @family mcmc
#' @seealso \code{\link{doSim}} for simulating data.
#' @export cont.inf.model
NULL


#' Transmission Modeling MCMC
#' 
#' Run transmission modeling markov chain monte carlo.
#' This is a convenience function for setting the starting parameters
#' then running the mcmc.
#' 
#' @param N The number of iterations to run the MCMC
#' @param patients The patients data frame
#' @param tests The test data frame
#' @param transmission The starting point for the transmission parameter
#' @param importation The starting point for the importation parameter
#' @param fp The starting point for the false positive parameter
#' @param fn The starting point for the false negative parameter
#' @param prop.sigma The proposal standard deviation for transmission
#' @param max should the maximization algorithm be used.
#' @param with.object  should the C++ reference object be returned with the data.
#' 
#' @family mcmc
#' @export
tmMCMC <- function(N, patients, tests
                  , transmission = 0.001
                  , importation  = 0.10
                  , fp           = 0
                  , fn           = 0.20
                  , prop.sigma   = 0.1
                  , max = FALSE
                  , with.object = FALSE){
    loadModule('continuous')
    mcmc <- new(cont.inf.model, patients, tests)
    mcmc$transmission  = transmission
    mcmc$importation   = importation 
    mcmc$fp            = fp          
    mcmc$fn            = fn          
    run <- mcmc$run(N, max)
    if (with.object) structure(run, object = mcmc) else run
}




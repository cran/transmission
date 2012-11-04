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
# Wraps C++ functions for generating simulation and converting between formats.
#
################################################################################

#' wraps C++ sim class for generating simulations.
#'
#' @param capacity the capacity of the hospital
#' @param transmission the transmission rate
#' @param importation the probability of being infected when admitted
#' @param test_rate the rate of testing in the hospital in mean tests/day.
#' @param stay the mean stay time for the hospital
#' @param length the number of days to run the simulation for
#' @param balance the long run average percent of capacity filled
#' @param fn the probability of a false negative test
#' @param fp the probability of a false positive test
#'
#' @section Details:
#'  The simulation is conducted in continuous time as a competing risks model.
#'  The rates for Admission, infection, discharge and testing compete and
#'  are each modeled as exponential wait times.  This yields the correct
#'  marginal distributions.
#'
#'  \strong{Test Rate}  the test rate is expressed as the number of tests per
#'  day.  This is independent of the number of patients present.  Higher number
#'  of patients will receive less frequent tests.  For each test applied the
#'  test has \code{fn} probability of failing to detect the disease, and
#'  a \code{fp} probability of showing a false positive test.
#'
#'  \strong{Balance}  The balance determines the long run mean percentage of
#'  capacity.  It balances the admission rate to the discharge rate and
#'  capacity.  Discharge rate is determined by the mean stay time.
#'
#'  \strong{Infection Model} The \code{importation} parameter specifies
#'  the probability that an individual is infected with the disease
#'  on admission to the ward.  Cross infections happen between a susceptible
#'  and an infected patien at a transmission rate of
#'  \eqn{\lambda=}{lambda=}\code{transmission}.  For any individual susceptible 
#'  patient the time to infection is distributed exponential with mean time of
#'  \eqn{\lambda N_I}{\lambda*Ni} where \eqn{N_I}{Ni} denotes the total 
#'  number of infected present.
#'  The only vehicles for infection are inportation and cross-infection.
#'
#'
#' @return newSimulator returns A C++ Reference class.
#' @seealso \code{\link[Rcpp:C++Class-class]{Rcpp internal C++ class}}
#' @family simulation
#' @rdname simulation
#' @export
#' @examples
#'    sim <- newSimulator(capacity=100, balance=.75, stay=5.5, test_rate=7.5,
#'      transmission=0.01, importation=0.05, fn=0.05, fp=0)
#'    sim
#'    str(sim)
#'    sim$run(length=100)
#'    sim$finalize()
#'    simdata <- sim$getData()
#'    str(simdata)
#'    lapply(simdata, summary)
#'    rm(sim)
newSimulator<-function(capacity, transmission, importation, test_rate, stay,
                       length=0, balance=1, fn=0, fp=0){
  loadModule("sim")
  s<-new(sim2, capacity)
  s$transmission<- transmission
  s$importation<- importation
  s$test_rate<-test_rate
  s$mean_stay<-stay
  s$fp<-fp
  s$fn<-fn
  s$balance<-balance
  s$run(length)
  s
}

#' doSim is a convenience function for generating and running a simulation
#'
#' @inheritParams newSimulator
#' @param .finish Should all patients have a discharge time, and times strictly within length?
#' @param ... passed onto newSimulator
#'
#' @return \code{doSim} returns a list of two data.frames, patients, and tests.
#' @family simulation
#' @rdname simulation
#' @export
doSim<-function(capacity, transmission, importation, test_rate, stay, length, ..., .finish=TRUE){
  data <- structure(
    newSimulator(capacity, transmission, importation, test_rate, stay, length, ...)$getData(),
	  simparams = c( capacity=capacity, transmission=transmission, importation=importation,
                   test_rate=test_rate, stay=stay, length=length))
  if (.finish) {
    data$patients <- subset(data$patients, admit < length)
    data$tests <- subset(data$tests, time < length)
    data$patients <- within(data$patients, {
      pid <- seq_along(admit)
      infection[infection > length] <- NA
      discharge[(discharge > length) | is.na(discharge)] <- length
    })[, c(4,1,2,3)]
  }
  return(data)
}

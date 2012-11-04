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
# metrics.R
# Compute the metrics for comparing against resl world data.
#
################################################################################

#' Compute metrics on simulated disease transmission data.
#' @rdname metrics
#' @param patients the full patient info data.frame
#'
#' the patients days and patient days at risk
#' 
#' @details
#'  The patient days are computed as the sum of the time patients are in the hospital
#'  The patient days at risk are computed as the sum of the time patients are uninfected.
#'  Patients who are poistive upon admission do not add anything to the patient days at risk
#'  Patients who aqcuire in the hospital add the time from admission to infect to the patient 
#'  days at risk.
#'  
#'  The \code{patients} data.frame must have the columns
#'  \enumerate{
#'    \item pid the patient id
#'    \item admit the admission time
#'    \item discharge the discharge time
#'    \item infection the infection time: missing is never infected, equal to admit implies 
#'                    importation, and anything else is the actual infection time and must 
#'                    be between admit and discharge.
#'  }
#'  This format is the exact same that is the result from \code{\link{doSim}} for \code{$patients}.
#'  
#' @return a vector of patients days, and patients days at risk.
#' @export
computePatientDays <-
function(patients){
  admit     <- patients$admit
  discharge <- patients$discharge
  case      <- !is.na(patients$infection)
  infection <- ifelse(case, patients$infection, patients$discharge)
  
  hospital.days <- discharge - admit
  atrisk.days   <- infection - admit
  
  c(hospital = sum(hospital.days), atrisk = sum(atrisk.days))
}

#' Count infections
#' @rdname metrics
#' @export
countInfections <- 
function(patients){
  with(patients,
    sum( !(is.na(infection)) & (infection != admit) ))
}

#' Compute infection per patient days at risk
#' @param sim see \code{\link{doSim}}
#' @export
computeIPPD <-
function(sim){c(ippd=
  countInfections(sim$patients) /
  computePatientDays(sim$patients)[2]
)}

#' Compute the average incidence rate
#' 
#' The incidence rate is the average number of infections present in the hosiptal.
#' I compute it as (total infected patient days)/(total hospital patient days)
#' 
#' @param sim see \code{\link{doSim}}
#' @export
computeIncidence <-
function(sim){
  admit     <- sim$patients$admit
  discharge <- sim$patients$discharge
  case      <- !is.na(sim$patients$infection)
  infection <- ifelse(case, sim$patients$infection, sim$patients$discharge)
  sum(discharge-infection)/sum(discharge-admit)
}
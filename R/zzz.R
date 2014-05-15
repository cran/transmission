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
# Load dependencies
#
################################################################################

#'  @name transmission
#'  @rdname transmission-package
#'  @title Infectious Disease Transmission Modeling on Individuals
#'
#'  @description
#'  The transmission package provides methods for simulating data from a transmission
#'  model in continuous time.
#'  
#'  @details
#'  The model is fit through a continuous time MCMC estimation.
#'  
#'  
#'  @author
#'  Andrew Redd \email{Andrew.Redd@@hsc.utah.edu},
#'  Alun Thomas \email{alun.thomas@@utah.edu},
#'  Karim Khadar \email{Karim.Khader@@hsc.utah.edu}
#'  
#'  Maintainer: Andrew Redd \email{Andrew.Redd@@hsc.utah.edu}
#'  
#'  
#'  @seealso see \code{\link{doSim}} for simulating data.
#'  @docType package
#'  @keywords package
#' @import Rcpp
#' @import methods
#' @useDynLib transmission
#' @exportPattern ^_rcpp_module_[:alpha:]+
NULL

loadModule('continuous', T)
loadModule('sim', T)

# Global variable access protection.
event <-
value <-
result <-
pid <-
variable <- 
cpresent.change <- 
cpresent <- 
cinfected.change <- 
cinfected <- 
.id <-
counts.for <-
admit <- 
infection <-
event.type <- 
a <- 
b <- try(stop("attempt to acces variable out of scope"), silent=T)

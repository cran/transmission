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

#'  Transmission Model Simulation and Fitting in Continuous Time for Small Populations
#'
#'  The transmission package provides methods for simulating data from a transmission
#'  model in continuous time.
#'  
#'  @author
#'  Andrew Redd \email{Andrew.Redd@@hsc.utah.edu},
#'  Alun Thomas \email{alun.thomas@@utah.edu},
#'  Karim Khadar \email{Karim.Khader@@hsc.utah.edu}
#'  
#'  Maintainer: Andrew Redd \email{Andrew.Redd@@hsc.utah.edu}
#'  
#'  
#'  @name transmission
#'  @rdname transmission-package
#'  @docType package
#'  @title Infectious Disease Transmission Modeling on Individuals
#'  @keywords package
#'  @seealso see \code{\link{doSim}} for simulating data.
NULL

#' Loading function
#' @param pkgname the package name
#' @seealso \code{\link{.onAttach}}
#' @rdname utils
#' 
#' @import Rcpp
#' @import methods
#' @useDynLib transmission
#' @param libname the library name
#' @exportPattern ^_rcpp_module_[:alpha:]+
#' @export cont.inf.model
.onLoad <- function(libname, pkgname){
   # require("methods", character.only=TRUE, quietly=TRUE)
  stopifnot("package:methods" %in% search())
    loadModule('continuous', T)
    loadModule('sim', T)
}

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

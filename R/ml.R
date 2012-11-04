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
# ml.R
# Compute Maximum Likelihood and ML estimates.
# Contains function for computing maximum likelihood on continuous transmission data.
# Modified from Karim Khader's Code.
#
################################################################################



if(FALSE){# For Testing, remove when finished.
library(reshape2)
library(transsim)
library(plyr)
library(reshape2)
{ #Parameters
  capacity<-30
  transmission<-0.05
  length<-365*3
  importation<-0.1
  balance<-.8
  test_rate <- 4
  stay <- 5
}
rtdata<-doSim(capacity, transmission, importation, test_rate, stay, length, balance=balance)
attach(rtdata)
patient.vars = .(
  Admissions  = admit,
  Discharges  = discharge,
  Importation = (!is.na(infection)) & (infection==admit),
  AcqTimes    = infection)

Vswitch(d0$event.type, infection=1, discharge=-1,0)->x
}

#' Vectorized switch
#'
#' @param EXPR the expression to vectorize over.
#' @param ... passed onto switch for cases evaluation.
#'
#' @seealso \code{\link{switch}}
Vswitch<-Vectorize(function(EXPR,...){switch(EXPR,...)}, "EXPR")

#' Variables for defaults
#'
#' @rdname variables
sim.patient.vars<-plyr::.(
	Admissions  = admit,
	Discharges  = ifelse(is.na(discharge), max(discharge,admit,infection), discharge),
	Importation = ifelse( is.na(infection), F, infection==admit),
	Acquisition = ifelse( is.na(infection), F, infection!=admit),
	AcqTimes    = infection)
#' @rdname variables
sim.test.vars    <-plyr::.( PatientID   = pid, TestResult  = result, TestTime    = time)

#' @title Compute Maximum Likelihood Estimates
#'
#' @param patients data.frame for patient data
#' @param tests data.frame for testing data
#' @param patient.vars mapping for patient variables.  See DETAILS.
#' @param test.vars mapping for test variables. See DETAILS.
#'
#' \code{patient.vars} and \code{test.vars}  contain mappings for the variables
#' in patients and tests, respectively. they should be formed
#' with \code{\link[plyr:quoted]{.(name = data.expr)}}, where \code{name} is one
#' of the required variable names, and \code{data.expr} is variable name or
#' expression to be evaluated in the appropriate data frame.
#'
#' For patient.vars the required names are: Admissions, Discharges, Importation
#' Acquisition, and AcqTimes.  Admissions, Discharges, and AcqTimes are numeric
#' time values.  Importation and Acquisition are logical variables.
#'
#' For test.vars the required names are: TestResult, PatientID, and TestTime.
#' TestResult is a logical for status of the test, TRUE indicated positive,
#' FALSE indicates negative.
#' PatientID must correspond to the row in patients data.
#' TestTime is numeric and indicates the time of the test.
#'
#' @return named numeric vector for maximum likelihood estimates.  \
#'   The names are Import.mle , Sensitivity.mle, Transmission.mle
#'
#' Estimates the maximum likelihood estimates
#' for Importation, Transmission, and Sensitivity
#' given the full data.
#' @export
#' @seealso \code{\link[plyr:quoted]{.()}}
#' @family likelihoods
MaximumLikelihoodEstimates <- function(patients, tests, patient.vars=sim.patient.vars, test.vars=sim.test.vars){
  {# create PatientInfo
		Admissions <- if("Admissions"  %in% names(patient.vars)) eval(patient.vars[["Admissions"]],patients)  else if("Admissions"  %in% names(patients)) patients$Admissions  else stop("Could not find Admissions")
		Discharges <- if("Discharges"  %in% names(patient.vars)) eval(patient.vars[["Discharges"]],patients)  else if("Discharges"  %in% names(patients)) patients$Discharges  else stop("Could not find Discharges")
		Importation<- if("Importation" %in% names(patient.vars)) eval(patient.vars[["Importation"]],patients) else if("Importation" %in% names(patients)) patients$Importation else stop("Could not find Importation")
		Acquisition<- if("Acquisition" %in% names(patient.vars)) eval(patient.vars[["Acquisition"]],patients) else if("Acquisition" %in% names(patients)) patients$Acquisition else stop("Could not find Acquisition")
		AcqTimes   <- if("AcqTimes"    %in% names(patient.vars)) eval(patient.vars[["AcqTimes"]],patients)    else if("AcqTimes"    %in% names(patients)) patients$AcqTimes    else stop("Could not find AcqTimes")
		PatientInfo<- data.frame(Admissions, Discharges, Importation, Acquisition, AcqTimes)
	}
	{# create TestInfo
		TestResult <- if("TestResult"  %in% names(test.vars)) eval(test.vars[["TestResult"]],tests)  else if("TestResult"  %in% names(tests)) tests$TestResult  else stop("Could not find TestResult")
		PatientID  <- if("PatientID"   %in% names(test.vars)) eval(test.vars[["PatientID"]],tests)   else if("PatientID"   %in% names(tests)) tests$PatientID   else stop("Could not find PatientID")
		TestTime   <- if("TestTime"    %in% names(test.vars)) eval(test.vars[["TestTime"]],tests)    else if("TestTime"    %in% names(tests)) tests$TestTime    else stop("Could not find TestTime")
		.isPositive<- function(tdf){
			pInfo = PatientInfo[tdf$PatientID,]
			c(Positive=with(pInfo, Importation | (Acquisition & AcqTimes<tdf$TestTime)))
		}
		TestInfo<-ddply(data.frame(.id=seq_along(TestResult), TestResult,PatientID,TestTime),.(.id,TestResult,PatientID,TestTime), .isPositive)
	}

  # stack times identified by event.type
	d0<-subset(patients, !is.na(infection))
	d0<-reshape2::melt(d0,measure.vars = names(patients), na.rm=T)
	d0<-plyr::rename(d0, c(value="time", variable="event.type"))
  d0 <- arrange(d0,time)

  d1 <- transform(d0,
    a=ifelse(event.type=='infection',1,ifelse(event.type=='discharge',-1,0)),
    b=ifelse(event.type=='infection',0,ifelse(event.type=='discharge',-1,1)))
  d2 <- subset(transform(d1, NInfected=cumsum(a), N=cumsum(b)), select=c('time','NInfected','N'))
  d2 <- d2[!duplicated(d2$time,fromLast=T),]


  EventTimes <- sort(unique(c(Admissions, Discharges, AcqTimes[(!is.na(AcqTimes)) & (AcqTimes>0)])))
  NoInfectedPatients <- 0*EventTimes
  NoPatients <- 0*EventTimes
  for (i in 1:length(EventTimes)){
    temp <- PatientInfo[(PatientInfo$Admission<=EventTimes[i])&(PatientInfo$Discharge>EventTimes[i]),]
    NoInfectedPatients[i] <- sum(temp$Importation) + sum(temp$Acquisition[(temp$Acquisition==1)&(temp$AcqTimes<=EventTimes[i])])
    NoPatients[i] <- dim(temp)[1]
  }
  noPatAdmits <- length(Admissions)
  PresentAndUncolonized <- vector('list',length=noPatAdmits)
  ImportIndex <- c(1:noPatAdmits)[PatientInfo$Importation]
  AcqIndex <- c(1:noPatAdmits)[PatientInfo$Acquisition]
  NIIndex <- c(1:noPatAdmits)[(PatientInfo$Importation==0)&(PatientInfo$Acquisition==0)]
  for (i in ImportIndex){
    PresentAndUncolonized[[i]] <- NA
  }
  for (i in AcqIndex){
    PresentAndUncolonized[[i]] <- c((c(1:length(EventTimes))[PatientInfo[i, 'Admissions']==EventTimes]):((c(1:length(EventTimes))[PatientInfo[i, 'AcqTimes']==EventTimes])-1))
  }
  for (i in NIIndex){
    PresentAndUncolonized[[i]] <- c(c((1:length(EventTimes))[PatientInfo[i, 'Admissions']==EventTimes]):(c((1:length(EventTimes))[PatientInfo[i, 'Discharges']==EventTimes])-1))
  }

  PresentAndUncolonizedMatrix <- matrix(0, nrow=noPatAdmits, ncol=(length(EventTimes)-1))
  for (i in 1:noPatAdmits){
    if (length(PresentAndUncolonized[[i]])>1) PresentAndUncolonizedMatrix[i, PresentAndUncolonized[[i]]] <- 1
  }

  NoInfectedPatientsDeltat <- head(NoInfectedPatients, -1)*diff(EventTimes)

  ColonizedIndex <- c(1:noPatAdmits)[PatientInfo$Acquisition==1]
  NIPJBColonization <- 0*ColonizedIndex

  if (length(ColonizedIndex > 0)){
    for (i in 1:length(ColonizedIndex)){
      NIPJBColonization[i] <- NoInfectedPatients[max(PresentAndUncolonized[[ColonizedIndex[i]]])]
    }
  }

  Survival <- PresentAndUncolonizedMatrix*matrix(rep(NoInfectedPatientsDeltat, times=noPatAdmits), nrow=noPatAdmits, byrow=TRUE)
  Survival <- Survival[Survival>0]

  c(
  Import.mle      =sum(Importation)/length(Importation),
  Sensitivity.mle =sum(TestInfo$TestResult[TestInfo$Positive==1])/length(TestInfo$TestResult[TestInfo$Positive==1]),
  Transmission.mle=length(NIPJBColonization)/sum(Survival))
}

#' Compute log-likelihoods for the full data.
#'
#' @param transmission the transmission parameter of the model
#' @param importation the probability of importation
#' @param sensitivity the probability of detecting disease presence
#' @param fn the false negative probability.  Cannot also be
#' @param patients the data for patients
#' @param tests the data for tests
#' @param patient.vars mappings for patients
#' @param test.vars mappings for tests
#'
#' @return a named vector with the components of the likelihood.
#'
#' @seealso \code{\link{MaximumLikelihoodEstimates}} for explanation of
#'          \code{patient.vars} and \code{test.vars}
#' @family likelihood
#' @export
GetFullDataLikelihood <- function(transmission, importation, sensitivity, fn=1-sensitivity,
  patients, tests, patient.vars=sim.patient.vars, test.vars=sim.test.vars){
  if(!missing(sensitivity) && !missing(fn))
    stopifnot(sensitivity==1-fn)
  PatientInfo <- CreatePatientInfo(patients, patient.vars)
  TestInfo    <- CreateTestInfo(tests, PatientInfo, test.vars)
  { # compute importation likelihood
    importation.loglik <- with(PatientInfo,
      sum(ifelse(Importation, log(importation), log(1-importation)))
    )
  }
  { # compute observation likelihood.
    observation.loglik <- sum(daply(TestInfo, .(.id),
      function(TestInfo, PatientInfo) {
        with(TestInfo, {
          with(PatientInfo[PatientID, ],{
            infected = Importation || (Acquisition && AcqTimes < TestTime)
            ifelse(infected, ifelse(TestResult, log(1-fn), log(fn)), 0)
          })
        })
      }
      , PatientInfo=PatientInfo
    ))
  }
  { # Compute transmission likelihood
    y.frame <- ComputeYFrame(PatientInfo)
    # library(ggplot)
    # qplot(time, y, data=y.frame, geom=c('step'))
    { # compute survival portion
      survival.loglik <- -sum(indiv.SurvLL(transmission, PatientInfo, y.frame=y.frame))
    }
    { # compute hazard portion
      hazard.loglik <- sum(log(daply(subset(PatientInfo, PatientInfo$Acquisition), .(.id),
        function(df, y.frame, transmission) {
          with(df, transmission * tail(subset(y.frame, time < AcqTimes),1)$y)
        }
        , y.frame=y.frame, transmission=transmission
      )))
    }
    transmission.loglik <- survival.loglik + hazard.loglik
  }
  c(full         = transmission.loglik + importation.loglik + observation.loglik,
    importation  = importation.loglik,
    observation  = observation.loglik,
    transmission = transmission.loglik,
    survival     = survival.loglik,
    hazard       = hazard.loglik)
}

#' create PatientInfo
#' @aliases PatientInfo
#' @param patients a data.frame or environment
#' @param patient.vars patient variable mappings.  see \code{\link{sim.patient.vars}}
#' @param ... discarded
#' @return CreatePatientInfo returns a data.frame with columns
#'    .id, Admissions, Discharges, Importation, Acquisition, AcqTimes
#' @rdname helpers
CreatePatientInfo <- function(patients, patient.vars=sim.patient.vars, ...){
		Admissions <- if("Admissions"  %in% names(patient.vars)) eval(patient.vars[["Admissions"]],patients)
      else if("Admissions"  %in% names(patients)) patients$Admissions
      else stop("Could not find Admissions")
		Discharges <- if("Discharges"  %in% names(patient.vars)) eval(patient.vars[["Discharges"]],patients)
      else if("Discharges"  %in% names(patients)) patients$Discharges
      else stop("Could not find Discharges")
		Importation<- if("Importation" %in% names(patient.vars)) eval(patient.vars[["Importation"]],patients)
      else if("Importation" %in% names(patients)) patients$Importation
      else stop("Could not find Importation")
		Acquisition<- if("Acquisition" %in% names(patient.vars)) eval(patient.vars[["Acquisition"]],patients)
      else if("Acquisition" %in% names(patients)) patients$Acquisition
      else stop("Could not find Acquisition")
		AcqTimes   <- if("AcqTimes"    %in% names(patient.vars)) eval(patient.vars[["AcqTimes"]],patients)
      else if("AcqTimes"    %in% names(patients)) patients$AcqTimes
      else stop("Could not find AcqTimes")
		PatientInfo <- data.frame(.id=seq_along(Admissions), Admissions, Discharges, Importation,
      Acquisition, AcqTimes)
    structure(
      mutate(PatientInfo, AcqTimes[Importation] <- Admissions[Importation])
      , class=c("PatientInfo", 'data.frame'))
	}

#' create TestInfo
#' @aliases TestInfo
#' @inheritParams CreatePatientInfo
#' @param tests data frame.
#' @param PatientInfo see \code{\link{CreatePatientInfo}}
#' @param test.vars test variable mappings.  see \code{\link{sim.test.vars}}
#' @return CreateTestInfo returns a data frame with columns 
#'                        .id, TestResult, PatientID, TestTime
#' @rdname helpers
CreateTestInfo <- function(tests, PatientInfo, test.vars=sim.test.vars, ...){
  stopifnot(inherits(PatientInfo, 'PatientInfo'))
  TestResult <- if("TestResult"  %in% names(test.vars)) eval(test.vars[["TestResult"]],tests)
    else if("TestResult"  %in% names(tests)) tests$TestResult
    else stop("Could not find TestResult")
  PatientID  <- if("PatientID"   %in% names(test.vars)) eval(test.vars[["PatientID"]],tests)
    else if("PatientID"   %in% names(tests)) tests$PatientID
    else stop("Could not find PatientID")
  TestTime   <- if("TestTime"    %in% names(test.vars)) eval(test.vars[["TestTime"]],tests)
    else if("TestTime"    %in% names(tests)) tests$TestTime
    else stop("Could not find TestTime")
  .isPositive<- function(tdf){
    pInfo = PatientInfo[tdf$PatientID,]
    c(Positive=with(pInfo, Importation | (Acquisition & AcqTimes<tdf$TestTime)))
  }
  structure(
    ddply(data.frame(.id=seq_along(TestResult), TestResult,PatientID,TestTime),
        .(.id,TestResult,PatientID,TestTime),
        .isPositive)
    , class=c("TestInfo", 'data.frame'))
}

#' Compute y(t) the number of infections in the unit at time(t).
#' @return that has the form of a data frame.
#' @rdname helpers
ComputeYFrame <- function(PatientInfo){
  Importation <- Acquisition <- AcqTimes <- Discharges <- counts.y <- counts.z <- NULL
  stopifnot(inherits(PatientInfo, 'PatientInfo'))
  measure.vars = c('Admissions', 'AcqTimes', 'Discharges')
  id.vars = c('.id')
  y.frame <- reshape2:::melt.data.frame(
    mutate(PatientInfo, 
      AcqTimes=ifelse(Importation|Acquisition, AcqTimes, Discharges)
    )[, c(id.vars,measure.vars)]
    , measure.vars = measure.vars, id.vars=id.vars
    , value.name='time', variable.name="event.type", na.rm=T)
  y.frame <- plyr::arrange(y.frame, time, event.type)
  y.counts.for <- c(Admissions = 0, AcqTimes = 1, Discharges = -1)
  y.frame$counts.y = y.counts.for[as.character(y.frame$event.type)]
  z.counts.for <- c(Admissions = 1, AcqTimes = 0, Discharges = -1)
  y.frame$counts.z = z.counts.for[as.character(y.frame$event.type)]
  mutate(y.frame,
    y = cumsum(counts.y),
    z = cumsum(counts.z),
    delta.time = c(diff(time),0)
  )
}

#' Compute individual survival likelihoods
#' @param transmission parameter of the model
#' @param y.frame see ComputeYFrame
#' @rdname helpers
indiv.SurvLL <- function(transmission, PatientInfo, y.frame=ComputeYFrame(PatientInfo)){
  stopifnot(inherits(PatientInfo, 'PatientInfo'))
  daply(PatientInfo, .(.id), function(df, y.frame) with(df, {
    y.part <- subset(y.frame, Admissions <= time & time < min(Discharges, AcqTimes, na.rm=T))
    ifelse(nrow(y.part)==0, 0,
      with(y.part, transmission * crossprod(delta.time,y)))
  }), y.frame=y.frame)
}

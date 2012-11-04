\name{CreatePatientInfo}
\alias{ComputeYFrame}
\alias{CreatePatientInfo}
\alias{CreateTestInfo}
\alias{PatientInfo}
\alias{TestInfo}
\alias{indiv.SurvLL}
\title{create PatientInfo}
\usage{
  CreatePatientInfo(patients,
    patient.vars = sim.patient.vars, ...)

  CreateTestInfo(tests, PatientInfo,
    test.vars = sim.test.vars, ...)

  ComputeYFrame(PatientInfo)

  indiv.SurvLL(transmission, PatientInfo,
    y.frame = ComputeYFrame(PatientInfo))
}
\arguments{
  \item{patients}{a data.frame or environment}

  \item{patient.vars}{patient variable mappings.  see
  \code{\link{sim.patient.vars}}}

  \item{...}{discarded}

  \item{tests}{data frame.}

  \item{PatientInfo}{see \code{\link{CreatePatientInfo}}}

  \item{test.vars}{test variable mappings.  see
  \code{\link{sim.test.vars}}}

  \item{transmission}{parameter of the model}

  \item{y.frame}{see ComputeYFrame}
}
\value{
  CreatePatientInfo returns a data.frame with columns .id,
  Admissions, Discharges, Importation, Acquisition,
  AcqTimes

  CreateTestInfo returns a data frame with columns .id,
  TestResult, PatientID, TestTime

  that has the form of a data frame.
}
\description{
  create PatientInfo

  create TestInfo

  Compute y(t) the number of infections in the unit at
  time(t).

  Compute individual survival likelihoods
}


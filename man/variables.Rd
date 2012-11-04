\docType{data}
\name{sim.patient.vars}
\alias{sim.patient.vars}
\alias{sim.test.vars}
\title{Variables for defaults}
\format{List of 5
 $ Admissions : symbol admit
 $ Discharges : language ifelse(is.na(discharge), max(discharge, admit, infection), discharge)
 $ Importation: language ifelse(is.na(infection), F, infection == admit)
 $ Acquisition: language ifelse(is.na(infection), F, infection != admit)
 $ AcqTimes   : symbol infection
 - attr(*, "env")=<environment: namespace:transmission> 
 - attr(*, "class")= chr "quoted"}
\usage{
  sim.patient.vars

  sim.test.vars
}
\description{
  Variables for defaults
}
\keyword{datasets}


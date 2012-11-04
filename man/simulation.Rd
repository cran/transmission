\name{newSimulator}
\alias{doSim}
\alias{newSimulator}
\title{wraps C++ sim class for generating simulations.}
\usage{
  newSimulator(capacity, transmission, importation,
    test_rate, stay, length = 0, balance = 1, fn = 0,
    fp = 0)

  doSim(capacity, transmission, importation, test_rate,
    stay, length, ..., .finish = TRUE)
}
\arguments{
  \item{capacity}{the capacity of the hospital}

  \item{transmission}{the transmission rate}

  \item{importation}{the probability of being infected when
  admitted}

  \item{test_rate}{the rate of testing in the hospital in
  mean tests/day.}

  \item{stay}{the mean stay time for the hospital}

  \item{length}{the number of days to run the simulation
  for}

  \item{balance}{the long run average percent of capacity
  filled}

  \item{fn}{the probability of a false negative test}

  \item{fp}{the probability of a false positive test}

  \item{.finish}{Should all patients have a discharge time,
  and times strictly within length?}

  \item{...}{passed onto newSimulator}
}
\value{
  newSimulator returns A C++ Reference class.

  \code{doSim} returns a list of two data.frames, patients,
  and tests.
}
\description{
  wraps C++ sim class for generating simulations.

  doSim is a convenience function for generating and
  running a simulation
}
\section{Details}{
  The simulation is conducted in continuous time as a
  competing risks model.  The rates for Admission,
  infection, discharge and testing compete and are each
  modeled as exponential wait times.  This yields the
  correct marginal distributions.

  \strong{Test Rate} the test rate is expressed as the
  number of tests per day.  This is independent of the
  number of patients present.  Higher number of patients
  will receive less frequent tests.  For each test applied
  the test has \code{fn} probability of failing to detect
  the disease, and a \code{fp} probability of showing a
  false positive test.

  \strong{Balance} The balance determines the long run mean
  percentage of capacity.  It balances the admission rate
  to the discharge rate and capacity.  Discharge rate is
  determined by the mean stay time.

  \strong{Infection Model} The \code{importation} parameter
  specifies the probability that an individual is infected
  with the disease on admission to the ward.  Cross
  infections happen between a susceptible and an infected
  patien at a transmission rate of
  \eqn{\lambda=}{lambda=}\code{transmission}.  For any
  individual susceptible patient the time to infection is
  distributed exponential with mean time of \eqn{\lambda
  N_I}{\lambda*Ni} where \eqn{N_I}{Ni} denotes the total
  number of infected present.  The only vehicles for
  infection are inportation and cross-infection.
}
\examples{
sim <- newSimulator(capacity=100, balance=.75, stay=5.5, test_rate=7.5,
     transmission=0.01, importation=0.05, fn=0.05, fp=0)
   sim
   str(sim)
   sim$run(length=100)
   sim$finalize()
   simdata <- sim$getData()
   str(simdata)
   lapply(simdata, summary)
   rm(sim)
}
\seealso{
  \code{\link[Rcpp:C++Class-class]{Rcpp internal C++
  class}}
}


\name{GetFullDataLikelihood}
\alias{GetFullDataLikelihood}
\title{Compute log-likelihoods for the full data.}
\usage{
  GetFullDataLikelihood(transmission, importation,
    sensitivity, fn = 1 - sensitivity, patients, tests,
    patient.vars = sim.patient.vars,
    test.vars = sim.test.vars)
}
\arguments{
  \item{transmission}{the transmission parameter of the
  model}

  \item{importation}{the probability of importation}

  \item{sensitivity}{the probability of detecting disease
  presence}

  \item{fn}{the false negative probability.  Cannot also
  be}

  \item{patients}{the data for patients}

  \item{tests}{the data for tests}

  \item{patient.vars}{mappings for patients}

  \item{test.vars}{mappings for tests}
}
\value{
  a named vector with the components of the likelihood.
}
\description{
  Compute log-likelihoods for the full data.
}
\seealso{
  \code{\link{MaximumLikelihoodEstimates}} for explanation
  of \code{patient.vars} and \code{test.vars}
}


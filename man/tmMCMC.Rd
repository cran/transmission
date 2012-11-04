\name{tmMCMC}
\alias{tmMCMC}
\title{Transmission Modeling MCMC}
\usage{
  tmMCMC(N, patients, tests, transmission = 0.001,
    importation = 0.1, fp = 0, fn = 0.2, prop.sigma = 0.1,
    max = FALSE, with.object = FALSE)
}
\arguments{
  \item{N}{The number of iterations to run the MCMC}

  \item{patients}{The patients data frame}

  \item{tests}{The test data frame}

  \item{transmission}{The starting point for the
  transmission parameter}

  \item{importation}{The starting point for the importation
  parameter}

  \item{fp}{The starting point for the false positive
  parameter}

  \item{fn}{The starting point for the false negative
  parameter}

  \item{prop.sigma}{The proposal standard deviation for
  transmission}

  \item{max}{should the maximization algorithm be used.}

  \item{with.object}{should the C++ reference object be
  returned with the data.}
}
\description{
  Run transmission modeling markov chain monte carlo. This
  is a convenience function for setting the starting
  parameters then running the mcmc.
}
\seealso{
  Other mcmc: \code{\link{cont.inf.model}}
}


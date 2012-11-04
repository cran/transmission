\name{ComputeSurvivalLLViaAlunMethod}
\alias{.ConvertCInfected}
\alias{.ConvertCPresent}
\alias{ComputeSurvivalLLViaAlunMethod}
\title{Compute Survival portion of transmission model log likelihood.}
\usage{
  ComputeSurvivalLLViaAlunMethod(alun.data, transmission)

  .ConvertCPresent(x)

  .ConvertCInfected(x)
}
\arguments{
  \item{alun.data}{is Alun formated data. See Details}

  \item{transmission}{The transmission parameter}

  \item{x}{event either integer, character or factor}
}
\value{
  single numeric for survival portion of log likelihhod.
}
\description{
  Compute Survival portion of transmission model log
  likelihood.

  .ConvertCPresent converts to change in present count

  convert to change in infected count
}
\section{ComputeSurvivalLLViaAlunMethod Details}{
  alun.data is data formatted in Alun's format of an event
  list.  It is a data.frame with three columns of time,
  patien id, and event type.
}

\section{.ConvertCPresent Details}{
  Converts event type to change in number present.
}

\section{.ConvertCInfected Details}{
  Converts event type to change in number infected
}
\seealso{
  ConvertToAlun, ConvertFromAlun
}


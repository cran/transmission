\name{.convertForAlun}
\alias{.convertForAlun}
\alias{.convertFromAlun}
\title{converts to format for Alun's raw Java program.}
\usage{
  .convertForAlun(data, .include.infection.time = TRUE)

  .convertFromAlun(data)
}
\arguments{
  \item{data}{data to convert from or to.  See details.}

  \item{.include.infection.time}{include infection time or
  not.}
}
\description{
  converts to format for Alun's raw Java program.

  converts from Alun's flat format.  \code{data} should be
  of the form time, patient id, event.  where event can be
  a factor with levels equal to \code{\link{event.levels}}
  or an integer ranging from 0 to 4.  The integers are
  index of the \code{\link{event.levels}} minus one.
}
\section{Details}{
  converts from the flat file format that Alun uses in the
  standalone Java programs.
}


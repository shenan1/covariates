% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nDives.R
\name{n.dives}
\alias{n.dives}
\title{Calculate number of dives.}
\usage{
n.dives(
  gldr.time.end,
  pilot.start = 7,
  pilot.end = 22,
  dives.flight = 3,
  seconds.dive = 3600,
  comms = 20
)
}
\arguments{
\item{pilot.start}{time pilot starts work \link{hour}}

\item{pilot.end}{time pilot ends work \link{hour} (24-hour)}

\item{dives.flight}{standard number of dives per flight}

\item{seconds.dive}{number of seconds per dive}

\item{comms}{communication time with glider between flights \link{minutes}}
}
\value{
Number of dives.
}
\description{
Calculate number of dives.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inlaSummary.R
\name{inla.summary}
\alias{inla.summary}
\title{Extract and save fixed effects mean and sd.
Extract and save practical range 0.025 and sd 0.975 quantiles.}
\usage{
inla.summary(mod.mode, n.fixed, l = 1, k, RData, directory, write = FALSE)
}
\arguments{
\item{mod.mode}{result of inla()}

\item{n.fixed}{number of fixed effects}

\item{directory}{output directory}

\item{write}{write the results (TRUE or FALSE)}
}
\value{
Fixed effects mean and sd; range 0.025 and stdev 0.975 quantiles.
}
\description{
Extract and save fixed effects mean and sd.
Extract and save practical range 0.025 and sd 0.975 quantiles.
}
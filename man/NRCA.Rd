% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NRCA.R
\name{NRCA}
\alias{NRCA}
\title{Calculate normalised revealed comparative advantage index (NRCA, Yu et al. 2009)}
\usage{
NRCA(data, binary = F, transform = "regular")
}
\arguments{
\item{data}{An incidence matrix with agent in rows and activities in columns.}

\item{binary}{Logical if index should be binarised with 1 signalling specialisation. Default is non-binary.}

\item{transform}{String indicating whether the index should be transformed to
range between 0 and 2 (in theoretical extremes) instead of -1/4 & 1/4 (default)
scaled between 0 and 1 with actual min and max values (scaled)
be in standard deviations, centered at 1 (std), cut at 0 lower bound}
}
\value{
A sparse matrix (agent x activities) with NRCA values
}
\description{
Calculate NRCA index which is an alternative specialisation index derived from incidence matrices
}

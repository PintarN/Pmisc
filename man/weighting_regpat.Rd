% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighting_regpat.R
\name{weighting_regpat}
\alias{weighting_regpat}
\title{Calculate weights for REGPAT patent data}
\usage{
weighting_regpat(
  data,
  path = "no",
  quality = "no",
  within_cits = "leave",
  periodisation = "prio_year",
  years = "all",
  smallnumber = c("no", "no"),
  smallnumber_count = "non_qu"
)
}
\arguments{
\item{data}{A dataframe with prepared (load.regpat) REGPAT data.}

\item{path}{A vector of paths to OECD citation database files (EP_citation,EP_eqv,WO_citation)
and REGPAT files (EPO and PCT) if within_cits is "drop", or alternatively paths to the saved .rda files, as last entry please give the version YYYYMM}

\item{quality}{Two strings determining if and which quality weight adjustment should be used, defaults to no quality adjustment.
One of those:
"times_cits" for old multiplication of patent weight with number of forward citations
"times_cits1" for old multiplication of patent weight with number of forward citations + 1
"relative_cits" for Pintar (2018) version, cits relative to cohort mean
"relative_cits1" for Pintar (2018) version, cits + 1 relative to cohort mean + 1
and the number of months that should be taken as max citation lag}

\item{within_cits}{Text (leave or drop) to set whether citations from within region should be excluded from the citation count, default is leave in}

\item{periodisation}{Text to set which periodisation should be used. Options are: "prio_year" (default) or "app_year" for forward citation quality adjusted}

\item{years}{Vector of year intervals (like so: c(2000,2012)) used. Default is all years}

\item{smallnumber}{Two strings (corresponding to tech classes and then regions) giving the absolute patent count cutoff or
defining which relative cutoff is to be used."10th_perc" or "20th_perc" for e.g. relative. Techs are cut at level given as input.}

\item{smallnumber_count}{String determining how small techs or regions should be excluded, based on quality or non-quality adjusted (fractional) patent count
Default is non quality adjusted}
}
\value{
A dataframe of REGPAT data with corresponding weights
}
\description{
Calculate (fractional, quality adjusted) weights for REGPAT patent data
}

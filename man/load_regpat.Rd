% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_regpat.R
\name{load_regpat}
\alias{load_regpat}
\title{Load REGPAT data and prepare for further handling}
\usage{
load_regpat(
  path,
  region,
  disc_periphery = "yes",
  regionlist = "no",
  techclass = c("all", "all"),
  years = "all",
  periodisation = "prio_year",
  disc_unclass = "yes"
)
}
\arguments{
\item{path}{Vector of two (three with CPC) paths to two REGPAT text files (year_tech and inventor_region), starting with year_tech. For delim "|" should be used. If CPC classes are used, you also need to give the path to the CPC file (in third element).}

\item{region}{Text or number input to set regionalisation. Options are: "0" - "3" for NUTS regions and "metro" for matching with metro regions by EUROSTAT.}

\item{disc_periphery}{A string indicating if periphery (when metro matching) data is deleted or not, defaults to "yes".}

\item{regionlist}{A list of regions which should be used, all others will be removed. Column name for region codes should be "reg_code. Needs to match with regionalisation. Defaults to "no", meaning all regions are retained.}

\item{techclass}{Three strings indicating which tech class to use (IPC or CPC or Schmoch (2008)), the digit cutoff (or name of classification level (e.g. subclass)) for main tech class (1-8, not used in Schmoch matching) and thirdly the name of level for long tech class (e.g. 'maingroup'). Defaults to no cutting.}

\item{years}{Vector of year intervals used (like so: c(2000,2012). Default is "all" years.}

\item{periodisation}{Text to set which periodisation should be used. Options are: "prio_year" (default) or "app_year" for new naming convention}

\item{disc_unclass}{A string indicating whether unclassified regions should be removed, defaults to "yes".}
}
\value{
A list of dataframes of REGPAT data
}
\description{
Load REGPAT data from text files and creates a merged dataframe with some optional adjustments
}

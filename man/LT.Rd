% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/LT.R
\name{LT}
\alias{LT}
\title{Calculate and plot lactate breakpoint (lactate threshold)}
\usage{
LT(data, workload.column = 1, lactate.column = 2, plot = T)
}
\arguments{
\item{data}{A data frame with work intensity (e.g. watt) and lactate measurements}

\item{workload.column}{An integer specifying the column with work intensity data, default=1.}

\item{lactate.column}{An integer specifying the column with lactate data, default=2.}

\item{plot}{Logical specify whether to add plot or not. The plot shows data points with the model fit}
}
\value{
Work intensity at a specified rise from baseline lactate concentration
}
\description{
Calculates workload at the lactate breakpoint. Based on functions described by Newell et al. 2007 (see reference)
}
\references{
Software for calculating blood lactate endurance markers.
John Newell , David Higgins , Niall Madden , James Cruickshank , Jochen Einbeck , Kenny McMillan , Roddy McDonald.
Journal of Sports Sciences, Vol. 25, Iss. 12, 2007
}


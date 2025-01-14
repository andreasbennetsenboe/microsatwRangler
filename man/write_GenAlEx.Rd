% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_GenAlEx.R
\name{write_GenAlEx}
\alias{write_GenAlEx}
\title{Write a GenAlEx formated .csv file}
\usage{
write_GenAlEx(x, path, lociprefix)
}
\arguments{
\item{x}{A tibble with microsatellite data.}

\item{path}{The desired path for the output file.}

\item{lociprefix}{A character string specifying the first letters shared by all loci in the data used to identify locus columns.}
}
\value{
a .csv file placed in the directory
}
\description{
Create a GenAlEx formated .csv file in the directory from a dataset imported from Geneious.
}

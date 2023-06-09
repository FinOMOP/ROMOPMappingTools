% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importOMOPtables.R
\name{importOMOPtables}
\alias{importOMOPtables}
\title{readOMOPtables}
\usage{
importOMOPtables(path_OMOP_vocabulary_folder, table_names = NULL)
}
\arguments{
\item{path_OMOP_vocabulary_folder}{path to folder with the OMOP vocabulary tables}

\item{table_names}{list of OMOP vocabulary tables specified in \code{table_names}, or all tables if NULL}
}
\value{
tibble of OMOP vocabulary tables with columns:
\code{name} = name of omop vocabulary table
\code{table} = omop table
}
\description{
reads a list of OMOP vocabulary tables
}

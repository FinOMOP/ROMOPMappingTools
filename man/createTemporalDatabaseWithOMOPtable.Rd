% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createTemporalDatabaseWithOMOPtable.R
\name{createTemporalDatabaseWithOMOPtable}
\alias{createTemporalDatabaseWithOMOPtable}
\title{Create a Temporal Database with OMOP Vocabulary Tables}
\usage{
createTemporalDatabaseWithOMOPtable(path_to_omop_vocabulary_folder)
}
\arguments{
\item{path_to_omop_vocabulary_folder}{The path to the folder containing the OMOP vocabulary tables in CSV format.}
}
\value{
Connection details to the temporal database
}
\description{
This function creates a temporary database and load the OMOP vocabulary tables from \code{path_to_omop_vocabulary_folder}.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateCodeCountsFolder.R
\name{validateCodeCountsFolder}
\alias{validateCodeCountsFolder}
\title{Validate Code Counts Folder}
\usage{
validateCodeCountsFolder(pathToCodeCountsFolder)
}
\arguments{
\item{pathToCodeCountsFolder}{Path to folder containing code count files}
}
\value{
A tibble containing validation results for all files
}
\description{
Validates all code count files in a code counts folder against a set of rules.
The folder must contain a vocabularies_coverage.csv file that describes the vocabularies
and their coverage, and a databases_coverage.csv file that describes the databases
and their corresponding code count files.
}

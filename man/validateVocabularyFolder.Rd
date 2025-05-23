% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateVocabularyFolder.R
\name{validateVocabularyFolder}
\alias{validateVocabularyFolder}
\title{Validate Vocabulary Folder}
\usage{
validateVocabularyFolder(
  pathToVocabularyFolder,
  connection,
  vocabularyDatabaseSchema,
  validationResultsFolder
)
}
\arguments{
\item{pathToVocabularyFolder}{Path to folder containing vocabulary files}

\item{connection}{A DatabaseConnector connection object}

\item{vocabularyDatabaseSchema}{Schema name where the vocabulary tables are stored}

\item{validationResultsFolder}{Folder where validation results will be saved}
}
\value{
A tibble containing validation results for all files
}
\description{
Validates all Usagi files in a vocabulary folder against a set of rules.
The folder must contain a vocabularies.csv file that describes the vocabularies
and their corresponding Usagi files.
}

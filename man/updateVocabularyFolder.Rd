% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateVocabularyFolder.R
\name{updateVocabularyFolder}
\alias{updateVocabularyFolder}
\title{Update Vocabulary Folder}
\usage{
updateVocabularyFolder(
  pathToVocabularyFolder,
  connection,
  vocabularyDatabaseSchema,
  updateResultsFolder,
  skipValidation = TRUE
)
}
\arguments{
\item{pathToVocabularyFolder}{Path to folder containing vocabulary files}

\item{connection}{A DatabaseConnector connection object}

\item{vocabularyDatabaseSchema}{Schema name where the vocabulary tables are stored}

\item{updateResultsFolder}{Folder where updated files will be saved}

\item{skipValidation}{Whether to skip validation checks if TRUE. Default is TRUE}
}
\value{
A tibble containing update results for all files
}
\description{
Updates all Usagi files in a vocabulary folder with the latest concept information.
The folder must contain a vocabularies.csv file that describes the vocabularies
and their corresponding Usagi files.
}

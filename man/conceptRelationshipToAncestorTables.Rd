% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conceptRelationshipToAncestorTables.R
\name{conceptRelationshipToAncestorTables}
\alias{conceptRelationshipToAncestorTables}
\title{Convert CONCEPT_RELATIONSHIP to CONCEPT_ANCESTOR}
\usage{
conceptRelationshipToAncestorTables(
  connection,
  vocabularyDatabaseSchema,
  sourceToConceptMapTable
)
}
\arguments{
\item{connection}{A DatabaseConnector connection object}

\item{vocabularyDatabaseSchema}{Schema containing the vocabulary and STCM tables}

\item{sourceToConceptMapTable}{Name of the SOURCE_TO_CONCEPT_MAP_EXTENDED table}
}
\value{
Invisible TRUE if successful
}
\description{
Creates and populates the CONCEPT_ANCESTOR table
from CONCEPT_RELATIONSHIP table data.
}

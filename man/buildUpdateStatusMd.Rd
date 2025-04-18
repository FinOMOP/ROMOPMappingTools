% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildUpdateStatusMd.R
\name{buildUpdateStatusMd}
\alias{buildUpdateStatusMd}
\title{Build Update Status Markdown}
\usage{
buildUpdateStatusMd(
  updateLogTibble,
  pathToUpdateStatusMdFile = file.path(tempdir(),
    "VOCABULARIES_LAST_AUTOMATIC_UPDATE_STATUS.md")
)
}
\arguments{
\item{updateLogTibble}{A tibble containing update results with columns:
context, type, step, and message}

\item{pathToUpdateStatusMdFile}{Path where the markdown file will be saved
(default: tempdir()/VOCABULARIES_LAST_AUTOMATIC_UPDATE_STATUS.md)}
}
\value{
Path to the generated markdown file
}
\description{
Creates a markdown file containing the update status results.
The markdown file includes a table showing update results with:
\itemize{
\item Context of the update
\item Type of message (SUCCESS, WARNING, ERROR)
\item Update step
\item Detailed message
}
}

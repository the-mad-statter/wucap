% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complex.R
\name{redcap_migrate_files}
\alias{redcap_migrate_files}
\title{Migrate All Files from One Project to Another}
\usage{
redcap_migrate_files(
  redcap_uri_src = redcap_api_endpoints$prod$v7,
  redcap_uri_dst = redcap_api_endpoints$prod$latest,
  token_src,
  token_dst,
  return_format = c("xml", "csv", "json")
)
}
\arguments{
\item{redcap_uri_src}{The URI (uniform resource identifier) of the source
REDCap project.}

\item{redcap_uri_dst}{The URI (uniform resource identifier) of the
destination REDCap project.}

\item{token_src}{The API token specific to your source REDCap project and
username (each token is unique to each user for each project). See the
section on the left-hand menu for obtaining a token for a given project.}

\item{token_dst}{The API token specific to your destination REDCap project
and username (each token is unique to each user for each project). See the
section on the left-hand menu for obtaining a token for a given project.}

\item{return_format}{csv, json, xml - specifies the format of error messages.
If you do not pass in this flag, it will select the default format for you
passed based on the 'format' flag you passed in or if no format flag was
passed in, it will default to 'xml'.}
}
\description{
Migrate All Files from One Project to Another
}

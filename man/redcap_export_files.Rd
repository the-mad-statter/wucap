% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{redcap_export_files}
\alias{redcap_export_files}
\title{Export Files}
\usage{
redcap_export_files(
  file_keys,
  repo,
  redcap_uri = redcap_api_endpoints$prod$latest,
  token,
  pause_base = 1,
  pause_cap = 60,
  pause_min = 1,
  return_format = c("xml", "csv", "json")
)
}
\arguments{
\item{file_keys}{tibble of file keys}

\item{repo}{directory in which to write the downloaded files}

\item{redcap_uri}{The URI (uniform resource identifier) of the REDCap
project.}

\item{token}{The API token specific to your REDCap project and username (each
token is unique to each user for each project). See the section on the
left-hand menu for obtaining a token for a given project.}

\item{pause_base, pause_cap}{This method uses exponential back-off with full
jitter - this means that each request will randomly wait between 0 and
pause_base * 2 ^ attempt seconds, up to a maximum of pause_cap seconds.}

\item{pause_min}{Minimum time to wait in the backoff; generally only
necessary if you need pauses less than one second (which may not be kind to
the server, use with caution!).}

\item{return_format}{csv, json, xml - specifies the format of error messages.
If you do not pass in this flag, it will select the default format for you
passed based on the 'format' flag you passed in or if no format flag was
passed in, it will default to 'xml'.}
}
\value{
A download file log invisibly.
}
\description{
Export Files
}
\examples{
\dontrun{
file_index <-
  redcap_export_files(
    file_keys = file_keys,
    repo = "~/redcap_download_files",
    token = my_token
  )
}
}

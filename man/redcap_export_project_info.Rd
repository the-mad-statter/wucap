% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{redcap_export_project_info}
\alias{redcap_export_project_info}
\title{Export Project Information}
\usage{
redcap_export_project_info(
  redcap_uri = redcap_api_endpoints$prod$latest,
  token,
  format = c("xml", "csv", "json"),
  return_format = c("xml", "csv", "json")
)
}
\arguments{
\item{redcap_uri}{The URI (uniform resource identifier) of the REDCap
project.}

\item{token}{The API token specific to your REDCap project and username (each
token is unique to each user for each project). See the section on the
left-hand menu for obtaining a token for a given project.}

\item{format}{format of the response content}

\item{return_format}{csv, json, xml - specifies the format of error messages.
If you do not pass in this flag, it will select the default format for you
passed based on the 'format' flag you passed in or if no format flag was
passed in, it will default to 'xml'.}
}
\value{
\link[httr:response]{httr::response} object containing the project information
}
\description{
Export Project Information
}

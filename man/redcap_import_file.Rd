% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{redcap_import_file}
\alias{redcap_import_file}
\title{Import a File}
\usage{
redcap_import_file(
  redcap_uri = redcap_api_endpoints$prod$latest,
  token,
  record,
  field,
  event,
  repeat_instance,
  file,
  type = NULL,
  name = NULL,
  return_format = c("xml", "csv", "json")
)
}
\arguments{
\item{redcap_uri}{The URI (uniform resource identifier) of the REDCap
project.}

\item{token}{The API token specific to your REDCap project and username (each
token is unique to each user for each project). See the section on the
left-hand menu for obtaining a token for a given project.}

\item{record}{the record ID}

\item{field}{the name of the field that contains the file}

\item{event}{the unique event name - only for longitudinal projects}

\item{repeat_instance}{(only for projects with repeating instruments/events)
The repeat instance number of the repeating event (if longitudinal) or the
repeating instrument (if classic or longitudinal). Default value is '1'.}

\item{file}{path to the file to upload}

\item{type}{MIME content-type of the file.}

\item{name}{file name to use for the upload}

\item{return_format}{csv, json, xml - specifies the format of error messages.
If you do not pass in this flag, it will select the default format for you
passed based on the 'format' flag you passed in or if no format flag was
passed in, it will default to 'xml'.}
}
\value{
\link[httr:response]{httr::response} object reporting empty body on successful upload
}
\description{
This method allows you to upload a document that will be
attached to an individual record for a File Upload field. Please note that
this method may NOT be used for Signature fields (i.e. File Upload fields
with 'signature' validation type) because a signature can only be captured
and stored using the web interface.
}
\note{
To use this method, you must have API Import/Update privileges in the
project.
}
\examples{
\dontrun{
redcap_import_file(
  redcap_uri =
    "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  token = my_project_token,
  record = 1,
  field = "file_field_name"
)
}
}

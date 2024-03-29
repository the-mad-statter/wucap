% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{redcap_read_file_keys}
\alias{redcap_read_file_keys}
\title{Read File Keys}
\usage{
redcap_read_file_keys(
  data_dictionary,
  project_records,
  field_type = c("file", "signature", "both")
)
}
\arguments{
\item{data_dictionary}{Project metadata (i.e., Data Dictionary values).}

\item{project_records}{Project records.}

\item{field_type}{Type of fields in which to search for files.}
}
\value{
tibble containing file keys
}
\description{
Read File Keys
}
\examples{
\dontrun{
file_keys <-
  redcap_read_file_keys(
    data_dictionary = data_dictionary,
    project_records = project_records,
    field_type = "both"
  )
}
}

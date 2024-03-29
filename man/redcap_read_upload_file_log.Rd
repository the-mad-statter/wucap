% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{redcap_read_upload_file_log}
\alias{redcap_read_upload_file_log}
\title{Read Upload File Log}
\usage{
redcap_read_upload_file_log(repo)
}
\arguments{
\item{repo}{directory containing the downloaded files and upload file log.}
}
\value{
the contents of the upload file log
}
\description{
Read Upload File Log
}
\examples{
\dontrun{
file_upload_log <- redcap_read_upload_file_log("~/redcap_download_files")
}
}

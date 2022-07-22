#' REDCap Array
#'
#' @param name name of the array
#' @param values values to be included in the array
#'
#' @return an object of class "redcap_array"
#' @export
#'
#' @examples
#' ## records array for three records from static
#' redcap_array("records", c(16227, 16342, 16419))
#'
#' ## fields array for two fields from static
#' redcap_array("fields", c("chip1_install_date", "chip2_install_date"))
#'
#' ## events array for one event from mother
#' redcap_array("events", "baseline_arm_1")
redcap_array <- function(name, values) {
  a <- as.list(values)
  names(a) <- sprintf("%s[%i]", name, 0:(length(values) - 1))
  class(a) <- "redcap_array"
  return(a)
}

#' Parse Content Type
#'
#' @param content_type content-type header from REDCap to be parsed
#' @return list containing type, name, and charset
parse_content_type <- function (content_type) {
  parts <- stringr::str_remove_all(
    stringr::str_trim(strsplit(content_type, ";")[[1]]),
    '"'
  )

  list(
    "type" = parts[1],
    "name" = stringr::str_remove(parts[2], "name="),
    "charset" = stringr::str_remove(parts[3], "charset=")
  )
}

#' Generate Project XML Filename
#'
#' @param project_title name of the project
#'
#' @return a character representation of what REDCap would named the xml file
#' if it had been exported via the web interface
#'
#' @export
project_xml_filename <- function(project_title) {
  sprintf(
    "%s_%s.REDCap.xml",
    gsub(" ", "", project_title),
    format(Sys.time(), "%Y-%m-%d_%H%M")
  )
}

#' Read Project XML
#'
#' @param odm path to a file
#'
#' @return collapsed character representation of the project xml file
#'
#' @export
read_project_xml <- function(odm) {
  paste(readLines(odm), collapse = "")
}

#' Write Project XML
#'
#' @param r httr response obejct from redcap_export_project_xml()
#' @param file file or connection to write to
#'
#' @export
write_project_xml <- function(r, file) {
  r %>%
    httr::content() %>%
    as.character() %>%
    writeLines(con = file)
}

#' Optional Argument
#'
#' @description Utility function to set a default value if missing or null and
#' to run a validation test.
#'
#' @param x value to process
#' @param d default value
#' @param v validation function
#'
#' @return either x or the default value after passing validation
optional_argument <- function(x, d = NULL, v = checkmate::assert_character) {
  if(missing(x) || is.null(x))
    x <- d
  if(!is.null(x))
    do.call(v, list(x = x))
  x
}

#' Check if argument is of type redcap_array
#'
#' @param x object to check
assert_redcap_array <- function(x) {
  checkmate::assert_class(x, "redcap_array")
}

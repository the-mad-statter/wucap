#' Store a token and other credentials in a file
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#' token is unique to each user for each project). See the section on the
#' left-hand menu for obtaining a token for a given project.
#' @param path_credential The file path to the CSV containing the credentials.
#' Required.
#' @param username REDCap username associated with the token
#'
#' @export
store_credential_local <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           path_credential = "~/.REDCapR",
           username = Sys.getenv("WU_REDCAP_USER")) {
    redcap_export_project_info(
      redcap_uri = redcap_uri,
      token = token,
      format = "csv"
    ) %>%
      httr::content() %>%
      dplyr::transmute(
        redcap_uri = redcap_uri,
        username = username,
        project_id = as.character(.data["project_id"]),
        token = token,
        comment = .data["project_title"]
      ) %>%
      readr::write_csv(path_credential, na = "", append = TRUE, quote = "all")
  }

#' @inherit REDCapR::retrieve_credential
#' @export
retrieve_credential_local <-
  function(project_id,
           path_credential = "~/.REDCapR",
           ...) {
    REDCapR::retrieve_credential_local(
      path_credential = path_credential,
      project_id = project_id,
      ...
    )
  }

#' Edit tokens and other credentials in a file
#'
#' @param path_credential The file path to the CSV containing the credentials.
#' Required.
#'
#' @return Target path, invisibly.
#' @export
edit_credential_local <-
  function(path_credential = "~/.REDCapR") {
    r_user <- gsub("\\\\", "/", normalizePath("~"))
    path_credential <- sub("~", r_user, path_credential)
    usethis::edit_file(path_credential)
  }

#' @inherit REDCapR::redcap_read
#' @export
redcap_read <-
  function(project_id, path_credential = "~/.REDCapR", ...) {
    credentials <- REDCapR::retrieve_credential_local(
      path_credential,
      project_id
    )
    REDCapR::redcap_read(
      redcap_uri = credentials$redcap_uri,
      token = credentials$token,
      ...
    )$data
  }

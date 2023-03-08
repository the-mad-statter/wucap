#' Extract File Path or Name from a Request
#'
#' @param x request object
#' @param as desired type of output: file path or name
#'
#' @return For "file_path", a path to the downloaded file. For "file_name" the
#' intended name of the file.
#' @export
request_file_content <- function(x, as = c("file_path", "file_name")) {
  as <- match.arg(as)
  if (as == "file_path") {
    x$content
  } else {
    parse_content_type(x[["headers"]][["content-type"]])[["name"]]
  }
}

#' File Set Difference
#'
#' @param x file keys or index
#' @param y file index or keys
#'
#' @return information about files in x that are not in y
#' @export
#'
#' @examples
#' \dontrun{
#' repo <- "~/redcap_download_files"
#' local_file_index <- redcap_read_file_index(repo)
#'
#' ## check for missing downloads
#' source_file_keys <-
#'   redcap_read_file_keys(data_dictionary, project_records)
#' missing_downloads <- file_setdiff(source_file_keys, local_file_index)
#' redcap_export_files(missing_downloads, repo, token = source_token)
#'
#' ## check for missing uploads
#' destination_file_keys <-
#'   redcap_read_file_keys(data_dictionary, project_records)
#' missing_uploads <- file_setdiff(local_file_index, destination_file_keys)
#' redcap_import_files(missing_uploads, repo, token = destination_token)
#' }
file_setdiff <- function(x, y) {
  UseMethod("file_setdiff", x)
}

#' @rdname file_setdiff
#' @export
file_setdiff.wucap_file_keys <- function(x, y) {
  checkmate::assert_class(y, "wucap_file_index")
  dplyr::anti_join(x, y, names(x))
}

#' @rdname file_setdiff
#' @export
file_setdiff.wucap_file_index <- function(x, y) {
  checkmate::assert_class(y, "wucap_file_keys")
  dplyr::anti_join(x, y, names(y))
}

#' Signature Validation Disable
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param data_dictionary Project metadata (i.e., Data Dictionary values).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_signature_disable(
#'   token = my_token,
#'   data_dictionary = data_dictionary
#' )
#' }
redcap_signature_disable <- function(
    redcap_uri = redcap_api_endpoints$prod$latest,
    token,
    data_dictionary) {
  REDCapR::redcap_metadata_write(
    ds = data_dictionary %>%
      dplyr::mutate(
        text_validation_type_or_show_slider_number =
          dplyr::if_else(
            .data[["text_validation_type_or_show_slider_number"]] ==
              "signature",
            NA_character_,
            .data[["text_validation_type_or_show_slider_number"]]
          )
      ),
    redcap_uri = redcap_uri,
    token = token
  )
}

#' Signature Validation Enable
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param data_dictionary Original project metadata containing the signature
#' validation note (i.e., the original Data Dictionary values).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_signature_enable(
#'   token = my_token,
#'   data_dictionary = data_dictionary
#' )
#' }
redcap_signature_enable <- function(
    redcap_uri,
    token,
    data_dictionary) {
  REDCapR::redcap_metadata_write(
    ds = data_dictionary,
    redcap_uri = redcap_uri,
    token = token
  )
}

#' Read File Metafile
#'
#' @param repo directory containing the downloaded files and listed metafile
#' @param metafile the metafile to read
#' @param metaclass the class of metafile
#'
#' @return the contents of the metafile
read_file_metafile <-
  function(repo,
           metafile = c(
             "_redcap_file_index.csv",
             "_redcap_download_file_log.csv",
             "_redcap_upload_file_log.csv"
           ),
           metaclass = c(
             "wucap_file_index",
             "wucap_download_file_log",
             "wucap_upload_file_log"
           )) {
    metafile <- match.arg(metafile)
    metaclass <- match.arg(metaclass)


    p <- file.path(repo, metafile)

    n <- names(readr::read_csv(p, n_max = 0, show_col_types = FALSE))

    col_spec <- readr::cols(
      field_name = readr::col_character(),
      field_type = readr::col_character(),
      !!n[3] := readr::col_character(),
      redcap_event_name = readr::col_character(),
      redcap_repeat_instance = readr::col_character(),
      local_file = readr::col_character(),
      remote_file = readr::col_character(),
      read_status = readr::col_integer(),
      note = readr::col_character()
    )$cols[n]

    m <- readr::read_csv(p, col_types = col_spec)
    class(m) <- c(metaclass, class(m))
    return(m)
  }

#' Read File Index
#'
#' @param repo directory containing the downloaded files and index file.
#'
#' @return the contents of the index file
#' @export
#'
#' @examples
#' \dontrun{
#' file_index <- redcap_read_file_index("~/redcap_download_files")
#' }
redcap_read_file_index <- function(repo) {
  read_file_metafile(
    repo,
    "_redcap_file_index.csv",
    "wucap_file_index"
  )
}

#' Read Download File Log
#'
#' @param repo directory containing the downloaded files and download file log.
#'
#' @return the contents of the download file log
#' @export
#'
#' @examples
#' \dontrun{
#' file_download_log <- redcap_read_download_file_log("~/redcap_download_files")
#' }
redcap_read_download_file_log <- function(repo) {
  read_file_metafile(
    repo,
    "_redcap_download_file_log.csv",
    "wucap_download_file_log"
  )
}

#' Read Upload File Log
#'
#' @param repo directory containing the downloaded files and upload file log.
#'
#' @return the contents of the upload file log
#' @export
#'
#' @examples
#' \dontrun{
#' file_upload_log <- redcap_read_upload_file_log("~/redcap_download_files")
#' }
redcap_read_upload_file_log <- function(repo) {
  read_file_metafile(
    repo,
    "_redcap_upload_file_log.csv",
    "wucap_upload_file_log"
  )
}

#' Read File Keys
#'
#' @param data_dictionary Project metadata (i.e., Data Dictionary values).
#' @param project_records Project records.
#' @param field_type Type of fields in which to search for files.
#'
#' @return tibble containing file keys
#' @export
#'
#' @examples
#' \dontrun{
#' file_keys <-
#'   redcap_read_file_keys(
#'     data_dictionary = data_dictionary,
#'     project_records = project_records,
#'     field_type = "both"
#'   )
#' }
redcap_read_file_keys <-
  function(data_dictionary,
           project_records,
           field_type = c("file", "signature", "both")) {
    field_type <- match.arg(field_type)

    field_types <- if (field_type == "both") {
      c("file", "signature")
    } else {
      field_type
    }

    file_keys <- data_dictionary %>%
      dplyr::filter(.data[["field_type"]] %in% field_types) %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "field_name",
            "field_type",
            "text_validation_type_or_show_slider_number"
          )
        )
      ) %>%
      dplyr::transmute(
        field_name = .data[["field_name"]],
        field_type = dplyr::if_else(
          !is.na(.data[["text_validation_type_or_show_slider_number"]]),
          .data[["text_validation_type_or_show_slider_number"]],
          .data[["field_type"]]
        )
      ) %>%
      purrr::pmap_dfr(~ {
        project_records %>%
          dplyr::select(
            dplyr::all_of(data_dictionary[[1, 1]]),
            dplyr::any_of(c(
              "redcap_event_name",
              "redcap_repeat_instance"
            )),
            dplyr::all_of(..1)
          ) %>%
          dplyr::filter(!is.na(.data[[..1]])) %>%
          dplyr::rename(file = ..1) %>% # [document]
          dplyr::mutate(
            field_name = ..1,
            field_type = ..2
          )
      }) %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "field_name",
            "field_type",
            data_dictionary[[1, 1]]
          )
        ),
        dplyr::any_of(c(
          "redcap_event_name",
          "redcap_repeat_instance"
        ))
      )

    class(file_keys) <- c("wucap_file_keys", class(file_keys))
    return(file_keys)
  }

#' Export Files
#'
#' @param file_keys tibble of file keys
#' @param repo directory in which to write the downloaded files
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param pause_base,pause_cap This method uses exponential back-off with full
#' jitter - this means that each request will randomly wait between 0 and
#' pause_base * 2 ^ attempt seconds, up to a maximum of pause_cap seconds.
#' @param pause_min Minimum time to wait in the backoff; generally only
#' necessary if you need pauses less than one second (which may not be kind to
#' the server, use with caution!).
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return A download file log invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' file_index <-
#'   redcap_export_files(
#'     file_keys = file_keys,
#'     repo = "~/redcap_download_files",
#'     token = my_token
#'   )
#' }
redcap_export_files <-
  function(file_keys,
           repo,
           redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           pause_base = 1,
           pause_cap = 60,
           pause_min = 1,
           return_format = c("xml", "csv", "json")) {
    checkmate::assert_class(file_keys, "wucap_file_keys")

    # alias to call `data_dictionary[[1, 1]] for record id field name
    data_dictionary <- dplyr::tibble(names(file_keys)[3])

    return_format <- match.arg(return_format)

    if (!("redcap_event_name" %in% names(file_keys))) {
      file_keys$redcap_event_name <- NA_character_
    }

    if (!("redcap_repeat_instance" %in% names(file_keys))) {
      file_keys$redcap_repeat_instance <- NA_character_
    }

    total <- nrow(file_keys)
    pb <- progress::progress_bar$new(
      format = "Downloading files [:bar] :current/:total (:percent) ETR: :eta",
      total = total
    )
    pb$tick(0)

    download_file_log <-
      file_keys %>%
      tibble::rowid_to_column() %>%
      dplyr::mutate(n = total) %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "field_name", # 1
            "field_type", # 2
            data_dictionary[[1, 1]], # 3
            "redcap_event_name", # 4
            "redcap_repeat_instance", # 5
            "rowid", # 6
            "n" # 7
          )
        )
      ) %>%
      purrr::pmap_dfr(~ {
        pause_time <- max(
          pause_min,
          stats::runif(1, max = min(pause_cap, pause_base * (2^1)))
        )
        Sys.sleep(pause_time)

        ...4 <- ifelse(is.na(..4), rlang::missing_arg(), ..4)
        ...5 <- ifelse(is.na(..5), rlang::missing_arg(), ..5)

        msg_sfx <-
          sprintf(
            " file %s of %s for field = %s, %s = %s%s%s.",
            ..6,
            ..7,
            ..1,
            data_dictionary[[1, 1]], ..3,
            ifelse(is.na(..4), "", sprintf(", event = %s", ..4)),
            ifelse(is.na(..5), "", sprintf(", repeat instance = %s", ..5))
          )

        if (exists("pb")) { # pb missing on some loops
          pb$update(..6 / ..7)
        }

        r <- tryCatch(
          {
            redcap_export_file(
              redcap_uri = redcap_uri,
              token = token,
              record = ..3,
              field = ..1,
              event = ...4,
              repeat_instance = ...5,
              path = repo,
              return_format = return_format
            )
          },
          error = function(e) {
            message(
              sprintf(
                "Error message:\nIn redcap_export_file() : %s %s\n%s",
                "When reading",
                msg_sfx,
                e$message
              )
            )
            return(e$message)
          },
          warning = function(w) {
            message(
              sprintf(
                "Warning message:\nIn redcap_export_file() : %s %s\n%s",
                "When reading",
                msg_sfx,
                w$message
              )
            )
            return(w$message)
          }
        )

        http_status <- NA_integer_
        local_file <- NA_character_
        remote_file <- NA_character_
        note <- NA_character_

        if (is.character(r)) {
          note <- r
        }

        if (inherits(r, "response")) {
          http_status <- r$status_code

          if (!httr::http_error(r)) {
            local_file <- basename(request_file_content(r, "file_path"))
            remote_file <- request_file_content(r, "file_name")
          } else {
            note <- httr::http_status(r)$message
            warning(sprintf("\n%s %s\n%s", "When reading", msg_sfx, note))
          }
        }

        file_index_i <- dplyr::tibble(
          "field_name" = ..1,
          "field_type" = ..2,
          !!data_dictionary[[1, 1]] := ..3
        )

        if (!rlang::is_missing(...4)) {
          file_index_i$redcap_event_name <- ..4
        }

        if (!rlang::is_missing(...5)) {
          file_index_i$redcap_repeat_instance <- ..5
        }

        file_index_i <- file_index_i %>%
          dplyr::mutate(
            "local_file" = local_file,
            "remote_file" = remote_file
          )

        if (!is.na(local_file)) {
          file_index <- file.path(repo, "_redcap_file_index.csv")
          readr::write_csv(
            x = file_index_i,
            file = file_index,
            na = "",
            append = file.exists(file_index)
          )
        }

        file_log <- file.path(repo, "_redcap_download_file_log.csv")
        file_index_i %>%
          dplyr::mutate(
            "http_status" = http_status,
            "note" = note
          ) %>%
          readr::write_csv(
            file = file_log,
            na = "",
            append = file.exists(file_log)
          )
      })

    class(download_file_log) <- c(
      "wucap_download_file_log",
      class(download_file_log)
    )
    invisible(download_file_log)
  }

#' Import Files
#'
#' @param file_index contents of the index file
#' @param repo Directory containing the downloaded files and index file.
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param pause_base,pause_cap This method uses exponential back-off with full
#' jitter - this means that each request will randomly wait between 0 and
#' pause_base * 2 ^ attempt seconds, up to a maximum of pause_cap seconds.
#' @param pause_min Minimum time to wait in the backoff; generally only
#' necessary if you need pauses less than one second (which may not be kind to
#' the server, use with caution!).
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return An upload file log invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_import_files(
#'   file_index = redcap_read_file_index(repo),
#'   repo = "~/redcap_download_files",
#'   token = my_token
#' )
#' }
redcap_import_files <- function(
    file_index,
    repo,
    redcap_uri = redcap_api_endpoints$prod$latest,
    token,
    pause_base = 1,
    pause_cap = 60,
    pause_min = 1,
    return_format = c("xml", "csv", "json")) {
  checkmate::assert_class(file_index, "wucap_file_index")

  # alias to call `data_dictionary[[1, 1]] for record id field name
  data_dictionary <- dplyr::tibble(names(file_index)[3])

  return_format <- match.arg(return_format)

  if ("signature" %in% file_index$field_type) {
    signature_field_names <- file_index %>%
      dplyr::filter(.data[["field_type"]] == "signature") %>%
      dplyr::pull(.data[["field_name"]]) %>%
      unique()

    stop(
      sprintf(
        "Disable signature validation on fields: %s before continuing.",
        paste(signature_field_names, collaspe = ", ")
      )
    )
  }

  if (!("redcap_event_name" %in% names(file_index))) {
    file_index$redcap_event_name <- NA_character_
  }

  if (!("redcap_repeat_instance" %in% names(file_index))) {
    file_index$redcap_repeat_instance <- NA_character_
  }

  total <- nrow(file_index)
  pb <- progress::progress_bar$new(
    format = "Uploading files [:bar] :current/:total (:percent) ETR: :eta",
    total = total
  )
  pb$tick(0)

  upload_file_log <-
    file_index %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(n = total) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "field_name", # 1
          "field_type", # 2
          data_dictionary[[1, 1]], # 3
          "redcap_event_name", # 4
          "redcap_repeat_instance", # 5
          "local_file", # 6
          "remote_file", # 7
          "rowid", # 8
          "n" # 9
        )
      )
    ) %>%
    purrr::pmap_dfr(~ {
      pause_time <- max(
        pause_min,
        stats::runif(1, max = min(pause_cap, pause_base * (2^1)))
      )
      Sys.sleep(pause_time)

      ...4 <- ifelse(is.na(..4), rlang::missing_arg(), ..4)
      ...5 <- ifelse(is.na(..5), rlang::missing_arg(), ..5)

      msg_sfx <-
        sprintf(
          " file %s of %s for field = %s, %s = %s%s%s.",
          ..8,
          ..9,
          ..1,
          data_dictionary[[1, 1]], ..3,
          ifelse(is.na(..4), "", sprintf(", event = %s", ..4)),
          ifelse(is.na(..5), "", sprintf(", repeat instance = %s", ..5))
        )

      if (exists("pb")) { # pb missing on some loops?
        pb$update(..8 / ..9)
      }

      r <- tryCatch(
        {
          redcap_import_file(
            redcap_uri = redcap_uri,
            token = token,
            record = ..3,
            field = ..1,
            event = ...4,
            repeat_instance = ...5,
            file = file.path(repo, ..6),
            name = ..7,
            return_format = return_format
          )
        },
        error = function(e) {
          message(
            sprintf(
              "Error message:\nIn redcap_import_file() : %s %s\n%s",
              "When writing",
              msg_sfx,
              e$message
            )
          )
          return(e$message)
        },
        warning = function(w) {
          message(
            sprintf(
              "Warning message:\nIn redcap_import_file() : %s %s\n%s",
              "When writing",
              msg_sfx,
              w$message
            )
          )
          return(w$message)
        }
      )

      http_status <- NA_integer_
      note <- NA_character_

      if (is.character(r)) {
        note <- r
      }

      if (inherits(r, "response")) {
        http_status <- r$status_code

        if (httr::http_error(r)) {
          note <- httr::http_status(r)$message
          warning(sprintf("\n%s %s\n%s", "When writing", msg_sfx, note))
        }
      }

      upload_file_log_i <- dplyr::tibble(
        "field_name" = ..1,
        "field_type" = ..2,
        !!data_dictionary[[1, 1]] := ..3
      )

      if (!rlang::is_missing(...4)) {
        upload_file_log_i$redcap_event_name <- ..4
      }

      if (!rlang::is_missing(...5)) {
        upload_file_log_i$redcap_repeat_instance <- ..5
      }

      file_log <- file.path(repo, "_redcap_upload_file_log.csv")
      upload_file_log_i %>%
        dplyr::mutate(
          "local_file" = ..6,
          "remote_file" = ..7,
          "http_status" = http_status,
          "note" = note
        ) %>%
        readr::write_csv(
          file = file_log,
          na = "",
          append = file.exists(file_log)
        )
    })

  class(upload_file_log) <- c("wucap_upload_file_log", class(upload_file_log))
  invisible(upload_file_log)
}

#' Validate File Index
#'
#' @param repo Directory containing the downloaded files and index file.
#' @param purge_rcf Which REDCap files (.rcf) to purge: `not_indexed`: existing
#' local files not listed in the index file; `not_saved`: local files listed in
#' the index but not saved locally
#'
#' @return a pair of [dplyr::tibble()] listing REDCap files not saved or not
#' indexed
#' @export
#'
#' @examples
#' \dontrun{
#' ## check validity
#' redcap_validate_file_index("my_repo")
#' }
redcap_validate_file_index <-
  function(repo, purge_rcf = c("none", "not_indexed", "not_saved", "both")) {
    purge_rcf <- match.arg(purge_rcf)

    file_index <- redcap_read_file_index(repo)
    rcf_saved <- dplyr::tibble(local_file = list.files(repo, "\\.rcf$"))
    rcf_indexed <- dplyr::select(file_index, .data[["local_file"]])
    rcf_not_indexed <- dplyr::anti_join(
      rcf_saved,
      rcf_indexed,
      dplyr::join_by(.data[["local_file"]])
    )
    rcf_not_saved <- dplyr::anti_join(
      rcf_indexed,
      rcf_saved,
      dplyr::join_by(.data[["local_file"]])
    )

    if (purge_rcf %in% c("not_indexed", "both")) {
      purrr::walk(rcf_not_indexed, ~ unlink(file.path(repo, .)))
    }

    if (purge_rcf %in% c("not_saved", "both")) {
      readr::write_csv(
        dplyr::anti_join(
          file_index,
          rcf_not_saved,
          dplyr::join_by(.data[["local_file"]])
        ),
        file.path(repo, "_redcap_file_index.csv"),
        na = ""
      )
    }

    if (purge_rcf != "none") {
      redcap_validate_file_index(repo, "none")
    }

    list(rcf_not_saved = rcf_not_saved, rcf_not_indexed = rcf_not_indexed)
  }

#' Create Project
#'
#' @description  This method allows you to create a new REDCap project. A
#' 64-character Super API Token is required for this method (as opposed to
#' project-level API methods that require a regular 32-character token
#' associated with the project-user). In the API request, you must minimally
#' provide the project attributes 'project_title' and 'purpose' (with numerical
#' value 0=Practice/Just for fun, 1=Other, 2=Research, 3=Quality Improvement,
#' 4=Operational Support) when creating a project.
#'
#' When a project is created with this method, the project will automatically be
#' given all the project-level defaults just as if you created a new empty
#' project via the web user interface, such as a automatically creating a
#' single data collection instrument seeded with a single Record ID field and
#' Form Status field, as well as (for longitudinal projects) one arm with one
#' event. And if you intend to create your own arms or events immediately after
#' creating the project, it is recommended that you utilize the override=1
#' parameter in the 'Import Arms' or 'Import Events' method, respectively, so
#' that the default arm and event are removed when you add your own. Also, the
#' user creating the project will automatically be added to the project as a
#' user with full user privileges and a project-level API token, which could
#' then be used for subsequent project-level API requests.
#'
#' NOTE: Only users with Super API Tokens can utilize this method. Users can
#' only be granted a super token by a REDCap administrator (using the API Tokens
#' page in the REDCap Control Center). Please be advised that users with a
#' Super API Token can create new REDCap projects via the API without any
#' approval needed by a REDCap administrator. If you are interested in
#' obtaining a super token, please contact your local REDCap administrator.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The Super API Token specific to a user
#' @param project_title (required) title of the project
#' @param purpose (required) purpose of the project
#' @param purpose_other (required if purpose is "Other") custom purpose
#' @param project_notes (optional) notes describing the project
#' @param is_longitudinal is the project to be longitudinal
#' @param surveys_enabled is the project to contain surveys
#' @param record_autonumbering_enabled should the records be autonumbered
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param odm path to file containing an XML string in CDISC ODM XML format that
#' contains project metadata (fields, forms, events, arms) and might optionally
#' contain data to be imported as well. The XML contained in this parameter
#' can come from a REDCap Project XML export file from REDCap itself, or may
#' come from another system that is capable of exporting projects and data in
#' CDISC ODM format. If the 'odm' parameter is included in the API request, it
#' will use the XML to import its contents into the newly created project.
#' This will allow you not only to create the project with the API request,
#' but also to import all fields, forms, and project attributes (and events
#' and arms, if longitudinal) as well as record data all at the same time.
#'
#' @return an httr response object containing a 32-character project-level API
#' token (associated with both the project and user creating the project).
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_create_project_from_odm(
#'   token = my_super_token,
#'   project_title = "redcap_create_project_from_odm test",
#'   purpose = "Practice/Just for fun",
#'   odm = "odm.xml"
#' ) %>%
#'   httr::content(r, as = "text") -> my_project_token
#' }
redcap_create_project2 <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           project_title,
           purpose = c(
             "Practice/Just for fun",
             "Other",
             "Research",
             "Quality Improvement",
             "Operational Support"
           ),
           purpose_other,
           project_notes,
           is_longitudinal = FALSE,
           surveys_enabled = FALSE,
           record_autonumbering_enabled = FALSE,
           return_format = c("xml", "csv", "json"),
           odm) {
    purpose <- which(
      match.arg(purpose) == c(
        "Practice/Just for fun",
        "Other",
        "Research",
        "Quality Improvement",
        "Operational Support"
      )
    ) - 1

    data <- list(
      "project_title" = project_title,
      "purpose" = as.character(purpose),
      "purpose_other" = ifelse(purpose == 1,
        checkmate::assert_character(purpose_other),
        NULL
      ),
      "project_notes" = validate_arg(project_notes),
      "is_longitudinal" = as.character(as.integer(is_longitudinal)),
      "surveys_enabled" = as.character(as.integer(surveys_enabled)),
      "record_autonumbering_enabled" = as.character(
        as.integer(record_autonumbering_enabled)
      )
    )

    redcap_create_project(
      redcap_uri = redcap_uri,
      token = token,
      format = "json",
      data = paste0("[", jsonlite::toJSON(data, auto_unbox = TRUE), "]"),
      return_format = match.arg(return_format),
      odm = paste(readLines(odm), collapse = "")
    )
  }

#' Migrate a File from One Project to Another
#'
#' @param redcap_uri_src The URI (uniform resource identifier) of the source
#' REDCap project.
#' @param redcap_uri_dst The URI (uniform resource identifier) of the
#' destination REDCap project.
#' @param token_src The API token specific to your source REDCap project and
#' username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param token_dst The API token specific to your destination REDCap project
#' and username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param record the record ID
#' @param field the name of the field that contains the file
#' @param event the unique event name - only for longitudinal projects
#' @param repeat_instance (only for projects with repeating instruments/events)
#' The repeat instance number of the repeating event (if longitudinal) or the
#' repeating instrument (if classic or longitudinal). Default value is '1'.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return a pair of httr response objects: one for the export and one for the
#' import
#' @export
redcap_migrate_file <-
  function(redcap_uri_src = redcap_api_endpoints$prod$v7,
           redcap_uri_dst = redcap_api_endpoints$prod$latest,
           token_src,
           token_dst,
           record,
           field,
           event,
           repeat_instance,
           return_format = c("xml", "csv", "json")) {
    r_export <- redcap_export_file(
      redcap_uri = redcap_uri_src,
      token = token_src,
      record = record,
      field = field,
      event = event,
      repeat_instance = repeat_instance,
      return_format = match.arg(return_format)
    )

    base_name <- basename(r_export$content[[1]])

    message(sprintf("Downloading file: %s...", base_name))

    r_import <- redcap_import_file(
      token = token_dst,
      redcap_uri = redcap_uri_dst,
      record = record,
      field = field,
      event = event,
      repeat_instance = repeat_instance,
      file = r_export$content[[1]],
      return_format = match.arg(return_format)
    )

    message(sprintf("Uploading file: %s...", base_name))

    list(
      "export_response" = r_export,
      "import_response" = r_import
    )
  }

#' Migrate All Files from One Project to Another
#'
#' @param redcap_uri_src The URI (uniform resource identifier) of the source
#' REDCap project.
#' @param redcap_uri_dst The URI (uniform resource identifier) of the
#' destination REDCap project.
#' @param token_src The API token specific to your source REDCap project and
#' username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param token_dst The API token specific to your destination REDCap project
#' and username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @export
redcap_migrate_files <-
  function(redcap_uri_src = redcap_api_endpoints$prod$v7,
           redcap_uri_dst = redcap_api_endpoints$prod$latest,
           token_src,
           token_dst,
           return_format = c("xml", "csv", "json")) {
    message("Reading source project data dictionary...")
    data_dictionary <- REDCapR::redcap_metadata_read(
      redcap_uri = redcap_uri_src,
      token = token_src
    )$data

    message("Reading source project records...")
    project_records <- REDCapR::redcap_read(
      redcap_uri = redcap_uri_src,
      token = token_src,
      export_survey_fields = TRUE,
      export_data_access_groups = TRUE
    )$data

    # temporarily change signature fields into regular upload fields
    message("Disabling destination project signature constraints...")
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
      redcap_uri = redcap_uri_dst,
      token = token_dst
    )

    # get names of fields of type file
    file_field_names <- data_dictionary %>%
      dplyr::filter(.data[["field_type"]] == "file") %>%
      dplyr::pull(.data[["field_name"]])

    # for each file field find records requiring a file migration and do it
    for (file_field_name in file_field_names) {
      message(sprintf("Transfering files for field: %s...", file_field_name))

      # find records with files in the current field
      files_for_migration <- project_records %>%
        dplyr::select(
          dplyr::any_of(
            c(
              data_dictionary[[1, 1]],
              "redcap_event_name",
              "redcap_repeat_instance",
              file_field_name
            )
          )
        ) %>%
        dplyr::filter(
          !is.na(.data[[file_field_name]])
        )

      # migrate the files
      for (k in seq_len(nrow(files_for_migration))) {
        redcap_migrate_file(
          redcap_uri_src = redcap_uri_src,
          redcap_uri_dst = redcap_uri_dst,
          token_src = token_src,
          token_dst = token_dst,
          record = files_for_migration[[k, data_dictionary[[1, 1]]]],
          field = file_field_name,
          event = files_for_migration[[k, "redcap_event_name"]],
          repeat_instance = as.integer(
            files_for_migration[[k, "redcap_repeat_instance"]]
          ),
          return_format = match.arg(return_format)
        )
      }
    }

    # add signature constraint back to original signature fields
    message("Enabling destination project signature constraints...")
    REDCapR::redcap_metadata_write(
      ds = data_dictionary,
      redcap_uri = redcap_uri_dst,
      token = token_dst
    ) %>%
      invisible()
  }

#' Reset Completion Flags
#'
#' @description When exporting records containing instrument completion flag
#' fields from REDCap (regardless of format), REDCap maps both
#' grey/no sql table record/"Incomplete (no data saved)" and
#' red/1/"Incomplete" to red/1/"Incomplete". Because it is difficult to
#' determine which records should have remained
#' grey/no sql table record/"Incomplete (no data saved)", this function
#' assumes instruments with no data present should be set to
#' grey/no sql table record/"Incomplete (no data saved)"
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param data_dictionary (optional) If supplied should be data object from a
#' call to REDCapR::redcap_metadata_read(). If not supplied the function will
#' call REDCapR::redcap_metadata_read().
#' @param project_records (optional) If supplied should be the data object from
#' a call to REDCapR::redcap_read(). If not supplied the function will call
#' REDCapR::redcap_read().
redcap_reset_completion_flags <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           data_dictionary,
           project_records) {
    warning(paste0(
      "This function does not work as intended.\n",
      "See function definition for more information."
    ))

    # nolint start

    # the logic of this function works and the api accepts the write, but REDCap
    # v10 reverts to red and not grey as intended
    #
    # the following test code can be used to see the effect on a single record
    #
    # tmp <- tempfile(fileext = ".csv")
    # dplyr::tibble(
    #   record = 3,
    #   field_name = "instrument_1_complete",
    #   value = "", # "": Incomplete; 0: Incomplete; 1: Unverified; 2: Complete
    #   redcap_event_name = "event_1_arm_1",
    #   redcap_repeat_instrument = "instrument_1",
    #   redcap_repeat_instance = 1
    # ) %>%
    #   readr::write_csv(tmp)
    #
    # washu::redcap_import_records(
    #   redcap_uri = redcap_api_endpoints$prod$latest,
    #   token = "211F2459A749042B342EC8A4DB569F4E",
    #   format = "csv",
    #   type = "eav",
    #   overwrite_behavior = "overwrite",
    #   data = paste(readLines(tmp), collapse = "\n"),
    #   return_content = "ids",
    #   return_format = "xml"
    # ) %>%
    #   httr::content() %>%
    #   as.character()

    # and here are pseudo sql queries that might work to do it in the backend
    # DELETE D
    # FROM redcap_data D
    # LEFT JOIN redcap_events_metadata E ON D.event_id=E.event_id
    # WHERE D.project_id='13006' AND D.record='3' AND
    # D.field_name='instrument_1_complete' AND E.descrip='Event 1'
    # AND D.instance IS NULL
    #
    # DELETE D
    # FROM redcap_data D
    # LEFT JOIN redcap_events_metadata E ON D.event_id=E.event_id
    # WHERE D.project_id='13006' AND D.record='3' AND
    # D.field_name='instrument_1_complete' AND E.descrip='Event 1'
    # AND D.instance='2'

    # nolint end

    if (missing(data_dictionary) || is.null(data_dictionary)) {
      data_dictionary <- REDCapR::redcap_metadata_read(
        redcap_uri = redcap_uri,
        token = token
      )$data
    }

    if (missing(project_records) || is.null(project_records)) {
      project_records <- REDCapR::redcap_read(
        redcap_uri = redcap_uri,
        token = token,
        export_survey_fields = TRUE,
        export_data_access_groups = TRUE
      )$data
    }

    for (form in unique(data_dictionary$form_name)) {
      # generate vector of checkbox field names for form
      checkbox_field_names <- data_dictionary %>%
        dplyr::filter(.data[["form_name"]] == form) %>%
        dplyr::filter(.data[["field_type"]] == "checkbox") %>%
        dplyr::mutate(
          n_choices = stringr::str_count(
            .data[["select_choices_or_calculations"]], "\\|"
          )
        ) %>%
        dplyr::select(.data[["field_name"]], .data[["n_choices"]]) %>%
        purrr::pmap_chr(
          function(field_name, n_choices) {
            paste0(field_name, "___", 0:n_choices)
          }
        )

      # generate vector of non-checkbox field names for form
      noncheckbox_field_names <- data_dictionary %>%
        dplyr::filter(.data[["form_name"]] == form) %>%
        dplyr::filter(
          .data[["field_name"]] != data_dictionary[[1, 1]],
          .data[["field_type"]] != "checkbox",
          .data[["field_type"]] != "descriptive"
        ) %>%
        dplyr::pull(.data[["field_name"]])

      field_names <- c(noncheckbox_field_names, checkbox_field_names)

      # count not missing for form for record-event-instance
      n_nna <- project_records %>%
        dplyr::select(
          dplyr::all_of(field_names)
        ) %>%
        dplyr::mutate_at(
          dplyr::all_of(checkbox_field_names),
          ~ dplyr::if_else(.data == 0, NA_real_, .data)
        ) %>%
        dplyr::mutate(n_nna = rowSums(!is.na(.data))) %>%
        dplyr::pull(n_nna)

      # make dataframe of flags to set to grey "Incomplete (no data saved)"
      d <- project_records %>%
        dplyr::mutate(n_nna = n_nna) %>%
        dplyr::filter(n_nna == 0) %>%
        dplyr::select(
          dplyr::any_of(
            c(
              data_dictionary[[1, 1]],
              "redcap_event_name",
              "redcap_repeat_instance",
              paste0(form, "_complete")
            )
          )
        )

      if (nrow(d) > 0) {
        d[paste0(form, "_complete")] <- ""

        # write flags to REDCap
        REDCapR::redcap_write(
          ds_to_write = d,
          redcap_uri = redcap_uri,
          token = token
        ) %>%
          invisible()
      }
    }
  }

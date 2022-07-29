## Arms ------------------------------------------------------------------------

## Data Access Groups ----------------------------------------------------------

#' Export DAGs
#'
#' @description This method allows you to export the Data Access Groups for a
#' project
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml `[default]`
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return [httr::response] object containing DAGs for the project in the
#' format specified
#' @export
redcap_export_dags <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json"),
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "dag",
      "format" = match.arg(format),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Import DAGs
#'
#' @description This method allows you to import new DAGs (Data Access Groups)
#' into a project or update the group name of any existing DAGs.
#'
#' @note DAGs can be renamed by simply changing the group name
#' (data_access_group_name). DAG can be created by providing group name value
#' while unique group name should be set to blank. To use this method, you must
#' have API Import/Update privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml `[default]`
#' @param data Contains the attributes 'data_access_group_name'
#' (referring to the group name) and 'unique_group_name'
#' (referring to the auto-generated unique group name) of each DAG to be
#' created/modified, in which they are provided in the specified format.
#' 1. JSON Example:
#' `[{"data_access_group_name":"CA Site","unique_group_name":"ca_site"}
#' {"data_access_group_name":"FL Site","unique_group_name":"fl_site"},
#' {"data_access_group_name":"New Site","unique_group_name":""}]`
#' 2. CSV Example:
#' data_access_group_name,unique_group_name
#' "CA Site",ca_site
#' "FL Site",fl_site
#' "New Site",
#' #' XML Example:
#' \out{
#' <?xml version="1.0" encoding="UTF-8" ?>
#' <dags>
#'   <item>
#'   <data_access_group_name>CA Site</data_access_group_name>
#'   <unique_group_name>ca_site</unique_group_name>
#'   </item>
#'   <item>
#'   <data_access_group_name>FL Site</data_access_group_name>
#'   <unique_group_name>fl_site</unique_group_name>
#'   </item>
#'   <item>
#'   <data_access_group_name>New Site</data_access_group_name>
#'   <unique_group_name></unique_group_name>
#'   </item>
#' </dags>
#' }
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return [httr::response] object containing number of DAGs added or updated
#' @export
redcap_import_dags <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json"),
           data,
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "dag",
      "action" = "import",
      "format" = match.arg(format),
      "data" = data,
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

## Events ----------------------------------------------------------------------

## Field Names -----------------------------------------------------------------

#' Export List of Export Field Names
#'
#' @description This method returns a list of the export/import-specific
#' version of field names for all fields (or for one field, if desired) in a
#' project. This is mostly used for checkbox fields because during data exports
#' and data imports, checkbox fields have a different variable name used than
#' the exact one defined for them in the Online Designer and Data Dictionary,
#' in which *each checkbox option* gets represented as its own export field
#' name in the following format: field_name + triple underscore + converted
#' coded value for the choice. For non-checkbox fields, the export field name
#' will be exactly the same as the original field name. Note: The following
#' field types will be automatically removed from the list returned by this
#' method since they cannot be utilized during the data import process: 'calc',
#' 'file', and 'descriptive'.
#'
#' The list that is returned will contain the three following attributes for
#' each field/choice: 'original_field_name', 'choice_value', and
#' 'export_field_name'. The choice_value attribute represents the raw coded
#' value for a checkbox choice. For non-checkbox fields, the choice_value
#' attribute will always be blank/empty. The export_field_name attribute
#' represents the export/import-specific version of that field name.
#'
#' @note To use this method, you must have API Export privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml `[default]`
#' @param field A field's variable name. By default, all fields are returned,
#' but if field is provided, then it will only the export field name(s) for
#' that field. If the field name provided is invalid, it will return an error.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return [httr::response] object containing Returns a list of the
#' export/import-specific version of field names for all fields (or for one
#' field, if desired) in a project in the format specified and ordered by their
#' field order . The list that is returned will contain the three following
#' attributes for each field/choice: 'original_field_name', 'choice_value',
#' and 'export_field_name'. The choice_value attribute represents the raw
#' coded value for a checkbox choice. For non-checkbox fields, the choice_value
#' attribute will always be blank/empty. The export_field_name attribute
#' represents the export/import-specific version of that field name.
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_export_field_names(
#'   token = my_token,
#'   format = "csv",
#'   field = "checkboxes"
#' ) %>%
#'   httr::content(
#'     as = "text",
#'     encoding = "UTF-8",
#'     content_type = "text/csv"
#'   ) %>%
#'   readr::read_csv() %>%
#'   dplyr::pull(export_field_name)
#' }
redcap_export_field_names <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json"),
           field,
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "exportFieldNames",
      "format" = match.arg(format),
      "field" = validate_arg(field),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

## Files -----------------------------------------------------------------------

#' Export a File
#'
#' @description This method allows you to download a document that has been
#' attached to an individual record for a File Upload field. Please note that
#' this method may also be used for Signature fields (i.e. File Upload fields
#' with 'signature' validation type).
#'
#' @note Please be aware that Data Export user rights will be applied to this
#' API request. For example, if you have 'No Access' data export rights in the
#' project, then the API file export will fail and return an error. And if you
#' have 'De-Identified' or 'Remove all tagged Identifier fields' data export
#' rights, then the API file export will fail and return an error *only if* the
#' File Upload field has been tagged as an Identifier field. To make sure that
#' your API request does not return an error, you should have 'Full Data Set'
#' export rights in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
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
#' @return [httr::response] object containing the path to the file on disk
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_export_file(
#'   redcap_uri =
#'     "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
#'   token = my_project_token,
#'   record = 1,
#'   field = "file_field_name"
#' )
#' }
redcap_export_file <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           record,
           field,
           event,
           repeat_instance,
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "file",
      "action" = "export",
      "record" = record,
      "field" = field,
      "event" = validate_arg(event),
      "repeat_instance" = validate_arg(repeat_instance,
        v = checkmate::check_integer
      ),
      "returnFormat" = match.arg(return_format)
    )

    # write to temp file because do not know name yet
    tmp <- tempfile()
    r <- httr::POST(
      redcap_uri,
      body = body,
      encode = "form",
      httr::write_disk(tmp)
    )

    # retrieve the desired file name from header and construct absolute path
    fname <- parse_content_type(r[["headers"]][["content-type"]])[["name"]]
    fpath <- gsub("\\\\", "/", file.path(tempdir(), fname))

    # rename temp file to desired
    file.rename(r$content[[1]], fpath)

    # edit httr response object to reflect new download name
    content_type <- sub(basename(tmp), fname, r[["headers"]][["content-type"]])
    attr(fpath, "class") <- "path"
    r[["headers"]][["content-type"]] <- content_type
    r[["all_headers"]][[1]][["headers"]][["content-type"]] <- content_type
    r[["content"]] <- fpath
    r[["request"]][["output"]][["path"]] <- fpath[[1]]

    r
  }

#' Import a File
#'
#' @description This method allows you to upload a document that will be
#' attached to an individual record for a File Upload field. Please note that
#' this method may NOT be used for Signature fields (i.e. File Upload fields
#' with 'signature' validation type) because a signature can only be captured
#' and stored using the web interface.
#'
#' @note To use this method, you must have API Import/Update privileges in the
#' project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param record the record ID
#' @param field the name of the field that contains the file
#' @param event the unique event name - only for longitudinal projects
#' @param repeat_instance (only for projects with repeating instruments/events)
#' The repeat instance number of the repeating event (if longitudinal) or the
#' repeating instrument (if classic or longitudinal). Default value is '1'.
#' @param file path to the file to upload
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return [httr::response] object reporting empty body on successful upload
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_import_file(
#'   redcap_uri =
#'     "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
#'   token = my_project_token,
#'   record = 1,
#'   field = "file_field_name"
#' )
#' }
redcap_import_file <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           record,
           field,
           event,
           repeat_instance,
           file,
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "file",
      "action" = "import",
      "record" = record,
      "field" = field,
      "event" = validate_arg(event),
      "repeat_instance" = validate_arg(repeat_instance,
        v = checkmate::check_integer
      ),
      "file" = httr::upload_file(file),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "multipart")
  }

## Instruments -----------------------------------------------------------------

## Metadata --------------------------------------------------------------------

## Projects --------------------------------------------------------------------

#' Create A New Project
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
#' @param format format of the data argument
#' @param data Contains the attributes of the project to be created, in which
#' they are provided in the specified format. While the only required attributes
#' are 'project_title' and 'purpose', the fields listed below are all the
#' possible attributes that can be provided in the 'data' parameter. The
#' 'purpose' attribute must have a numerical value (0=Practice/Just for fun,
#' 1=Other, 2=Research, 3=Quality Improvement, 4=Operational Support), in which
#' 'purpose_other' is only required to have a value (as a text string) if
#' purpose=1. The attributes is_longitudinal (0=False, 1=True; Default=0),
#' surveys_enabled (0=False, 1=True; Default=0), and
#' record_autonumbering_enabled (0=False, 1=True; Default=1) are all boolean.
#' Please note that either is_longitudinal=1 or surveys_enabled=1 does not add
#' arms/events or surveys to the project, respectively, but it merely enables
#' those settings which are seen at the top of the project's Project Setup page.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param odm default: NULL - The 'odm' parameter must be an XML string in CDISC
#' ODM XML format that contains project metadata (fields, forms, events, arms)
#' and might optionally contain data to be imported as well. The XML contained
#' in this parameter can come from a REDCap Project XML export file from REDCap
#' itself, or may come from another system that is capable of exporting
#' projects and data in CDISC ODM format. If the 'odm' parameter is included in
#' the API request, it will use the XML to import its contents into the newly
#' created project. This will allow you not only to create the project with the
#' API request, but also to import all fields, forms, and project attributes
#' (and events and arms, if longitudinal) as well as record data all at the same
#' time.
#'
#' @return an httr response object containing a 32-character project-level API
#' token (associated with both the project and user creating the project).
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_create_project(
#'   token = my_super_token,
#'   format = "json",
#'   data = paste0(
#'     "[",
#'     jsonlite::toJSON(
#'       list(
#'         "project_title" = "My New REDCap Project",
#'         "purpose" = "0"
#'       ),
#'       auto_unbox = TRUE
#'     ),
#'     "]"
#'   )
#' ) %>%
#'   httr::content(as = "text") -> my_project_token
#' }
redcap_create_project <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json"),
           data,
           return_format = c("xml", "csv", "json"),
           odm) {
    body <- list(
      "token" = token,
      "content" = "project",
      "format" = format,
      "data" = data,
      "returnFormat" = match.arg(return_format),
      "odm" = validate_arg(odm)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Import Project Information
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The Super API Token specific to a user
#' @param format format of the data argument
#' @param data Contains some or all of the attributes from Export Project
#' Information in the same data format as in the export. These attributes will
#' change the project information. Attributes for the project in the format
#' specified. For any values that are boolean, they should be represented as
#' either a '0' (no/false) or '1' (yes/true). The following project attributes
#' can be udpated: project_title, project_language, purpose, purpose_other,
#' project_notes, custom_record_label, secondary_unique_field, is_longitudinal,
#' surveys_enabled, scheduling_enabled, record_autonumbering_enabled,
#' randomization_enabled, project_irb_number, project_grant_number,
#' project_pi_firstname, project_pi_lastname, display_today_now_button
#'
#' @return Returns the number of values accepted to be updated in the project
#' settings (including values which remained the same before and after the
#' import).
#' @export
#'
#' @examples
#' \dontrun{
#' ## xml
#' redcap_export_project_info(token = my_src_token) %>%
#'   httr::content() %>%
#'   as.character() -> xml_string_of_source_info
#'
#' redcap_import_project_info(
#'   token = my_dst_token,
#'   data = xml_string_of_source_info
#' ) %>%
#'   httr::content(as = "text")
#'
#' ## json
#' redcap_export_project_info(
#'   token = my_src_token,
#'   format = "json"
#' ) %>%
#'   httr::content() -> list_of_source_info
#'
#' redcap_import_project_info(
#'   token = my_dst_token,
#'   format = "json",
#'   data = jsonlite::toJSON(list_of_source_info, auto_unbox = TRUE)
#' ) %>%
#'   httr::content(as = "text")
#' }
redcap_import_project_info <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json"),
           data) {
    body <- list(
      "token" = token,
      "content" = "project_settings",
      "format" = match.arg(format),
      data = data
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Export Project Information
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format format of the response content
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return [httr::response] object containing the project information
#' @export
redcap_export_project_info <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json"),
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "project",
      "format" = match.arg(format),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Export Entire Project as REDCap XML File (containing metadata & data)
#'
#' @description The entire project (all records, events, arms, instruments,
#' fields, and project attributes) can be downloaded as a single XML file, which
#'  is in CDISC ODM format (ODM version 1.3.1). This XML file can be used to
#'  create a clone of the project (including its data, optionally) on this
#'  REDCap server or on another REDCap server (it can be uploaded on the Create
#'  New Project page). Because it is in CDISC ODM format, it can also be used to
#'   import the project into another ODM-compatible system. NOTE: All the option
#'    paramters listed below ONLY apply to data returned if the
#'    'returnMetadataOnly' parameter is set to FALSE (default). For this API
#'    method, ALL metadata (all fields, forms, events, and arms) will always be
#'    exported. Only the data returned can be filtered using the optional
#'    parameters.
#'
#' Note about export rights: If the 'returnMetadataOnly' parameter is set to
#' FALSE, then please be aware that Data Export user rights will be applied to
#' any data returned from this API request. For example, if you have
#' 'De-Identified' or 'Remove all tagged Identifier fields' data export rights,
#' then some data fields *might* be removed and filtered out of the data set
#' returned from the API. To make sure that no data is unnecessarily filtered
#' out of your API request, you should have 'Full Data Set' export rights in the
#'  project.
#'
#' @note To use this method, you must have API Export privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param return_metadata_only TRUE returns only metadata (all fields, forms,
#' events, and arms), whereas FALSE returns all metadata and also data (and
#' optionally filters the data according to any of the optional parameters
#' provided in the request)
#' @param records an array of record names specifying specific records you wish
#' to pull (by default, all records are pulled)
#' @param fields an array of field names specifying specific fields you wish to
#' pull (by default, all fields are pulled)
#' @param events an array of unique event names that you wish to pull records
#' for - only for longitudinal projects
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param export_survey_fields specifies whether or not to export the survey
#' identifier field (e.g., 'redcap_survey_identifier') or survey timestamp
#' fields (e.g., instrument+'_timestamp') when surveys are utilized in the
#' project. If you do not pass in this flag, it will default to 'false'. If set
#' to 'true', it will return the redcap_survey_identifier field and also the
#' survey timestamp field for a particular survey when at least one field from
#' that survey is being exported. NOTE: If the survey identifier field or survey
#'  timestamp fields are imported via API data import, they will simply be
#'  ignored since they are not real fields in the project but rather are
#'  pseudo-fields.
#' @param export_data_access_groups specifies whether or not to export the
#' 'redcap_data_access_group' field when data access groups are utilized in the
#' project. If you do not pass in this flag, it will default to 'false'. NOTE:
#' This flag is only viable if the user whose token is being used to make the
#' API request is *not* in a data access group. If the user is in a group, then
#' this flag will revert to its default value.
#' @param filter_logic String of logic text (e.g., \[age\] > 30) for filtering
#' the data to be returned by this API method, in which the API will only return
#'  the records (or record-events, if a longitudinal project) where the logic
#'  evaluates as TRUE. This parameter is blank/null by default unless a value is
#'   supplied. Please note that if the filter logic contains any incorrect
#'   syntax, the API will respond with an error message.
#' @param export_files TRUE will cause the XML returned to include all files
#' uploaded for File Upload and Signature fields for all records in the project,
#'  whereas FALSE will cause all such fields not to be included. NOTE: Setting
#'  this option to TRUE can make the export very large and may prevent it from
#'  completing if the project contains many files or very large files.
#'
#' @return [httr::response] object
#' @export
#'
#' @examples
#' \dontrun{
#' ## full export from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_export_project_xml(
#'   token,
#'   exportFiles = TRUE
#' )
#'
#' ## full export from mother
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 6785)$token
#' redcap_export_project_xml(
#'   token,
#'   exportFiles = TRUE
#' )
#'
#' ## one record and two fields from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_export_project_xml(
#'   token,
#'   records = redcap_array("records", 16227),
#'   fields = redcap_array("fields", c("id", "updatedate"))
#' )
#' }
redcap_export_project_xml <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           return_metadata_only = FALSE,
           records,
           fields,
           events,
           return_format = c("xml", "json", "csv"),
           export_survey_fields = FALSE,
           export_data_access_groups = FALSE,
           filter_logic = NULL,
           export_files = FALSE) {
    # https://community.projectredcap.org/questions/81879/
    # api-project-xml-export-missing-redcapsurveysgroup.html
    # - bug reported in v9.7.7 saying xml file missing many project settings
    # - essentially this endpoint can only export metadata + data only in
    # versions prior to v9

    body <- list(
      "token" = token,
      "content" = "project_xml",
      "format" = "xml", # not in api dox
      "returnMetadataOnly" = tolower(return_metadata_only),
      "records" = validate_arg(records, v = assert_redcap_array),
      "fields" = validate_arg(fields, v = assert_redcap_array),
      "events" = validate_arg(events, v = assert_redcap_array),
      "returnFormat" = match.arg(return_format),
      "exportSurveyFields" = tolower(export_survey_fields),
      "exportDataAccessGroups" = tolower(export_data_access_groups),
      "filterLogic" = validate_arg(filter_logic),
      "exportFiles" = tolower(export_files)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

## Records ---------------------------------------------------------------------

#' Export Records
#'
#' @description This method allows you to export a set of records for a project.
#'
#' @note Note about export rights: Please be aware that Data Export user rights
#' will be applied to this API request. For example, if you have 'No Access'
#' data export rights in the project, then the API data export will fail and
#' return an error. And if you have 'De-Identified' or 'Remove all tagged
#' Identifier fields' data export rights, then some data fields *might* be
#' removed and filtered out of the data set returned from the API. To make sure
#' that no data is unnecessarily filtered out of your API request, you should
#' have 'Full Data Set' export rights in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The Super API Token specific to a user
#' @param format csv, json, xml `[default]`, odm ('odm' refers to CDISC ODM XML
#' format, specifically ODM version 1.3.1)
#' @param type * 'record' refers to the record ID for the project
#' \itemize{
#'   \item{flat}{- output as one record per row `[default]`}
#'   \item{eav}{- output as one data point per row
#'     \item{Non-longitudinal:}{record*, field_name, value}
#'     \item{Longitudinal:}{record*, field_name, value, redcap_event_name}
#'   }
#' }
#' @param records an array of record names specifying specific records you wish
#' to pull (by default, all records are pulled)
#' @param fields an array of field names specifying specific fields you wish to
#' pull (by default, all fields are pulled)
#' @param forms an array of form names you wish to pull records for. If the
#' form name has a space in it, replace the space with an underscore (by
#' default, all records are pulled)
#' @param events an array of unique event names that you wish to pull records
#' for - only for longitudinal projects
#' @param raw_or_label raw `[default]`, label - export the raw coded values or
#' labels for the options of multiple choice fields
#' @param raw_or_label_headers raw `[default]`, label - (for 'csv' format 'flat'
#' type only) for the CSV headers, export the variable/field names (raw) or the
#' field labels (label)
#' @param export_checkbox_label true, false `[default]` - specifies the format
#' of checkbox field values specifically when exporting the data as labels
#' (i.e., when rawOrLabel=label) in flat format (i.e., when type=flat). When
#' exporting labels, by default (without providing the exportCheckboxLabel
#' flag or if exportCheckboxLabel=false), all checkboxes will either have a
#' value Checked' if they are checked or 'Unchecked' if not checked. But if
#' exportCheckboxLabel is set to true, it will instead export the checkbox
#' value as the checkbox option's label (e.g., 'Choice 1') if checked or it
#' will be blank/empty (no value) if not checked. If rawOrLabel=false or if
#' type=eav, then the exportCheckboxLabel flag is ignored. (The
#' exportCheckboxLabel parameter is ignored for type=eav because 'eav' type
#' always exports checkboxes differently anyway, in which checkboxes are
#' exported with their true variable name (whereas the 'flat' type exports them
#' as variable___code format), and another difference is that 'eav' type
#' *always* exports checkbox values as the choice label for labels export, or
#' as 0 or 1 (if unchecked or checked, respectively) for raw export.)
#' @param return_format csv, json, xml - specifies the format of error
#' messages. If you do not pass in this flag, it will select the default format
#' for you passed based on the 'format' flag you passed in or if no format flag
#' was passed in, it will default to 'xml'.
#' @param export_survey_fields true, false `[default]` - specifies whether or
#' not to export the survey identifier field (e.g., 'redcap_survey_identifier')
#' or survey timestamp fields (e.g., instrument+'_timestamp') when surveys are
#' utilized in the project. If you do not pass in this flag, it will default to
#' 'false'. If set to 'true', it will return the redcap_survey_identifier field
#' and also the survey timestamp field for a particular survey when at least
#' one field from that survey is being exported. NOTE: If the survey identifier
#' field or survey timestamp fields are imported via API data import, they will
#' simply be ignored since they are not real fields in the project but rather
#' are pseudo-fields.
#' @param export_data_access_groups true, false `[default]` - specifies whether
#' or not to export the 'redcap_data_access_group' field when data access
#' groups are utilized in the project. If you do not pass in this flag, it will
#' default to 'false'. NOTE: This flag is only viable if the user whose token
#' is being used to make the API request is *not* in a data access group. If
#' the user is in a group, then this flag will revert to its default value.
#' @param filter_logic String of logic text (e.g., `[age]` > 30) for filtering
#' the data to be returned by this API method, in which the API will only
#' return the records (or record-events, if a longitudinal project) where the
#' logic evaluates as TRUE. This parameter is blank/null by default unless a
#' value is supplied. Please note that if the filter logic contains any
#' incorrect syntax, the API will respond with an error message.
#' @param date_range_begin To return only records that have been created or
#' modified *after* a given date/time, provide a timestamp in the format
#' YYYY-MM-DD HH:MM:SS (e.g., '2017-01-01 00:00:00' for January 1, 2017 at
#' midnight server time). If not specified, it will assume no begin time.
#' @param date_range_end To return only records that have been created or
#' modified *before* a given date/time, provide a timestamp in the format
#' YYYY-MM-DD HH:MM:SS (e.g., '2017-01-01 00:00:00' for January 1, 2017 at
#' midnight server time). If not specified, it will use the current server time.
#' @param csv_delimiter Set the delimiter used to separate values in the CSV
#' data file (for CSV format only). Options include: comma ',' (default),
#' 'tab', semi-colon ';', pipe '|', or caret '^'. Simply provide the value in
#' quotes for this parameter.
#' @param decimal_character If specified, force all numbers into same decimal
#' format. You may choose to force all data values containing a decimal to have
#' the same decimal character, which will be applied to all calc fields and
#' number-validated text fields. Options include comma ',' or dot/full stop '.',
#' but if left blank or null, then it will export numbers using the fields'
#' native decimal format. Simply provide the value of either ',' or '.' for
#' this parameter.
#'
#' @return [httr::response] object
#' @export
#'
#' @examples \dontrun{
#' redcap_export_records(token = my_token)
#' }
redcap_export_records <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json", "odm"),
           type = c("flat", "eav"),
           records,
           fields,
           forms,
           events,
           raw_or_label = c("raw", "label"),
           raw_or_label_headers = c("raw", "label"),
           export_checkbox_label = FALSE,
           return_format = c("xml", "csv", "json"),
           export_survey_fields = FALSE,
           export_data_access_groups = FALSE,
           filter_logic,
           date_range_begin,
           date_range_end,
           csv_delimiter = c(",", "\t", ";", "|", "^"),
           decimal_character = c("native", ",", ".")) {
    body <- list(
      "token" = token,
      "content" = "record",
      "format" = match.arg(format),
      "type" = match.arg(type),
      "records" = validate_arg(records, v = assert_redcap_array),
      "fields" = validate_arg(fields, v = assert_redcap_array),
      "forms" = validate_arg(forms, v = assert_redcap_array),
      "events" = validate_arg(events, v = assert_redcap_array),
      "rawOrLabel" = match.arg(raw_or_label),
      "rawOrLabelHeaders" = match.arg(raw_or_label_headers),
      "exportCheckboxLabel" = export_checkbox_label,
      "returnFormat" = match.arg(return_format),
      "exportSurveyFields" = export_survey_fields,
      "exportDataAccessGroups" = export_data_access_groups,
      "filterLogic" = validate_arg(filter_logic),
      "dateRangeBegin" = validate_arg(date_range_begin),
      "dateRangeEnd" = validate_arg(date_range_end),
      "csvDelimiter" = match.arg(csv_delimiter),
      "decimalCharacter" = match.arg(decimal_character)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Import Records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml \[default\], odm ('odm' refers to CDISC ODM XML
#' format, specifically ODM version 1.3.1)
#' @param type
#' * flat - output as one record per row \[default\]
#' * eav - input as one data point per row
#'     + Non-longitudinal: Will have the fields - record(1), field_name, value
#'     + Longitudinal: Will have the fields - record*, field_name, value,
#'     redcap_event_name(2)
#'
#' 1. 'record' refers to the record ID for the project
#' 2. Event name is the unique name for an event, not the event label
#' @param overwrite_behavior
#' * normal - blank/empty values will be ignored \[default\]
#' * overwrite - blank/empty values are valid and will overwrite data
#' @param force_auto_number If record auto-numbering has been enabled in the
#' project, it may be desirable to import records where each record's record
#' name is automatically determined by REDCap (just as it does in the user
#' interface). If this parameter is set to 'true', the record names provided in
#' the request will not be used (although they are still required in order to
#' associate multiple rows of data to an individual record in the request), but
#' instead those records in the request will receive new record names during the
#'  import process. NOTE: To see how the provided record names get translated
#'  into new auto record names, the returnContent parameter should be set to
#'  'auto_ids', which will return a record list similar to 'ids' value, but it
#'  will have the new record name followed by the provided record name in the
#'  request, in which the two are comma-delimited. For example, if false (or
#'  'false') - The record names provided in the request will be used.
#'  \[default\] true (or 'true') - New record names will be automatically
#'  determined.
#' @param data The formatted data to be imported.
#'
#' TIP: If importing repeating instances for a repeating event or repeating
#' instrument, you may auto-number the instances by providing a value of 'new'
#' for the 'redcap_repeat_instance' field in the dataset you are importing. This
#'  is useful because it allows you to import such data without the need to
#'  determine how many instances already exist for a given repeating
#'  event/instance prior to the import. NOTICE: The 'new' value option for
#'  auto-numbering instances does NOT work for 'eav' type data but only for
#'  'flat' type.
#'
#' NOTE: When importing data in EAV type format, please be aware that checkbox
#' fields must have their field_name listed as variable+'___'+optionCode and its
#'  value as either '0' or '1' (unchecked or checked, respectively). For
#'  example, for a checkbox field with variable name 'icecream', it would be
#'  imported as EAV with the field_name as 'icecream___4' having a value of '1'
#'  in order to set the option coded with '4' (which might be 'Chocolate') as '
#'  checked'.
#' @param date_format MDY, DMY, YMD \[default\] - the format of values being
#' imported for dates or datetime fields (understood with M representing
#' 'month', D as 'day', and Y as 'year') - NOTE: The default format is Y-M-D
#' (with dashes), while MDY and DMY values should always be formatted as M/D/Y
#' or D/M/Y (with slashes), respectively.
#' @param csv_delimiter Set the delimiter used to separate values in the CSV
#' data file (for CSV format only). Options include: comma ',' (default), 'tab',
#'  semi-colon ';', pipe '|', or caret '^'. Simply provide the value in quotes
#'  for this parameter.
#' @param return_content count \[default\] - the number of records imported, ids
#'  - a list of all record IDs that were imported, auto_ids = (used only when
#'  forceAutoNumber=true) a list of pairs of all record IDs that were imported,
#'  includes the new ID created and the ID value that was sent in the API
#'  request (e.g., 323,10).
#' @param return_format csv, json, xml - specifies the format of error messages.
#'  If you do not pass in this flag, it will select the default format for you
#'  passed based on the 'format' flag you passed in or if no format flag was
#'  passed in, it will default to 'xml'.
#'
#' @return [httr::response] object containing the number of records deleted.
#' @export
#'
#' @examples
#' \dontrun{
#' ## csv eav example to edit a single data point
#' ### write data to disk and read back in to get correctly parsed csv string
#' csv_data <- data.frame(
#'   record = 2,
#'   field_name = "text_box",
#'   value = "a new value"
#' )
#' csv_file <- tempfile("data", fileext = ".csv")
#' write.csv(csv_data, csv_file, row.names = FALSE)
#' data <- paste(readLines(csv_file), collapse = "\n")
#'
#' ### retreive credentials
#' path <- system.file("misc/example.credentials", package = "REDCapR")
#' p1 <- REDCapR::retrieve_credential_local(path, 153L)
#'
#' ### submit api request and check response
#' httr::content(
#'   redcap_import_records(
#'     token = p1$token,
#'     format = "csv",
#'     type = "eav",
#'     data = data,
#'     return_format = "json"
#'   )
#' )
#' }
redcap_import_records <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           format = c("xml", "csv", "json", "odm"),
           type = c("flat", "eav"),
           overwrite_behavior = c("normal", "overwrite"),
           force_auto_number = FALSE,
           data,
           date_format = c("YMD", "MDY", "DMY"),
           csv_delimiter = c(",", "tab", ";", "|", "^"),
           return_content = c("count", "ids", "auto_ids"),
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "record",
      "format" = match.arg(format),
      "type" = match.arg(type),
      "overwriteBehavior" = match.arg(overwrite_behavior),
      "forceAutoNumber" = tolower(force_auto_number),
      "data" = data,
      "dateFormat" = match.arg(date_format),
      "csvDelimiter" = match.arg(csv_delimiter),
      "returnContent" = match.arg(return_content),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Delete Records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param records an array of record names specifying specific records you wish
#' to delete
#' @param arm the arm number of the arm in which the record(s) should be
#' deleted. (This can only be used if the project is longitudinal with more than
#'  one arm.) NOTE: If the arm parameter is not provided, the specified records
#'  will be deleted from all arms in which they exist. Whereas, if arm is
#'  provided, they will only be deleted from the specified arm.
#'
#' @return [httr::response] object containing the number of records deleted.
#' @export
#'
#' @examples
#' \dontrun{
#' ## delete two records from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_delete_records(
#'   token,
#'   records = redcap_array("records", c(16227, 16342))
#' )
#' }
redcap_delete_records <-
  function(redcap_uri = redcap_api_endpoints$prod$latest,
           token,
           records,
           arm) {
    body <- list(
      "token" = token,
      "content" = "record",
      "action" = "delete",
      "records" = validate_arg(records, v = assert_redcap_array),
      "arm" = validate_arg(arm, v = checkmate::assert_integer)
    )

    httr::POST(redcap_uri, body = body)
  }

## Repeating Instruments and Events --------------------------------------------

## Reports ---------------------------------------------------------------------

## REDCap ----------------------------------------------------------------------

## Surveys ---------------------------------------------------------------------

## Users & User Privileges -----------------------------------------------------

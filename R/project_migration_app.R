#' Project Migration App
#'
#' @description This shiny app migrates most project settings from one REDCap
#' instance to another by using API calls. It was written to help transfer
#' projects on a v7 instance to a v10 instance where version inconsistencies as
#' well as technical and labor issues prevented a simple CDISC ODM XML port.
#'
#' @param redcap_uri_src The URI (uniform resource identifier) of the source
#' REDCap project.
#' @param redcap_uri_dst The URI (uniform resource identifier) of the
#' destination REDCap project.
#' @param token_src The API token specific to your source REDCap project and
#' username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param token_spr The Super API Token specific to a user
#' @param redcap_dag_uri_src The base URI for the source DAG page
#' @param redcap_dag_uri_dst The base URI for the destination DAG page
#'
#' @export
redcap_project_migration_app <- function(
  redcap_uri_src = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  redcap_uri_dst = "https://redcap.wustl.edu/redcap/api/",
  token_src = "",
  token_spr = "",
  redcap_dag_uri_src = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/redcap_v7.3.5/DataAccessGroups/index.php?pid=",
  redcap_dag_uri_dst = "https://redcap.wustl.edu/redcap/redcap_v10.6.28/index.php?route=DataAccessGroupsController:index&pid="
) {
  ui <- function() {
    shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$script("
          Shiny.addCustomMessageHandler('close-window', function(x) {
            window.open('', '_self', '').close();
          });
        ")
      ),
      shiny::titlePanel("REDCap Project Migration App"),

      shiny::verticalLayout(
        shiny::passwordInput(
          "token_src",
          "Source Project Token",
          token_src,
          width = "100%"
        ),
        shiny::passwordInput(
          "token_spr",
          "Destination Super Token",
          token_spr,
          width = "100%"
        ),
        shiny::actionButton("migrate", "Migrate")
      )
    )
  }

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    rvs <- shiny::reactiveValues(
      token_dst = NULL # api token created during destination project creation
    )

    shiny::observeEvent(input$migrate, {
      shiny::withProgress({

        shiny::setProgress(0.00, "Initiating migration...")
        Sys.sleep(3)

        # these attributes will be copied to the new project during creation
        shiny::setProgress(0.05, "Exporting attributes from source...")
        redcap_export_project_info(
          redcap_uri = redcap_uri_src,
          token = input$token_src,
          format = "json"
        ) %>%
          httr::content() -> project_info_src

        # cdisc odm is the most complete/portable representation of the project
        # this will be used during the new project creation
        shiny::setProgress(0.10, "Exporting metadata from source...")
        redcap_export_project_xml(
          redcap_uri_src,
          input$token_src,
          return_metadata_only = TRUE
        ) %>%
          httr::content() %>%
          as.character() -> cdisc_odm_xml

        # create the new project and make a note of the project specific token
        shiny::setProgress(0.20, "Creating project at destination...")
        redcap_create_project(
          redcap_uri = redcap_uri_dst,
          token = input$token_spr,
          format = "json",
          data = paste0(
            "[", jsonlite::toJSON(project_info_src, auto_unbox = TRUE), "]"
          ),
          odm = cdisc_odm_xml
        ) %>%
          httr::content(as = "text", encoding = "UTF-8") -> rvs$token_dst

        # extract attributes from the new project to identify pid
        shiny::setProgress(0.30, "Exporting attributes from destination...")
        redcap_export_project_info(
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst,
          format = "json"
        ) %>%
          httr::content() -> project_info_dst

        Sys.sleep(3)
      })

      # data access groups definitions cannot be exported from v7 via the api
      # here we help the user manually define them as much as possible
      # if we allow database access here is sql query for use in conjunction
      # with redcap_import_dags():
      # SELECT * FROM redcap_data_access_groups WHERE project_id='6648'
      shinyalert::shinyalert(
        title = "DAGs?",
        text = paste0(
          "<p>Because data access groups (DAGs) definitions cannot be exported ",
          "via the API from REDCap v7, they must be manually migrated.</p><br/>",
          "<p>View the definitions in the source project ",
          "<a href=\"",
          redcap_dag_uri_src, project_info_src$project_id,
          "\" target = \"_blank\">here</a>.</p><br/>",
          "<p>Enter the definitions in the destination project ",
          "<a href=\"",
          redcap_dag_uri_dst, project_info_dst$project_id,
          "\" target = \"_blank\">here</a>.</p><br/>",
          "<p>Click OK after all DAGs have been manually migrated.</p><br/>"
        ),
        type = "info",
        html = TRUE,
        inputId = "dags_defined"
      )
    })

    # after dags have been dealt with
    shiny::observeEvent(input$dags_defined, {
      shiny::withProgress({
        # export records
        shiny::setProgress(0.35, "Exporting records from source...")
        REDCapR::redcap_read(
          redcap_uri = redcap_uri_src,
          token = input$token_src,
          export_survey_fields = TRUE,
          export_data_access_groups = TRUE
        )$data -> project_records

        # import records
        shiny::setProgress(0.45, "Importing records to destination...")
        REDCapR::redcap_write(
          ds_to_write = project_records,
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst
        )

        # read the data dictionary to:
        # a) identify fields of type file (this includes uploads and signatures)
        # b) to temporarily change signature fields to regular upload fields as
        #    the api does not allow signatures to be uploaded, but we can trick
        #    it by setting signature fields to be regular upload fields while
        #    uploading files and then reverting after the fact
        # Note: while signatures are also files, they are not importable:
        # <?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<hash>\n  <error>The field
        # 'var_8' is a signature field, which cannot be imported using the API but
        # can only be created using the web interface. However, it can be
        # downloaded or deleted using the API.</error>\n</hash>\n
        shiny::setProgress(0.60, "Reading data dictionary...")
        REDCapR::redcap_metadata_read(
          redcap_uri = redcap_uri_src,
          token = input$token_src
        )$data -> data_dictionary

        # temporarily change signature fields into regular upload fields
        shiny::setProgress(0.60, "Disabling signature constraints...")
        REDCapR::redcap_metadata_write(
          ds = data_dictionary %>%
            dplyr::mutate(
              text_validation_type_or_show_slider_number =
                dplyr::if_else(
                  text_validation_type_or_show_slider_number == "signature",
                  NA_character_,
                  text_validation_type_or_show_slider_number
                )
            ),
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst
        )

        # get names of fields of type file
        data_dictionary %>%
          dplyr::filter(field_type == "file") %>%
          dplyr::pull(field_name) -> field_names_files

        # compute a delta for updating the progress bar
        progress_field_delta <- (0.90 - 0.60) / length(field_names_files)

        # for each file field find records requiring a file migration and do it
        for(j in 1:length(field_names_files)) {
          shiny::setProgress(
            0.6 + j * progress_field_delta,
            sprintf("Transfering files for field: %s...", field_names_files[j]),
            ""
          )

          # find records with files in the current field
          project_records %>%
            dplyr::select(
              dplyr::any_of(
                c(
                  data_dictionary[[1, 1]],
                  "redcap_event_name",
                  "redcap_repeat_instance",
                  field_names_files[j]
                )
              )
            ) %>%
            dplyr::filter(
              !is.na(.data[[field_names_files[j]]])
            ) -> d_files_to_migrate

          # migrate the files
          for(k in 1:nrow(d_files_to_migrate)) {
            redcap_export_file(
              redcap_uri = redcap_uri_src,
              token = input$token_src,
              record = d_files_to_migrate[[k, data_dictionary[[1, 1]]]],
              field = field_names_files[j],
              event = d_files_to_migrate[[k, "redcap_event_name"]],
              repeat_instance = as.integer(
                d_files_to_migrate[[k, "redcap_repeat_instance"]]
              )
            ) -> r_export

            shiny::setProgress(
              message = sprintf(
                "Transfering files for field: %s...", field_names_files[j]
              ),
              detail = sprintf(
                "Downloading file: %s...", basename(r_export$content[[1]])
              )
            )

            redcap_import_file(
              token = rvs$token_dst,
              redcap_uri = redcap_uri_dst,
              record = d_files_to_migrate[[k, data_dictionary[[1, 1]]]],
              field = field_names_files[j],
              event = d_files_to_migrate[[k, "redcap_event_name"]],
              repeat_instance = as.integer(
                d_files_to_migrate[[k, "redcap_repeat_instance"]]
              ),
              file = r_export$content[[1]]
            ) -> r_import

            shiny::setProgress(
              message = sprintf(
                "Transfering files for field: %s...", field_names_files[j]
              ),
              detail = sprintf(
                "Uploading file: %s...", basename(r_export$content[[1]])
              )
            )
          }
        }

        # add signature constraint back to original signature fields
        shiny::setProgress(0.90, "Enabling signature constraints...")
        REDCapR::redcap_metadata_write(
          ds = data_dictionary,
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst
        )

        shiny::setProgress(1.00, "Completing migration...", "")
        Sys.sleep(3)
      })

      shinyalert::shinyalert(
        title = "Migration Complete",
        text = "Project migration has completed.",
        type = "info",
        inputId = "migration_complete"
      )
    })

    shiny::observeEvent(input$migration_complete, {
      session$sendCustomMessage("close-window", "")
    })
  }

  shiny::runGadget(ui(), server, viewer = shiny::browserViewer())
}

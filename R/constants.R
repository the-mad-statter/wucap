#' REDCap API Endpoints
#'
#' Constants for various API endpoints
redcap_api_endpoints <- list(
  "dev" = list(
    "v12.4.31" =
      "https://redcapdev.wustl.edu/redcap/api/",
    "latest" =
      "https://redcapdev.wustl.edu/redcap/api/"
  ),
  "qa" = list(
    "v12.4.31" =
      "https://redcapqa.wustl.edu/redcap/api/",
    "latest" =
      "https://redcapqa.wustl.edu/redcap/api/"
  ),
  "prod" = list(
    "v7.3.5" =
      "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
    "v12.4.31" =
      "https://redcap.wustl.edu/redcap/api/",
    "latest" =
      "https://redcap.wustl.edu/redcap/api/"
  )
)

#' REDCap Data Access Group Endpoints
#'
#' Constants for various DAG endpoints
redcap_dag_endpoints <- list(
  "dev" = list(
    "v12.4.31" = paste0(
      "https://redcapdev.wustl.edu/redcap/redcap_v12.4.31/",
      "index.php?route=DataAccessGroupsController:index&pid="
    ),
    "latest" = paste0(
      "https://redcapdev.wustl.edu/redcap/redcap_v12.4.31/",
      "index.php?route=DataAccessGroupsController:index&pid="
    )
  ),
  "qa" = list(
    "v12.4.31" = paste0(
      "https://redcapqa.wustl.edu/redcap/redcap_v12.4.31/",
      "index.php?route=DataAccessGroupsController:index&pid="
    ),
    "latest" = paste0(
      "https://redcapqa.wustl.edu/redcap/redcap_v12.4.31/",
      "index.php?route=DataAccessGroupsController:index&pid="
    )
  ),
  "prod" = list(
    "v7.3.5" = paste0(
      "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/",
      "redcap/redcap_v7.3.5/DataAccessGroups/index.php?pid="
    ),
    "v12.4.31" = paste0(
      "https://redcap.wustl.edu/redcap/redcap_v12.4.31/",
      "index.php?route=DataAccessGroupsController:index&pid="
    ),
    "latest" = paste0(
      "https://redcap.wustl.edu/redcap/redcap_v12.4.31/",
      "index.php?route=DataAccessGroupsController:index&pid="
    )
  )
)

#' REDCap API Endpoints
#'
#' Constants for various API endpoints
redcap_api_endpoints <- list(
  "prod" = list(
    "v7" = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
    "latest" = "https://redcap.wustl.edu/redcap/api/"
  )
)

#' REDCap Data Access Group Endpoints
#'
#' Constants for various DAG endpoints
redcap_dag_endpoints <- list(
  "prod" = list(
    "v7" = paste0(
      "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/",
      "redcap/redcap_v7.3.5/DataAccessGroups/index.php?pid="
    ),
    "latest" = paste0(
      "https://redcap.wustl.edu/redcap/redcap_v10.6.28/",
      "index.php?route=DataAccessGroupsController:index&pid="
    )
  )
)

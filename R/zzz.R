.onAttach <- function(...) {
  if (!is_attached("dplyr")) {
    library(dplyr)
  }
}

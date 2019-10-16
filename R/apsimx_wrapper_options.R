#' @title Getting a apsimx_wrapper options list with initialized fields
#'
#' @description This function returns a default options list
#'
#' @param apsimx_path Path of the apsimx binary executable file (Models.exe)
#'
#' @param apsimx_file Path to the .apsimx file to be run
#'
#' @param predicted_table_name Name of the predicted table in the datastore
#'
#' @param observed_table_name Name of the observed table in the datastore
#'
#' @param time_display Logical value used to display (TRUE) or not (FALSE)
#' simulations duration
#'
#' @param multi_process Logical value used to run (TRUE) or not (FALSE) apsim
#' in multi-process mode. This will generally result in faster execution times
#' on systems with many cores (ie in a HPC/cluster environment).
#'
#' @return A list containing apsimx model apsimx_wrapper options
#'
#' @examples
#'
#' @export
#'
apsimx_wrapper_options <- function(apsimx_path,
                                   apsimx_file, ... ) {

  # Template list
  options <- list()
  options$apsimx_path <- character(0)
  options$apsimx_file <- character(0)
  options$time_display <- FALSE
  options$warning_display <- TRUE
  options$multi_process <- FALSE
  options$predicted_table_name <- 'Report'
  options$observed_table_name <- 'Observed'
  options$variable_names <- character(0)
  options$met_files_path <- character(0)

  # For getting the template
  # running apsimx_wrapper_options
  if (! nargs()) return(options)

  # For fixing mandatory fields values
  options$apsimx_path <- apsimx_path
  options$apsimx_file <- apsimx_file

  # Fixing optional fields,
  # if corresponding to exact field names
  # in options list
  list_names <- names (options)
  add_args <- list(...)

  for (n in names(add_args)) {
    if ( n %in% list_names) {
      options[[n]] <- add_args[[n]]
    }
  }

  return(options)
}


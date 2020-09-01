#' @title Changing .apsimx file parameters values
#'
#' @description This function apply parameters values changes in an . apsimx
#' using a paarameters named vector of values
#'
#' @param file_to_run a .apsimx file path
#'
#' @param param_values a named vector of parameters values
#'
#' @return TRUE if changes are successful, FALSE otherwise
#'
#' @export
#'
change_apsimx_param <- function(exe, file_to_run, param_values) {


  # Generate config file containing parameter changes ---------------------------
  config_file <- tempfile('apsimOnR', fileext = '.conf')
  parameter_names <- names(param_values)
  fileConn <- file(config_file)
  lines <- vector("character", length(param_values))
  for (i in 1:length(param_values))
    lines[i] <- paste(parameter_names[i], '=', as.character(param_values[i]))
  writeLines(lines, fileConn)
  close(fileConn)

  # Apply parameter changes to the model -----------------------------------------
  cmd <- paste(exe, file_to_run, '/Edit', config_file)
  if (.Platform$OS.type == 'unix') {
    # need to run via mono on unices
    cmd <- paste('mono', cmd)
  }
  #edit_file_stdout <- shell(cmd, translate = FALSE, intern = TRUE, mustWork = TRUE)
  edit_file_stdout <- system(cmd, wait = TRUE, intern = TRUE)

  #print(edit_file_stdout)

  # returning the changes status
  return(is.null(attr(edit_file_stdout,"status")))

}

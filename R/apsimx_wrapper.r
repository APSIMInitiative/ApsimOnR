
#' @title Running .apsimx files from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses ApsimX directly through a system call, can
#' force ApsimX input parameters with values given in arguments.
#'
#' @param param_values named vector containing the value(s) and names of the
#' parameters to force (optional)
#'
#' @param sit_var_dates_mask List of situations, variables and dates for which
#' simulated values should be returned. Typically a list containing the
#' observations to which simulations should be compared as provided by
#' apsimxRFiles::read_obs_to_list
#'
#' @param prior_information Prior information on the parameters to estimate.
#' For the moment only uniform distribution are allowed.
#' Either a list containing (named) vectors of upper and lower
#' bounds (\code{ub} and \code{lb}), or a named list containing for each
#' parameter the list of situations per group (\code{sit_list})
#' and the vector of upper and lower bounds (one value per group) (\code{ub} and \code{lb})
#'
#' @param model_options List containing any information needed by the model.
#' In the case of apsimx: \code{apsimx_path} the path of apsimx executable file and
#' \code{apsimx_file} the path of the directory containing the apsimx input data
#' for each USM (one folder per USM where apsimx input files are stored in txt
#' format)
#'
#' @return A list containing simulated values (\code{sim_list}) and a flag
#' (\code{flag_allsim}) indicating if all required situations, variables and
#' dates were simulated.
#'
#' @examples
#'
#' @export
#'
apsimx_wrapper <- function( param_values=NULL, sit_var_dates_mask=NULL,
                           prior_information=NULL, model_options ) {

  # TODO : make a function dedicated to checking model_options
  # Because it may be model dependant, so it could be possible to pass anything
  # useful in the model running function...
  # Reuse next lines before `Run apsimx` block
  # Check presence of mandatory information in model model_options list

  apsimx_path <- model_options$apsimx_path
  apsimx_file <- model_options$apsimx_file
  warning_display <- model_options$warning_display
  exe <- apsimx_path
  if (.Platform$OS.type == "unix") {
	exe <- paste('mono', exe)
  }

  # Preliminary model checks ---------------------------------------------------
  if (is.null(model_options$apsimx_path) || is.null(model_options$apsimx_file)) {
    stop("apsimx_path and apsimx_file should be elements of the model_model_options
    list for the apsimx model")
  }

  # Test if the model executable file exists is executable ----------------------
  if (!file.exists(apsimx_path)){
    stop(paste("apsimx executable file doesn't exist !",apsimx_path))
  }
  if (!file.exists(apsimx_file)) {
	stop(paste("apsimx file doesn't exist !", apsimx_file))
  }
  val <- try(system(paste(exe,'/Version'),
                    intern = FALSE,
                    ignore.stdout = TRUE),
             silent = TRUE)

  if (val != 0) {
    stop(paste(apsimx_path,"is not executable or is not a apsimx executable !"))
  }

  start_time <- Sys.time()

  # Copy the .apsimx file to a temp file ----------------------------------------
  file_to_run <- tempfile('apsimOnR', fileext = '.apsimx')
  file.copy(apsimx_file, file_to_run)

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
  edit_file_stdout <- shell(cmd, translate = FALSE, intern = TRUE, mustWork = TRUE)
  #print(stdout)

  # Run apsimx ------------------------------------------------------------------
  cmd <- paste(exe, file_to_run)
  if (model_options$multi_process)
	cmd <- paste(cmd, '/MultiProcess')

  # run_file_stdout <- shell(cmd, translate = FALSE, intern = TRUE)
  # Portable version for system call
  run_file_stdout <- system(cmd,
                            ignore.stdout = TRUE,
                            ignore.stderr = TRUE)
  flag_allsim <- !run_file_stdout

  # Store results ---------------------------------------------------------------
  db_file_name <- gsub('.apsimx', '.db', file_to_run)

  predicted_data <- read_apsimx_output(db_file_name, model_options$predicted_table_name, model_options$variable_names)
  #observed_data <- read_apsimx_output(db_file_name, model_options$observed_table_name, model_options$variable_names)

  # Display simulation duration -------------------------------------------------
  if (model_options$time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  return(list(sim_list = predicted_data, flag_allsim = flag_allsim))

}

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
  options$variable_names <- c()

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

read_apsimx_output <- function(dbFileName, tableName, variables) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)

  # Fetch all data from each table and store it in a list.

  # vars <- ''
  # for (var in variables) {
  #   vars <- paste0(vars, 'Report.[', var, '], ')
  # }
  #
  # vars <- paste0(vars, '_Simulations.Name as SimulationName')

  vars <- paste(sprintf("Report.[%s], ", variables),
                "_Simulations.Name as SimulationName", collapse="")

  sql <- paste0('SELECT ', vars, ' FROM ', tableName, ', _Simulations WHERE _Simulations.ID = ', tableName, '.SimulationID')
  data <- DBI::dbGetQuery(con, sql)
  DBI::dbDisconnect(con)

  simulationNames <- data$SimulationName
  tables <- c()
  for (i in 1:length(simulationNames)) {
    sim <- simulationNames[i]
    tables[[i]] <- data[which(data$SimulationName == sim), ]
  }
  names(tables) <- simulationNames
  return(tables)
}

apsimx_display_warnings <- function(in_string) {
  # print(in_string)
  # print(length(in_string))
  if (nchar(in_string) ) warning(in_string, call. = FALSE)
}


#' @title Running .apsimx files from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses ApsimX directly through a system call, can
#' force ApsimX input parameters with values given in arguments.
#'
#' @param model_options List containing any information needed by the model.
#' In the case of apsimx: \code{apsimx_path} the path of apsimx executable file and
#' \code{apsimx_file} the path of the directory containing the apsimx input data
#' for each situation to simulate
#'
#' @param sit_names Vector of situations names for which results must be returned.
#'
#' @param param_values (optional) a named vector that contains the value(s) and name(s)
#' of the parameters to force for each situation to simulate. If not provided (or if is
#' NULL), the simulations will be performed using default values of the parameters
#' (e.g. as read in the model input files).
#'
#' @param sit_var_dates_mask (optional) List of situations:
#' may be either a character vector of situation names or a named list
#' containing information about variables and dates for which simulated values
#' should be returned. Typically a list containing the observations to which
#' simulations should be compared as provided by apsimxRFiles::read_obs_to_list
#'
#'
#' @return A list containing simulated values (`sim_list`: a vector of list (one
#' element per values of parameters) containing usms outputs data.frames) and an
#' error code (`error`) indicating if at least one simulation ended with an
#' error.
#'
#' @examples
#'
#' @export
#'
apsimx_wrapper <- function(model_options,
                           sit_names=NULL,
                           param_values = NULL,
                           sit_var_dates_mask = NULL,
                           ...) {

  # TODO : make a function dedicated to checking model_options
  # Because it may be model dependant, so it could be possible to pass anything
  # useful in the model running function...
  # Reuse next lines before `Run apsimx` block
  #
  #        find a way to force parameters values that may be different for the
  # different situations to simulate

  apsimx_path <- model_options$apsimx_path
  apsimx_file <- model_options$apsimx_file
  apsimx_file_dir <- dirname(apsimx_file)
  warning_display <- model_options$warning_display


  # Default output data list
  nb_paramValues=1
#  situation_names <- "all"   # no more used for the moment but could be later
  if (base::is.array(param_values)) {
    nb_paramValues=dim(param_values)[1]
#    situation_names <- dimnames(param_values)[[3]] # situations to simulate
  }
  res <- list()
  res$error <- FALSE
  res$sim_list <- list()


  # Preliminary model checks ---------------------------------------------------
  if (is.null(model_options$apsimx_path) || is.null(model_options$apsimx_file)) {
    stop("apsimx_path and apsimx_file should be elements of the model_model_options
    list for the apsimx model")
  }

  # Test if the model executable file exists is executable ----------------------
  if (!file.exists(apsimx_path)) {
    stop(paste("apsimx executable file doesn't exist !",apsimx_path))
  }
  if (!file.exists(apsimx_file)) {
    stop(paste("apsimx file doesn't exist !", apsimx_file))
  }
  cmd <- paste(apsimx_path, '/Version')
  # On unix systems, need to run via mono.
  if (.Platform$OS.type == 'unix') {
    cmd <- paste('mono', cmd)
  }
  val <- system(cmd, wait = TRUE, intern = TRUE)

  if ( !is.null(attr(val, "status"))) {
    stop(paste(apsimx_path,"is not executable or is not a apsimx executable !"))
  }

  if (base::is.array(param_values) &&
      !all(sapply(1:dim(param_values)[3],function(x) all(param_values[,,x]==param_values[,,1])))) {
    stop("ApsimX wrapper can not handle different parameters values for the different simulated situations for the moment.")
  }

  start_time <- Sys.time()

  # Copy the .apsimx file to a temp file ----------------------------------------
  temp_dir <- tempdir()
  file_to_run <- tempfile('apsimOnR', tmpdir = temp_dir, fileext = '.apsimx')
  db_file_name <- gsub('.apsimx', '.db', file_to_run)
  file.copy(apsimx_file, file_to_run)

  # copying met file
  met_files <- list.files(model_options$met_files_path,".met$", full.names = TRUE)
  file.copy(met_files, temp_dir)


  # copying XL file
  obs_files <- list.files(model_options$obs_files_path,".xlsx$", full.names = TRUE)
  file.copy(obs_files,temp_dir)

  # Delete .db file if it already exists (just in case)
  if (file.exists(db_file_name)) {
    file.delete(db_file_name)
  }

  for(ip in 1:nb_paramValues) {

    # If any parameter value to change
    if ( ! is.null(param_values) ) {
      # Generate config file containing parameter changes ---------------------------

      if (base::is.array(param_values)) {
        param_values_tmp=param_values[ip,,1]    # for the moment param values are supposed
                                                # to be the same for each situations (see check at the beginning)
        names(param_values_tmp)=colnames(param_values)

        config_file <- generate_config_file(param_values_tmp)
      } else {
        config_file <- generate_config_file(param_values)
      }
      #if (!out) {
      #  warning(paste("Error when changing parameters in", file_to_run))
      #  res$error=TRUE
      #  return(res)
      #}

    }

    # Run apsimx ------------------------------------------------------------------
    cmd <- paste(apsimx_path, file_to_run)
    if (!is.null(param_values)) {
      cmd <- paste(cmd, '/Edit', config_file)
    }

    # on unix, need to run via mono.
    if (endsWith(apsimx_path, '.dll')) {
      cmd <- paste('dotnet', cmd)
    } else if (.Platform$OS.type == 'unix') {
      cmd <- paste('mono', cmd)
    }
    if (model_options$multi_process)  cmd <- paste(cmd, '/MultiProcess')

    if (!is.null(sit_names)) {
      regex <- paste('(', paste(sit_names, collapse = ')|('), ')', sep = '')
      if (.Platform$OS.type == 'unix') {
        # on unix, need to escape the regex with quotes
        cmd <- paste(cmd, " '/SimulationNameRegexPattern:", regex, "'", sep = '')
      } else {
        cmd <- paste(cmd, ' /SimulationNameRegexPattern:', regex, sep = '')
      }
    }
    else if (!is.null(sit_var_dates_mask)) {
      # This generates a regular expression of simulation names using alternation
      # which will be passed to Models.exe to limit execution to the specified
      # simulation names.
      regex <- paste('(', paste(names(sit_var_dates_mask), collapse = ')|('), ')', sep = '')
      if (.Platform$OS.type == 'unix') {
        # on unix, need to escape the regex with quotes
        cmd <- paste(cmd, " '/SimulationNameRegexPattern:", regex, "'", sep = '')
      } else {
        cmd <- paste(cmd, ' /SimulationNameRegexPattern:', regex, sep = '')
      }
    }

    # Portable version for system call
    run_file_stdout <- system(cmd,wait = TRUE, intern = TRUE)

    # Getting the execution status
    res$error  <- !is.null(attr(run_file_stdout,"status"))

    # Preserve .apsimx file in case of error
    if (res$error) {
      print(run_file_stdout)

      apsimx_name <- basename(apsimx_file)
      backupFileName <- gsub('.apsimx', '.error.apsimx', apsimx_name)
      file.copy(file_to_run, file.path(apsimx_file_dir,backupFileName))

      backup_db_file <- gsub('.apsimx', '.error.db', apsimx_name)
      file.copy(db_file_name, file.path(apsimx_file_dir,backup_db_file))
    }

    # Store results ---------------------------------------------------------------
    results_tmp <- read_apsimx_output(db_file_name,
                                      model_options$predicted_table_name,
                                      model_options$variable_names)
    # We don't need to filter the results based on sit_names;
    # The only simulations which were run are those specified by sit_names.
    res$sim_list <- results_tmp

    # filtering on situations mask
    # We don't need to filter on sit_names, because only the relevant simulations
    # were run in the first place, so the outputs/predictions shouldn't contain
    # any simulations which aren't in sit_names.
    if (! is.null(sit_var_dates_mask) ) {
      situation_names_red <- names(sit_var_dates_mask)
      res$sim_list <- res$sim_list[situation_names_red]
      vars_list <- lapply(sit_var_dates_mask, colnames)
      dates_list <- lapply(sit_var_dates_mask, function(x) x$Date)

      for (i in 1:length(situation_names_red)) {
        sit_name <- situation_names_red[i]
        res$sim_list[[sit_name]] <- select(res$sim_list[[sit_name]],vars_list[[i]]) %>%
          filter(Date %in% dates_list[[i]])

      }

    }

  }

  # Display simulation duration -------------------------------------------------
  if (model_options$time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }


  res$db_file_name = db_file_name
  return(res)

}

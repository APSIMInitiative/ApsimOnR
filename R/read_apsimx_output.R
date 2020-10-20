#' @export
read_apsimx_output <- function(dbFileName, tableName, variables, sim_names=NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)

  # Fetch all data from each table and store it in a list.


  # TODO: add a request to check if tableName exists in db file


  vars <- paste(sprintf("%s.[%s], ",tableName, variables),
                collapse="")
  vars <- paste(vars,"_Simulations.Name as SimulationName")

  sql <- paste0('SELECT ', vars, ' FROM ', tableName, ', _Simulations WHERE _Simulations.ID = ', tableName, '.SimulationID')
  data <- DBI::dbGetQuery(con, sql)
  DBI::dbDisconnect(con)
  simulationNames <- unique(data$SimulationName)

  # Selecting simulations
  if (!is.null(sim_names)) {
    sim_idx <- simulationNames %in% sim_names
    simulationNames <- simulationNames[sim_idx]
  }

  # Creating empty list
  sim_nb <- length(simulationNames)
  tables <- vector("list", sim_nb)

  # Filling it with results
  for (i in 1:sim_nb) {
    sim <- simulationNames[i]
    tables[[i]] <- data[which(data$SimulationName == sim), ] %>% select(one_of(variables))
    if ("Clock.Today" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Clock.Today)) %>%
        select(-Clock.Today)
    } else if ("Date" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Date))
    } else if ("Predicted.Clock.Today" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Predicted.Clock.Today)) %>%
        select(-Predicted.Clock.Today)
    }
  }

  names(tables) <- simulationNames

  return(tables)
}

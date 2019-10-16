#' @export
read_apsimx_output <- function(dbFileName, tableName, variables) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbFileName)

  # Fetch all data from each table and store it in a list.

  # vars <- ''
  # for (var in variables) {
  #   vars <- paste0(vars, 'Report.[', var, '], ')
  # }
  #
  # vars <- paste0(vars, '_Simulations.Name as SimulationName')

  # TODO: add a request to check if tableName exists in db file


  vars <- paste(sprintf("%s.[%s], ",tableName, variables),
                collapse="")
  vars <- paste(vars,"_Simulations.Name as SimulationName")

  sql <- paste0('SELECT ', vars, ' FROM ', tableName, ', _Simulations WHERE _Simulations.ID = ', tableName, '.SimulationID')
  data <- DBI::dbGetQuery(con, sql)
  DBI::dbDisconnect(con)

  simulationNames <- unique(data$SimulationName)
  tables <- c()
  for (i in 1:length(simulationNames)) {
    sim <- simulationNames[i]
    tables[[i]] <- data[which(data$SimulationName == sim), ] %>% select(variables)
  }
  names(tables) <- simulationNames
  return(tables)
}

#' @export
read_apsimx_output_dplyr <- function(dbFileName, tableName, variables, sim_names=NULL) {

  tables_list <- c("_Simulations", "Observed")


  mydb <- src_sqlite(dbFileName)


  # checking if tableName exists in db file

  if ( ! tableName %in% src_tbls(mydb)) {
    stop(paste("Unkown table name in sqlite db file:",tableName))
  }


  simulationNames <- tbl(mydb, "_Simulations") %>%
    select(Name,ID) %>% collect()


  # Selecting simulations
  if (!is.null(sim_names)) {
    IDS <- simulationNames$ID[simulationNames$Name %in% sim_names ]
    simulationNames <- simulationNames %>% filter(ID %in% IDS)
  }

  data <- tbl(mydb, tableName) %>%
    select(c(variables, "SimulationID")) %>%
    filter(SimulationID %in% IDS) %>% collect()

  sim_nb <- nrow(simulationNames)
  tables <- vector("list", sim_nb)
  for (i in 1:sim_nb) {
    sim_id <- IDS[i]


    tables[[i]] <- data %>% filter(SimulationID %in% sim_id) %>% collect()

    if ("Clock.Today" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Clock.Today)) %>%
        select(-c("Clock.Today"))
    } else if ("Date" %in% names(tables[[i]])) {
      tables[[i]] <- mutate(tables[[i]],Date=as.Date(Date))
    }
  }
  names(tables) <- simulationNames$Name

  if (!is.null(sim_names)) {
    tables <- tables[sim_names]
    names(tables) <- sim_names
  }


  return(tables)
}

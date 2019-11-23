#' ---
#' title: Running an ApsimX model from R
#' output:
#'    html_document:
#'      toc: true
#'      toc_float: true
#' ---

library(ApsimOnR)
library(dplyr)
library(RSQLite)
library(ggplot2)


#' ### Introduction
#' Testing ApsimX runs without and with forcing parameters for one model and one output variable.
#' Dynamics plots are done for both simulations for `LAI` variable.
#'
#' ### Setting model executable and simulation file paths
#' All of the above information are used for setting model options in a list which is
#' passed to the model wrapper
#'
#' -  Setting the model executable file path
#'
apsimx_path="/usr/local/bin/Models"

#' - Setting apsimx file path
#' (stored in the ApsimOnR installed library)
files_path <- system.file(file.path("extdata","apsimx_files"),package = "ApsimOnR")
apsimx_file <- file.path(files_path, "template.apsimx")

#' - Setting situation name (i.e. ApsimX `model` name in the file)
situation_name <- "GattonRowSpacingRowSpace50cm"

#' - Wheather files files path
met_files_path <- files_path

#' - Observed files path
obs_files_path <- files_path

#' - Setting output variables list
variable_names=c("Wheat.Leaf.LAI")

#' - Setting output data kind
predicted_table_name <- "DailyReport"

#' ### Running the model without forcing parameters
model_options=apsimx_wrapper_options(apsimx_path,
                                    apsimx_file,
                                    variable_names = variable_names,
                                    predicted_table_name = predicted_table_name,
                                    met_files_path = met_files_path,
                                    obs_files_path = obs_files_path)

sim=apsimx_wrapper(model_options=model_options)


#' ### Running the model with **forcing parameters**
# Setting parameters values vector
param_values <- c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.4,
                  .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.4)

sim_par_forcing=apsimx_wrapper(param_values=param_values,model_options=model_options)


#' ### Results plots
#' - Getting dates
Dates <- as.Date(sim$sim_list[[situation_name]]$Date)
#' - Getting LAI
LAI <- sim$sim_list[[situation_name]]$Wheat.Leaf.LAI
LAI_forcing <- sim_par_forcing$sim_list[[situation_name]]$Wheat.Leaf.LAI
#' - LAI dynamics

ggplot(data.frame(Dates=Dates,LAI=LAI), aes(x=Dates, y=LAI)) +
  geom_line() +
  labs(title="LAI dynamics without forcing parameters")


ggplot(data.frame(Dates=Dates,LAI=LAI_forcing), aes(x=Dates, y=LAI)) +
  geom_line() +
  labs(title="LAI dynamics with forcing parameters",
  subtitle = "ExtinctionCoeff (0.4) and RUE (1.4)" )


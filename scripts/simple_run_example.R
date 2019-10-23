library(ApsimOnR)
library(dplyr)
library(RSQLite)


# TEST ON ONE MODEL and ONE VARIABLE

# Select the model, waiting for adding
simulation_name = ""

# Model path and simulation file
# full path
apsimx_path="/usr/local/bin/Models"
#
# Getting apsimx file fro the package
files_path <- system.file(file.path("extdata","apsimx_files"),package = "ApsimOnR")
apsimx_file <- file.path(files_path, "template.apsimx")

# met files path
met_files_path <- file.path(files_path)

# define the variables list
variable_names=c("Wheat.Leaf.LAI","Date")

predicted_table_name <- "DailyReport"

# Runnning the model without forcing parameters
model_options=apsimx_wrapper_options(apsimx_path,
                                    apsimx_file,
                                    variable_names = variable_names,
                                    predicted_table_name = predicted_table_name,
                                    met_files_path = met_files_path)

sim=apsimx_wrapper(model_options=model_options)


# Run the model with forcing parameters
# Setting parameters values vector
param_values <- c()
names(param_values) <- c()

sim_par_forcing=apsimx_wrapper(param_values=param_values,model_options=model_options)

# # Plot the results
# dev.new()
# par(mfrow = c(1,2))
# Ymax=max(max(obs_list[[simulation_name]][,var_name], na.rm=TRUE),
#          max(sim_before_optim$sim_list[[simulation_name]][,var_name], na.rm=TRUE))
# plot(sim_before_optim$sim_list[[simulation_name]][,c("Date",var_name)],type="l",
#      main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
# points(obs_list[[simulation_name]],col="green")
# plot(sim_after_optim$sim_list[[simulation_name]][,c("Date",var_name)],type="l",
#      main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
# points(obs_list[[simulation_name]],col="green")

plot(as.Date(sim$sim_list$GattonRowSpacingRowSpace50cm$Date),
     sim$sim_list$GattonRowSpacingRowSpace50cm$Wheat.Leaf.LAI,
     type = "l")

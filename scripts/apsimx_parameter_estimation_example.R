library(ApsimOnR)
library(CroptimizR)
library(dplyr)
library(nloptr)
library(DiceDesign)


# TEST ON ONE MODEL and ONE VARIABLE

start_time <- Sys.time()

# Select the model
simulation_name <- "GattonRowSpacingRowSpace25cm"

# define the variables list
variable_names=c("Wheat.Leaf.LAI")

# Getting apsimx file fro the package
apsimx_path="/usr/local/bin/Models"
files_path <- system.file(file.path("extdata","apsimx_files"),package = "ApsimOnR")
apsimx_file <- file.path(files_path, "template.apsimx")


# met files path
met_files_path <- files_path

# obs path
obs_files_path <- files_path


predicted_table_name <- "DailyReport"
observed_table_name <- "Observed"

# Runnning the model without forcing parameters
model_options=apsimx_wrapper_options(apsimx_path = apsimx_path,
                                     apsimx_file = apsimx_file,
                                     variable_names = variable_names,
                                     predicted_table_name = predicted_table_name,
                                     met_files_path = met_files_path,
                                     observed_table_name = observed_table_name,
                                     obs_files_path = obs_files_path)


sim_before_optim=apsimx_wrapper(model_options=model_options)


# observations
# obs_idx <- names(sim_before_optim$obs_list) %in% names(sim_before_optim$sim_list)
# obs_list <- sim_before_optim$obs_list[obs_idx]
obs_list <- read_apsimx_output(sim_before_optim$db_file_name,
                               model_options$observed_table_name,
                               model_options$variable_names,
                               names(sim_before_optim$sim_list))

obs_list=obs_list[simulation_name]
names(obs_list) <- simulation_name

# Set prior information on the parameters to estimate
#
param_info=list(lb=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.4,
                            .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.4),
                       ub=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.6,
                            .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.6))

# Set options for the parameter estimation method
optim_options=list()
optim_options$nb_rep <- 3 # How many times we run the minimization with different parameters
optim_options$xtol_rel <- 1e-05 # Tolerance criterion between two iterations
optim_options$maxeval <- 2 # Maximum number of iterations executed by the function
#optim_options$path_results <- "/home/plecharpent/tmp/tests_SticsOptimizR/estim_example" # path where to store results graphs
optim_options$path_results <- "/home/drew/code/ApsimOnR/output"

# Run the optimization
optim_output=estim_param(obs_list=obs_list,
                         model_function=apsimx_wrapper,
                         model_options=model_options,
                         optim_options=optim_options,
                         param_info=param_info)

# Run the model after optimization
sim_after_optim=apsimx_wrapper(param_values= optim_output$final_values,
                               model_options=model_options)

# Plot the results

var_name <- "Wheat.Leaf.LAI"
simulation_name <- "GattonRowSpacingRowSpace25cm"
duration <- Sys.time() - start_time
print(sprintf('duration: %s', duration))
dev.new()
par(mfrow = c(1,2))
Ymax=max(max(obs_list[[simulation_name]][,var_name], na.rm=TRUE),
         max(sim_before_optim$sim_list[[simulation_name]][,var_name], na.rm=TRUE))
plot(sim_before_optim$sim_list[[simulation_name]][,c("Date",var_name)],type="l",
     main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[simulation_name]]$Date,obs_list[[simulation_name]][[var_name]],col="red")
plot(sim_after_optim$sim_list[[simulation_name]][,c("Date",var_name)],type="l",
     main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[simulation_name]]$Date,obs_list[[simulation_name]][[var_name]],col="red")


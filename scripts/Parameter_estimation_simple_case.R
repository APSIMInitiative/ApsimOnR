# Parameter estimation with the ApsimX crop Model: a simple case

library(ApsimOnR)
library(dplyr)
library(SticsOptimizR)
library(nloptr)
library(DiceDesign)



if(!require("ApsimOnR")){
  devtools::install_github("ApsimOnR")
  library("ApsiOnR")
}
if(!require("SticsOptimizR")){
  devtools::install_github("SticsRPacks/SticsOptimizR")
  library("SticsOptimizR")
}
if(!require("dplyr")){
  install.packages("dplyr",repos="http://cran.irsn.fr")
  library("dplyr")
}
if(!require("nloptr")){
  install.packages("nloptr",repos="http://cran.irsn.fr")
  library("nloptr")
}
if(!require("DiceDesign")){
  install.packages("DiceDesign",repos="http://cran.irsn.fr")
  library("DiceDesign")
}
if(!require("doParallel")){
  install.packages("doParallel",repos="http://cran.irsn.fr")
  library("doParallel")
}

# starting time
start_time <- Sys.time()

# sites name
sit_name="GattonRowSpacingRowSpace25cm"
# variables names
var_name = c("Wheat.Leaf.LAI","Clock.Today")



# Set the model options (see '? stics_wrapper_options' for details)
#
# For a linux OS : apsimx_path="/usr/local/bin/Models"
# For a Windows OS: apsimx_path=
files_path <- system.file(file.path("extdata","apsimx_files"),package = "ApsimOnR")
apsimx_file <- file.path(files_path, "template.apsimx")
# ApsimX exe path
apsimx_path <- system("which Models", intern = TRUE)

# Setting met files path
met_files_path <- files_path

# Setting observed data files path
obs_files_path <- files_path

# Setting sqlite db tables names
predicted_table_name <- "DailyReport"
observed_table_name <- "Observed"

model_options=apsimx_wrapper_options(apsimx_path = apsimx_path,
                                     apsimx_file =  apsimx_file,
                                     variable_names = var_name,
                                     predicted_table_name = predicted_table_name,
                                     met_files_path = met_files_path,
                                     observed_table_name = observed_table_name,
                                     obs_files_path = obs_files_path)


# Run the model (on all situations found in the apsimx_file)
sim_before_optim=apsimx_wrapper(model_options=model_options)


# Retrieving observed data
# At the moment, the observed data are actually read from the db file after the first # simulation ran before optimization. But they may be loaded using the original xlsx data file (from the files_path)
obs_list <- read_apsimx_output(sim_before_optim$db_file_name,
                               model_options$observed_table_name,
                               model_options$variable_names,
                               names(sim_before_optim$sim_list))

sit_name="GattonRowSpacingRowSpace25cm"

obs_list=obs_list[sit_name]
names(obs_list) <- sit_name



# 2 parameters here: ExtinctionCoeff and RUE, of prior distributions U([0.4,0.6]) and U([1.4,1.6])
# This is also used to set the list of parameters to estimate
prior_information <-
  list(lb=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.4,
            .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.4),
       ub=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.6,
            .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.6))


# Setting optimization options
optim_options=list()
optim_options$nb_rep <- 2 # Number of repetitions of the minimization
# (each time starting with different initial
# values for the estimated parameters)
optim_options$maxeval <- 20 # Maximum number of evaluations of the
# minimized criteria
optim_options$xtol_rel <- 1e-05 # Tolerance criterion between two iterations
# (threshold for the relative difference of
# parameter values between the 2 previous
# iterations)
# Output storage directory
# TODO: adapt it to user own directory
optim_options$path_results <- "/home/plecharpent/tmp/tests_SticsOptimizR/ApsimX_param_est_simple"
# results graphs
#optim_options$ranseed <- 1234 # random seed


# Running optimization process
param_est_values=main_optim(obs_list=obs_list,
                            crit_function=concentrated_wss,
                            model_function=apsimx_wrapper,
                            model_options=model_options,
                            optim_options=optim_options,
                            prior_information=prior_information)

print(paste("Results of the optimization were saved in",
            optim_options$path_results," folder."))

# displaying optimization results
param_est_values

# Running simulation after optimization
sim_after_optim=apsimx_wrapper(param_values=param_est_values,
                               model_options=model_options)


# calculating duration
duration <- Sys.time() - start_time
print(sprintf('duration: %s', duration))


var_name = c("Wheat.Leaf.LAI")

# Plotting results: simulated vs observed data
# before and after optimization
par(mfrow = c(1,2))

# Simulated and observed LAI before optimization
Ymax=max(max(obs_list[[sit_name]][,var_name], na.rm=TRUE),
         max(sim_before_optim$sim_list[[sit_name]][,var_name], na.rm=TRUE))
plot(sim_before_optim$sim_list[[sit_name]][,c("Date",var_name)],type="l",
     main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[sit_name]]$Date,obs_list[[sit_name]][[var_name]],col="red")
plot(sim_after_optim$sim_list[[sit_name]][,c("Date",var_name)],type="l",
     main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[sit_name]]$Date,obs_list[[sit_name]][[var_name]],col="red")

# exporting plot to a png file
dev.print(device = png,
          file = file.path(optim_options$path_results,"sim_obs_plots.png"),
          width = 15,
          height = 10,
          units = "cm",
          res=1000)




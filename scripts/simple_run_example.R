 library(ApsimOnR)
 library(dplyr)


 # TEST ON ONE MODEL and ONE VARIABLE

 # Select the model
 simulation_name = ""

 # Model path and simulation file
 # full path
 apsimx_path=""
 #
 # Getting apsimx file fro the package
apsimx_file <- system.file(file.path("extdata","apsimx_files", "template.apsimx"),package = "ApsimOnR")

 # define the variables list
 variable_names=c()

 # Runnning the model without forcing parameters
 model_options=apsim_wrapper_options(apsimx_path,
                                     apsimx_file,
                                     variable_names = variable_names)

 sim=apsim_wrapper(model_options=model_options)


 # Run the model with forcing parameters
 # Setting parameters values vector
 param_values <- c()
 names(param_values) <- c()

 sim_par_forcing=apsim_wrapper(param_values=param_values,model_options=model_options)

 # Plot the results
 dev.new()
 par(mfrow = c(1,2))
 Ymax=max(max(obs_list[[simulation_name]][,var_name], na.rm=TRUE),
          max(sim_before_optim$sim_list[[simulation_name]][,var_name], na.rm=TRUE))
 plot(sim_before_optim$sim_list[[simulation_name]][,c("Date",var_name)],type="l",
      main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
 points(obs_list[[simulation_name]],col="green")
 plot(sim_after_optim$sim_list[[simulation_name]][,c("Date",var_name)],type="l",
      main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
 points(obs_list[[simulation_name]],col="green")


# Title: "basic_setup.R"
# Description: A collection of functions that help identify and install missing packages

Setup <- function(base.config ="config/config.yml", install.missing.packages=TRUE,
                  log.cleanup=TRUE, run.all.tests=TRUE, time.zone="UTC"){
  # Set time zone: 
  Sys.setenv(TZ=time.zone)
  
  # Check to ensure 'config' package is installed: 
  if (!'config' %in% installed.packages()[,'Package']){
    print('Installing Required package: "config"')
    install.packages('config')
  } 
  
  # Get configuration objects from config.yml file: 
  config <- config::get()
  
  # Check Packages
  CheckPackages(config)
  
  # # Load Lookups: 
  # config <- LoadCSVLookups(config)
  
  # Source Scripts/Functions: 
  LoadAllScripts(script.dirs=config$loadableDirs)
  

  return(config)
}

# Function: CheckPackages()
CheckPackages <- function(config){
  # Check to ensure all packages are loaded: 
  missing.visualizationPackages <- config$visualizationPackages[!(config$visualizationPackages %in% installed.packages()[,'Package'])]
  missing.analysisPackages <- config$analysisPackages[!(config$analysisPackages %in% installed.packages()[,'Package'])]
  missing.dataPackages <- config$dataPackages[!(config$dataPackages %in% installed.packages()[,'Package'])]
  
  if (length(missing.visualizationPackages)) install.packages(missing.visualizationPackages)
  if (length(missing.analysisPackages)) install.packages(missing.analysisPackages)
  if (length(missing.dataPackages)) install.packages(missing.dataPackages)
  
  print('All packages successfully installed')
}

# # Function: LoadCSVLookups()
# LoadCSVLookups <- function(config){
#   require(data.table)
#   config$lookup <- list()
#   for (lookup in names(config$lookups)){
#     if (grepl('.csv$',config$lookups[[lookup]])){
#       config$lookup[[lookup]] <- data.table::fread(config$lookups[[lookup]])
#     }
#   }
# # Return config: 
#   print('All CSV Lookups successfully loaded')
#   return(config)
# }

# Function: LoadAllScripts()
LoadAllScripts <- function(script.dirs, pattern="\\.R$"){
  for (dir in script.dirs){
    sourceable.files <- grep(pattern, list.files(dir, recursive=TRUE, full.names=TRUE), value=TRUE)
    sapply(sourceable.files, source)
  }
  print('All Scripts successfully sourced')
}

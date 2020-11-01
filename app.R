# app.R

# Title: SYS-5370 Final Project ('The Essential Ones')
# Description: Application to analyze the hypothesis that a difference in the 
#              primary political party by County compared to Governor's political
#              party has an effect on the death and infection rates of COVID-19
# Author: Dan Klemfuss (Team: The Essential Ones)

# Load setup script: 
source('functions/basic_setup.R')

##
## Run setup and create global config object: 
##
global.config <<- Setup()

# Define libraries used (note: packages should be loaded in 'Setup' function)
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library('shinyBS')
library('plotly')
library('ggplot2')
library('htmlwidgets')
library('data.table')
library('dplyr')
library('tidyr')
library('stringr')
library('lubridate')

##
## Define the UI
##
UI <- dashboardPage(
  # Create Dashboard Header: 
  dashboardHeader(title = "COVID Analysis"),
  # Create Dashboard Sidebar Menu: 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Political Affiliation Analysis", tabName = "paa", icon = icon('map'))
    )
  ),
  # Create Dashboard Body (main panel when menu item selected)
  dashboardBody(
    tabItem(tabName = "paa",
            CovidAnalysisUI('paa'))
  )
)

##
## Define the Server Code: 
##
Server <- function(input, output, session){
  # Define source for logging: 
  name <- 'Example app.R'
  
  # Define namespace:
  ns <- session$ns
  
  # Call Modules:
  callModule(module=CovidAnalysisModule, id="paa", config=global.config)
  
}

# Return a Shiny app object
shinyApp(ui=UI, server=Server)

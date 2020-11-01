# Load COVID Data Module

library(data.table)
library(lubridate)

CorrelateCovidData <- function(global.config, normalization=100000){
  # Attempt to load Weekly Covid19 Data from /Data Directory: 

  data1 <- LoadCountyElectionData(global.config$lookups$Counties)
  data2 <- LoadGovernorData(global.config$lookups$Governors)
  data3 <- LoadCovid19Data(directory='data/',pattern='^csse_covid_19')
  
  if (is.null(data1) | is.null(data2) | is.null(data3)){
    print('Warning: All data not available for correlation')
  } else {
    
    # Merge the Governor and Election Data: 
    merge1 <- merge(data1, data2, by.x='ST', by.y='State_ID')
    
    # Determine Correlate the COVID Data by FIPS
    merge2 <- merge( data3,merge1, by.x='FIPS', by.y='FIPS')
    
    # Determine if there's a difference in political party of governor and majority of voters:
    output <- merge2 %>% mutate(
      POLITICAL_DIFF = case_when(toupper(Party)==MAJORITY ~ 'SAME',
                                 TRUE ~ 'DIFFERENT'),
      DEATH_RATE_100k = DEATHS/`Total Population`*normalization,
      INFECTION_RATE_100K = CONFIRMED/`Total Population`*normalization
    )
    
    return(output)
  }
  
  
  
}

LoadCovid19Data <- function(directory, pattern='^csse_covid_19'){
  require(stringr)
  require(dplyr)
  # Note: Data Typically downloaded for each week from the following source: 
  # https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
  # Extract files from directory that contain the pattern: 
  subset.files <- list.files(directory)[grepl(pattern,list.files(directory))]
  if (length(subset.files)>0){
    temp <- list()
    # Iterate through the data files based on date range: 
    for (file in subset.files){
      filePath <- paste0(directory,file)
      temp[[file]] <- fread(filePath)
    }
    
    # Combine the data into one data frame: 
    raw.data <- rbindlist(temp,use.names=TRUE,fill=TRUE)
    
    # Clean and standardize the header names (this allows user to copy/paste from source without modification:
    check <- c('FIPS','Admin2','Province_State','Country_Region','Last_Update','Lat','Long_','Confirmed','Deaths','Recovered','Active') %in% colnames(raw.data)
    if (all(check)){
      output <- raw.data %>% filter(Country_Region=='US') %>% mutate(
        FIPS = str_pad(as.character(FIPS),5,pad="0"),
        COUNTY = Admin2, 
        REPORT_DATE = lubridate::date(Last_Update),
        LAT = Lat, 
        LNG = Long_, 
        CONFIRMED = Confirmed, 
        DEATHS = Deaths, 
        RECOVERED = Recovered,
        ACTIVE = Active
      )
      
      output <- output %>% select(c('FIPS','COUNTY','REPORT_DATE','LAT','LNG','CONFIRMED','DEATHS','RECOVERED','ACTIVE'))
      print(paste0('Success: Loaded files with pattern: "',pattern, '" in Directory: ', directory))
      return(output)
      
    } else {
      print(paste0('Invalid File ingested, missing required header(s): ',check))
    }

  } else {
    print(paste0('Warning: No matching files with pattern: "',pattern, '" in Directory: ', directory))
  }
  
  
}

LoadGovernorData <- function(fileID){
  # Note: Data modified from the following source: 
  # https://simple.wikipedia.org/wiki/List_of_United_States_governors
  
  # Note: Since this file is created manually, only check headers: 
  raw.data <- fread(fileID)
  
  check <- c('State_name','State_ID','Governor','Party','Gender') %in% colnames(raw.data)
  if (all(check)){
    output <- raw.data
    return(output)
  } else {
    print(paste0('Invalid File ingested, missing required header(s): ',check))
  }
  
}

LoadCountyElectionData <- function(fileID){
  require(stringr)
  require(dplyr)
  # Note: Data modified from the following source: 
  #https://public.opendatasoft.com/explore/dataset/usa-2016-presidential-election-by-county/export/?disjunctive.state&basemap=mapbox.light&location=5,40.31304,-82.15576
  
  # Read in raw data: 
  raw.data <- fread(fileID, sep=';')
  
  # Check for matching required headers (note: since a static file should not be changed): 
  check <- c('State','ST','Fips','County','Votes','Republicans 2016','Democrats 2016','Total Population', 'Geo Shape') %in% colnames(raw.data)
  
  if (all(check)){
    output <- raw.data %>% mutate(
      FIPS = str_pad(as.character(Fips),5,pad="0"),
      MAJORITY = case_when(`Republicans 2016` > `Democrats 2016` ~ 'REPUBLICAN',
                           `Democrats 2016` > `Republicans 2016` ~ 'DEMOCRAT',
                           TRUE ~ NA_character_)
    )
    return(output)
  } else {
    print(paste0('Invalid File ingested, missing required header(s): ',check))
  }
  
}

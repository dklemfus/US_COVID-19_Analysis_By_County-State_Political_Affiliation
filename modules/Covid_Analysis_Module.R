# SCOVID Analysis Module

################################################################################
#                               MODULE UI                                      #
################################################################################

CovidAnalysisUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(title="Application Info:", solidHeader=TRUE, collapsible=TRUE, width=12,
          column(6,
                 tags$span(tags$b("SYS-5370 FINAL PROJECT"),
                           tags$p(tags$b("Team:"),"The Essential Ones"),
                           tags$p(tags$b("Term:"),"Summer 2020"),
                           tags$p(tags$b("Florida Institute of Technology")))),
          column(6,
                 tags$span(tags$b("Description:"),
                           tags$p(paste0("This application analyzes the Death and Infection ",
                                  "rates from COVID-19, using data stored in the local 'data/' directory. ",
                                  "It uses the most recent election results to determine a ",
                                  "Political Party for each U.S. county, a lookup of the current ",
                                  "State Governor to determine the state Political Party, and ",
                                  "Correlates this data to the COVID Death/Infection rates by county. ",
                                  "The user can perform analysis of the most recent aggregation date, or ",
                                  "Analyze a weekly trend. The user can also filter by state.")),
                           tags$p(paste0("A Shapiro-Wilk normality test determines if the data is normal. ",
                                         "If so, a T-test is performed to determine whether the Null ", 
                                         "Hypothesis is rejected (mu_same=mu_diff). If the data is non-normal, a Wilcoxan ",
                                         "Rank Sum Test (with Continuity Correction) is used to determine ",
                                         "if the data is significantly different for the death/infection rate means."))))
          ),
      box(title="Load/Filter Data:", solidHeader=TRUE, collapsible=TRUE, width=12,
          fluidRow(
            column(12,
                   bsButton(ns('load.data'),'Load Data',style='primary',type='action')
            ),
            column(8,
                   plotOutput(ns('map'))),
            column(4, 
                   pickerInput(ns('state.filter'),label='Filter by State(s):', choices="Load Data", selected="Load Data", multiple=FALSE,
                                  options=list(`actions-box`=TRUE,`live-search`=TRUE)),
                   bsButton(ns('perform.analysis'),'Run Analysis',sytle='primary', type='action')),
            
          )),
      box(title="Hypothesis Testing (Same/Diff Political Parties)", solidHeader=TRUE, collapsible=TRUE, width=12,
          fluidRow(
            column(12,
                   uiOutput(ns('analysis.out1')))
          )),
      box(title="Analysis: Weekly Aggregation", solidHeader=TRUE, collapsible=TRUE, width=12,
          fluidRow(
            column(12,
                   uiOutput(ns('analysis.out2')))
          )),
      box(title="Exploratory Analysis", solidHeader=TRUE, collapsible=TRUE, width=12,
          fluidRow(
            column(12,
                   uiOutput(ns('analysis.out3')))
          ))
    )
  )
}

################################################################################
#                             MODULE  SERVER                                   #
################################################################################

CovidAnalysisModule <- function(input, output, session, config){
  ns <- session$ns
  
  name <- "CovidAnalysisModule"
  
  # Create Reactive Values: 
  values <- reactiveValues(covid_data = NULL)
  
  # Initialize Values
  # Create the map
  output$map <- renderPlot({
    usmap::plot_usmap(regions='counties') + 
      theme(panel.background = element_rect(color='black', fill='lightblue'))
  })
  
  ##############################################################################
  #                         REACTIVE EVENTS                                    #
  ##############################################################################
observeEvent(input$load.data, { 
  values$covid_data <- CorrelateCovidData(global.config)
  test.covid.data <<- values$covid_data
  if (nrow(values$covid_data) > 0 ){
    sendSweetAlert(session,title='Successfully Loaded/Merged Data', 
                   text = paste0('Success: Loaded files'),
                   type = 'success')
    
    updatePickerInput(session, 'state.filter',label='Filter by State(s):',choices=c('All',sort(unique(values$covid_data$ST))), selected=c('All'))
  }
  
})
  # Update Charts to show hypothesis testing: 
  observeEvent(input$perform.analysis,{
    if (!input$state.filter=="Load Data"){
      if (input$state.filter=="All"){
        filtered.data <- values$covid_data
      } else {
        filtered.data <- values$covid_data[grepl(input$state.filter, values$covid_data$ST)]
      }
      test.filtered.data <<- filtered.data
      
      PerformPoliticalAffiliationAnalysis(input,output,session,filtered.data)
      filtered.data$fips <- filtered.data$FIPS
      
      output$map <- renderPlot({
        usmap::plot_usmap(data=filtered.data, labels=FALSE, value='POLITICAL_DIFF') + 
          scale_discrete_manual('POLITICAL_DIFF',values=c(21,22))
      })
    }
  })
  
  # Update Leaflet with county data:

  
  ##############################################################################
  #                         DOWNLOAD HANDLERS                                  #
  ##############################################################################
  
  
  ##############################################################################
  #                         HELPER FUNCTIONS                                   #
  ##############################################################################
  PerformPoliticalAffiliationAnalysis <- function(input,output,session,filtered.data,ALPHA=0.05){
    
    # Determine the average death rates (mu,d) for counties with same/different political affiliations compared to governor:
    most.recent.date <- max(filtered.data$REPORT_DATE)
    most.recent.data <- filter(filtered.data, REPORT_DATE==most.recent.date)
    
    # Plot Infection and Death Rate Distributions: 
    death.density <- ggplot(data=most.recent.data, aes(x=DEATH_RATE_100k, fill=POLITICAL_DIFF )) + geom_density(alpha=0.5) + 
      ggtitle('Density Plot: Death Rate Per 100k by County')
    infection.density <- ggplot(data=most.recent.data, aes(x=INFECTION_RATE_100K, fill=POLITICAL_DIFF )) + geom_density(alpha=0.5) + 
      ggtitle('Density Plot: Infection Rate Per 100k by County')

    # Total Sample Size for states that had same/differing political affiliations: 
    analysis.agg <- most.recent.data %>% 
      group_by(POLITICAL_DIFF) %>% 
      summarize(`Population` = sum(`Total Population`),
                `Death Rate Per 100k (Mean)` = mean(DEATH_RATE_100k),
                `Death Rate Per 100k (SD)` = sd(DEATH_RATE_100k),
                
                `Infection Rate Per 100k (Mean)` = mean(INFECTION_RATE_100K),
                `Infection Rate Per 100k (SD)` = sd(INFECTION_RATE_100K))
    
    # Create Box plots from data: 
    death.boxplot <- ggpubr::ggboxplot(filtered.data, x='POLITICAL_DIFF', y= 'DEATH_RATE_100k',
                                       color='POLITICAL_DIFF', palette = c("#00AFBB", "#E7B800"),
                                       ylab = 'Deaths', xlab = "Political Difference")
    
    # Check Independent T-test assumptions: Using a Shapiro-Wilk normality test: Null Hypothesis: Data is normally distributed, Alternate Hypothesis: Data is not normally distributed
    same.data.death <- most.recent.data$DEATH_RATE_100k[grepl('SAME',most.recent.data$POLITICAL_DIFF)]
    diff.data.death <- most.recent.data$DEATH_RATE_100k[grepl('DIFFERENT',most.recent.data$POLITICAL_DIFF)]
    same.data.infection <- most.recent.data$DEATH_RATE_100k[grepl('SAME',most.recent.data$POLITICAL_DIFF)]
    diff.data.infection <- most.recent.data$DEATH_RATE_100k[grepl('DIFFERENT',most.recent.data$POLITICAL_DIFF)]

    # Shapiro-Wilk normality test for Political Diff = SAME:
    if (length(same.data.death) > 3){
      # Note: Shapiro test only works with 3-5000 data points. If less than 3, assume non-normality
      test1 <- shapiro.test(same.data.death)
      test2 <- shapiro.test(same.data.infection)
      is.normal.same <- ifelse(test1$p.value > ALPHA & test2$p.value > ALPHA, TRUE, FALSE)
      
      test1.str <- paste0('METHOD: ',test1$method,' STATISTIC: ',test1$statistic,' P-VALUE: ',test1$p.value)
      test2.str <- paste0('METHOD: ',test2$method,' STATISTIC: ',test2$statistic,' P-VALUE: ',test2$p.value)
    }  else {
      test1.str <- 'Unable to Determine Death Rate for Same Political Parties, no available data'
      test2.str <- 'Unable to Determine Infection Rate for Same Political Parties, no available data'
      is.normal.same <- FALSE
    }

    if (length(diff.data.death) > 3){
      # Shapiro-Wilk normality test for Political Diff = DIFFERENT
      test3 <- shapiro.test(diff.data.death)
      test4 <- shapiro.test(diff.data.infection)
      is.normal.diff <- ifelse(test3$p.value > ALPHA & test4$p.value > ALPHA, TRUE, FALSE)
      
      test3.str <- paste0('METHOD: ',test3$method,' STATISTIC: ',test3$statistic,' P-VALUE: ',test3$p.value)
      test4.str <- paste0('METHOD: ',test4$method,' STATISTIC: ',test4$statistic,' P-VALUE: ',test4$p.value)
    } else {
      test3.str <- 'Unable to Determine Death Rate for Differing Political Parties, no available data'
      test4.str <- 'Unable to Determine Infection Rate for Differing Political Parties, no available data'
      is.normal.diff <- FALSE
    }
    
    if (is.normal.same & is.normal.diff){
      #Perform a T-test if data is distributed normally, assuming equal variation:
      res.death <- t.test(same.data.death, diff.data.death, var.equal=TRUE)
      res.infection <- t.test(same.data.infection, diff.data.infection)
      death.test.str <- paste0('METHOD: ',res.death$method, ' STATISTIC: ', res.death$statistic, ' P-VALUE: ',res.death$p.value, ' ALTERNATIVE: ',res.death$alternative)
      infection.test.str <- paste0('METHOD: ',res.infection$method, ' STATISTIC: ', res.infection$statistic, ' P-VALUE: ',res.infection$p.value, ' ALTERNATIVE: ',res.infection$alternative)
      death.conclusion <- ifelse(res.death$p.value < ALPHA, 'USING T-TEST: Since P-value < alpha, results are significantly different for Death Rate', 
                           'USING T-TEST: Since P-Value > alpha, results are not significantly different for Death Rate')
      infection.conclusion <- ifelse(res.infection$p.value < ALPHA, 'USING T-TEST: Since P-value < alpha, results are significantly different for Infection Rate', 
                                 'USING T-TEST: Since P-Value > alpha, results are not significantly different for Infection Rate')
    } else if (length(same.data.death) > 0 & length(diff.data.death) > 0 ) {
      # Perform Wilcoxon rank sum test if data is not normally distributed: 
      res.death <- wilcox.test(same.data.death, diff.data.death)
      res.infection <- wilcox.test(same.data.infection, diff.data.infection)
      death.test.str <- paste0('METHOD: ',res.death$method, ' STATISTIC: ', res.death$statistic, ' P-VALUE: ',res.death$p.value, ' ALTERNATIVE: ',res.death$alternative)
      infection.test.str <- paste0('METHOD: ',res.infection$method, ' STATISTIC: ', res.infection$statistic, ' P-VALUE: ',res.infection$p.value, ' ALTERNATIVE: ',res.infection$alternative)
      death.conclusion <- ifelse(res.death$p.value < ALPHA, 'USING WILCOXAN: Since P-Value < alpha, results are significantly different for Death Rate', 
                           'USING WILCOXAN: Since P-Value > alpha, results are not significantly different for Death Rate')
      infection.conclusion <- ifelse(res.infection$p.value < ALPHA, 'USING WILCOXAN: Since P-Value < alpha, results are significantly different for Infection Rate', 
                                 'USING WILCOXAN: Since P-Value > alpha, results are not significantly different for Infection Rate')
    } else {
      death.test.str <- paste0('Unable to perform Death Rate Comparison, all available data is for same political party')
      infection.test.str <- paste0('Unable to perform Infection Rate Comparison, all available data is for same political party')
      death.conclusion <- "Unable to perform Death Rate comparison, all available data is for same political party"
      infection.conclusion <- "Unable to perform Infection Rate comparison, all available data is for same political party"
    }
    
    output$analysis.out1 <- renderUI({
      tagList(
        tags$div(tags$b(paste0("Most Recent Date Analyzed: ", most.recent.date) )),
        tags$div(tags$b("Density Plot: Death Rate per 100k")),
        plotly::ggplotly(death.density),
        tags$div(tags$b("Density Plot: Infection Rate per 100k")),
        plotly::ggplotly(infection.density),
        DT::renderDataTable(analysis.agg), 
        tags$div(tags$b("Box Plot (Deaths)")),
        plotly::ggplotly(death.boxplot),
        tags$div(tags$b("Check for Normal Distribution (Shapiro-Wilk Normality):")),
        tags$div(tags$b("Test 1: Death Rate for Same Political Party: ")),
        renderPrint(test1.str),
        tags$div(tags$b('Test 2: Death Rate for Different Political Party ')),
        renderPrint(test3.str), #Note: This is supposed to be 3, despite what's shown in GUI
        tags$div(tags$b('Test 3: Infection Rate for Same Political Party:')),
        renderPrint(test2.str),  #Note: This is supposed to be 2, despite what's shown in GUI
        tags$div(tags$b('Test 4: Infection Rate for Different Political Party: ')),
        renderPrint(test4.str),  #Note: This is supposed to be 4, despite what's shown in GUI
        tags$div(tags$b('TEST FOR NORMAL DISTRIBUTION (Compare against alpha = 0.5):')),
        renderPrint(paste0('SAME DATA: ',is.normal.same, ', DIFF DATA: ',is.normal.diff)),
        tags$div(tags$b("Perform T-Test (normal dist.) or Wilcoxan (non-normal dist.) for Deaths:")),
        renderPrint(death.test.str),
        tags$div(tags$b("Perform T-Test (normal dist.) or Wilcoxan (non-normal dist.) for Infections:")),
        renderPrint(infection.test.str),
        tags$div(tags$b("DEATH RATE CONCLUSIONS:")),
        renderPrint(paste0(death.conclusion)),
        tags$div(tags$b("INFECTION RATE CONCLUSIONS:" )),
        renderPrint(paste0(infection.conclusion))
      )
    })
    
    # Create Outputs for Weekly Aggregation:
    rolling.week.agg <- filtered.data %>% group_by(REPORT_DATE,POLITICAL_DIFF) %>%
      summarize(`Affected Counties` = n(),
                `Death Rate Per 100k` = mean(DEATH_RATE_100k),
                `Infection Rate Per 100k` = mean(INFECTION_RATE_100K))
    
    death.plot <- ggplot(data=rolling.week.agg, aes(x=REPORT_DATE, y=`Death Rate Per 100k`, fill=POLITICAL_DIFF)) + 
      geom_bar(stat='identity',color='black',position=position_dodge()) + 
      theme_minimal() +
      ggtitle('Aggregate Deaths Per 100k')
    
    infection.plot <- ggplot(data=rolling.week.agg, aes(x=REPORT_DATE, y=`Infection Rate Per 100k`, fill=POLITICAL_DIFF)) + 
      geom_bar(stat='identity',color='black',position=position_dodge()) + 
      theme_minimal() +
      ggtitle('Aggregate Infections Per 100k')
    
    output$analysis.out2 <- renderUI({
      tagList(
        DT::renderDT(rolling.week.agg), 
        ggplotly(death.plot),
        ggplotly(infection.plot)
      )
    })
    
    
    
    
    # Create Ouptuts for By-County Analysis:
    
    exploratory.data.table <- most.recent.data %>% 
      select(c('ST','COUNTY','FIPS','CONFIRMED','DEATHS','Total Population',
               'INFECTION_RATE_100K','DEATH_RATE_100k','MAJORITY','Governor',
               'Party','Gender','POLITICAL_DIFF','Homicide.rate','Violent.crime','Unemployment',
               'Uninsured','Diabetes','Adult.obesity','Adult.smoking',
               'Children.in.single.parent.households','Teen.births',
               'Median Age','White','Black','Asian','Hispanic','Amerindian','Other',
               'School Enrollment','Less Than High School Diploma','At Least High School Diploma',
               "At Least Bachelors's Degree",'Graduate Degree'))
    
    race.table <- exploratory.data.table %>% 
      select('COUNTY','DEATH_RATE_100k','White','Black','Asian','Hispanic','Amerindian','Other') %>%
      mutate(
        white.rate = White*DEATH_RATE_100k*0.01,
        black.rate = Black*DEATH_RATE_100k*0.01,
        asian.rate = Asian*DEATH_RATE_100k*0.01,
        hispanic.rate = Hispanic*DEATH_RATE_100k*0.01,
        amerindian.rate = Amerindian*DEATH_RATE_100k*0.01,
        other.rate = Other*DEATH_RATE_100k*0.01
      )
    
    # Determine if data is filtered on a single state:
    state.filter <- ifelse(length(unique(exploratory.data.table$ST))>1, FALSE, TRUE)

    if (state.filter){
      # Create Output geared towards a single state (county-based)
      death.by.majority.plot <- ggplot(data=exploratory.data.table, aes(x=MAJORITY, y=`DEATH_RATE_100k`, fill=COUNTY)) + 
        geom_bar(stat='identity',color='black',position=position_dodge()) + 
        theme_minimal() +
        ggtitle('Death Rate (per 100k) by County Majority Polical Party')
      
      death.by.race <- ggplot(data=race.table, aes(x=COUNTY, ))
      
      
    } else {
      # Create Output geared towards all States (state-based): 
      death.by.majority.plot <- ggplot(data=exploratory.data.table, aes(x=Party, y=`DEATH_RATE_100k`, fill=ST)) + 
        geom_bar(stat='identity',color='black',position=position_dodge()) + 
        theme_minimal() +
        ggtitle('Death Rate (per 100k) by STATE Majority Polical Party')
      
      
    }
    
    
    
    

    
    infection.by.majority.plot <- ggplot(data=exploratory.data.table, aes(x=MAJORITY, y=`Death Rate Per 100k`, fill=POLITICAL_DIFF)) + 
      geom_bar(stat='identity',color='black',position=position_dodge()) + 
      theme_minimal() +
      ggtitle('Death Rate (per 100k) by County Majority Polical Party')
    
    death.by.governor.plot <- ggplot(data=exploratory.data.table, aes(x=Party, y=`Death Rate Per 100k`, fill=POLITICAL_DIFF)) + 
      geom_bar(stat='identity',color='black',position=position_dodge()) + 
      theme_minimal() +
      ggtitle('Death Rate (per 100k) by Governor Polical Party')
    
    infection.by.governor.plot <- ggplot(data=exploratory.data.table, aes(x=Party, y=`Death Rate Per 100k`, fill=POLITICAL_DIFF)) + 
      geom_bar(stat='identity',color='black',position=position_dodge()) + 
      theme_minimal() +
      ggtitle('Death Rate (per 100k) by Governor Polical Party')
    
    output$analysis.out3 <- renderUI({
      tagList(
        DT::renderDT(exploratory.data.table),
        
      )
    })
  }

  
}
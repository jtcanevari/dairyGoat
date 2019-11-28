library(shinydashboard); library(leaflet)

header <- dashboardHeader(
  title = "Dairy herd health"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Reproduction", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Kidding rate analyses", tabName = "CalvingRateAnalyses"),
             menuSubItem("Submission rate analyses", tabName = "SubmissionRateAnalyses"),
             menuSubItem("Return interval analyses", tabName = "ReturnIntervalAnalyses"),
             menuSubItem("Conception rate analyses", tabName = "ConceptionRateAnalyses"),
             menuSubItem("In-kid analyses", tabName = "InCalfAnalyses")),
    
    menuItem("Production", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Group", tabName = "ProductionByGroup"),
             menuSubItem("Individual", tabName = "ProductionByInd")),
    
    menuItem("Milk quality", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Group", tabName = "ICSCCByGroup"),
             menuSubItem("Individual", tabName = "ICSCCByInd")),
    
    menuItem("Weight and condition score", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Group", tabName = "WeightByGroup"),
             menuSubItem("Individual", tabName = "WeightByInd")),

    menuItem("Health", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Group", tabName = "HealthByGroup")),
        
    menuItem("Demographics", icon = icon("bar-chart-o"), startExpanded = FALSE,
             menuSubItem("Preweaning mortality", tabName = "PreweaningMortality"))
  )
)


body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "CalvingRateAnalyses", h2("Kidding rate analyses"), width = 24,
            fluidRow(
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "cstart", label = "Planned Start of Kidding:", value = "2016-06-01", 
                            min = NULL, max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "cstop", label = "Date to end kidding analysis:", value = "2016-07-31", 
                            min = NULL, max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Counts of kidding outcomes for period, by age:", tableOutput("tblCalvingOutcomes")),
              
              box(title = "Number of kidding events by week following Planned Start of Kidding, by age:", tableOutput("tblCalvingCounts")),
              
              box(title = "Percentage of kidding events by week following Planned Start of Kidding, by age:", tableOutput("tblCalvingPercent")),
              
              box(title = "Frequency histogram showing the number of kidding events by day for period:", 
                  plotOutput("pltCalvings", width = 500, height = 500)))),
    
    
    tabItem(tabName = "SubmissionRateAnalyses", h2("Submission rate analyses"), width = 24,
            fluidRow(
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "sstart", label = "Planned Start of Mating start:", value = "1993-06-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "sstop", label = "Planned Start of Mating end:", value = "1993-08-10", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Number of submission events by week following Planned Start of Mating, by age:", 
                  tableOutput("tblSubmissionCounts")),
              
              box(title = "Cumulative percentage of submission events by week following Planned Start of Mating, by age:", 
                  tableOutput("tblSubmissionPercent")),
              
              box(title = "Cumulative proportion of does to be mated following Planned Start of Mating:", 
                  plotOutput("pltSubmission", width = 500, height = 500)))),
    
    
    tabItem(tabName = "ReturnIntervalAnalyses", h2("Return interval analyses"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "rstart", label = "Heat and service date start:", value = "1993-06-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "rstop", label = "Heat and service date end:", value = "1993-08-10", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Number and percentage of return intervals by category:", 
                  tableOutput("tblReturnIntervalCountsPercent")),
              
              box(title = "Frequency histogram of return intervals for period:", 
                  plotOutput("pltReturnInterval", width = 500, height = 500)))),
    
    
    tabItem(tabName = "ConceptionRateAnalyses", h2("Conception rate analyses"), width = 24,
            fluidRow(
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "crstart", label = "Service date start:", value = "1993-06-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "crstop", label = "Service date end:", value = "1993-08-10", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Conception rates by age:", 
                  tableOutput("tblConceptionRates")))),
    
    tabItem(tabName = "InCalfAnalyses", h2("In-kid analyses"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "icstart", label = "Planned Start of Mating start:", value = "2016-01-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "icstop", label = "Planned Start of Mating end:", value = "2016-02-28", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Number of does in-kid by week following Planned Start of Mating, by age:", 
                  tableOutput("tblInCalfCounts")),
              
              box(title = "Cumulative percentage of does in-kid by week following Planned Start of Mating, by age:", 
                  tableOutput("tblInCalfPercent")),
              
              box(title = "Cumulative proportion of does in-kid following Planned Start of Mating:", 
                  plotOutput("pltInCalf", width = 500, height = 500)))),
    
    tabItem(tabName = "ProductionByGroup", h2("Production by group"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "gpstart", label = "Start of analysis period:", value = "2016-01-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "gpstop", label = "End of analysis period:", value = "2016-12-31", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Descriptive statistics of daily milk volumes (L) by age and days in milk:", 
                  tableOutput("tblGrpDailyYield")),
              
              box(title = "Estimated 305-day milk volume (L) by age:", 
                  tableOutput("tblGrpLactYield")),
              
              box(title = "Average daily milk volumes (L) by days in milk (all age groups):", 
                  plotOutput("pltGrpDailyYield", width = 500, height = 500)))),
    
    tabItem(tabName = "ProductionByInd", h2("Production by individual"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "ipstart", label = "Start of analysis period:", value = "2016-01-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "ipstop", label = "End of analysis period:", value = "2016-12-31", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  numericInput(inputId = "ipcowid", label = "Ear tag", value = 100058, min = NA, max = NA, step = NA, width = NULL)),
              
              box(title = "Descriptive statistics of daily milk volumes (L) by days in milk:", 
                  tableOutput("tblIndDailyYield")),
              
              box(title = "Estimated 305-day milk volume (L):", 
                  tableOutput("tblIndLactYield")),
              
              box(title = "Average daily milk volume by days in milk:", 
                  plotOutput("pltIndDailyYield", width = 500, height = 500)))),
    
    tabItem(tabName = "ICSCCByGroup", h2("Somatic cell counts by group"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "icscstart", label = "Start of analysis period:", value = "1998-09-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "icscstop", label = "End of analysis period:", value = "1999-04-14", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Individual doe somatic cell count frequencies (all age groups):", 
                  plotOutput("pltGrpICSCC", width = 500, height = 500)))),
    
    tabItem(tabName = "ICSCCByInd", h2("Somatic cell counts by individual"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "picscstart", label = "Start of analysis period:", value = "1998-09-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "picscstop", label = "End of analysis period:", value = "1999-04-14", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  numericInput(inputId = "picsccowid", label = "Ear tag", value = 103, min = NA, max = NA, step = NA, width = NULL)),
              
              box(title = "Individual doe somatic cell counts by days in milk:", 
                  plotOutput("pltIndICSCC", width = 500, height = 500)))),
    
    
    tabItem(tabName = "WeightByGroup", h2("Weight and body condition by group"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "gwstart", label = "Start of analysis period:", value = "2015-07-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "gwstop", label = "End of analysis period:", value = "2016-06-30", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Descriptive statistics of body weight (kg) by age and days in milk:", 
                  tableOutput("tblGrpWeight")),
              
              box(title = "Body weight (kg) by days in milk (all age groups):", 
                  plotOutput("pltGrpWeight", width = 500, height = 500)))),
    
    tabItem(tabName = "WeightByInd", h2("Weight and body condition by individual"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "iwstart", label = "Start of analysis period:", value = "2015-07-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "iwstop", label = "End of analysis period:", value = "2016-06-30", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  numericInput(inputId = "iwcowid", label = "Ear tag", value = 100058, min = NA, max = NA, step = NA, width = NULL)),
              
              box(title = "Descriptive statistics of body weight (kg) by days in milk:", 
                  tableOutput("tblIndWeight")),
              
              box(title = "Body weight (kg) by days in milk:", 
                  plotOutput("pltIndWeight", width = 500, height = 500)))),

    tabItem(tabName = "HealthByGroup", h2("Health events by group"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "dfstart", label = "Start of analysis period:", value = "1998-07-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "dfstop", label = "End of analysis period:", value = "1999-04-30", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  textInput(inputId = "dtype", label = "Disease event type:", value = "DLAME")),
              
              box(title = "Disease events per 100 does per day (all age groups):", 
                  plotOutput("pltDisFreqDim", width = 500, height = 500)))),
    
    
        
    tabItem(tabName = "PreweaningMortality", h2("Preweaning mortality rate"), width = 24,
            fluidRow(
              
              box(title = "Input data:", background = "black", solidHeader = TRUE,
                  dateInput(inputId = "zstart", label = "Start of analysis period:", value = "2015-06-01", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy"),
                  dateInput(inputId = "zstop", label = "End of analysis period:", value = "2016-09-30", min = NULL, 
                            max = NULL, format = "dd-mm-yyyy")),
              
              box(title = "Estimated preweaning mortality rate:", 
                  plotOutput("pltPreweaningMortality", width = 500, height = 500))))
    
  ))

dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "yellow"
)
library(shiny); library(shinydashboard); library(ggplot2); library(survival); library(scales); library(dplyr); library(epiR)

function(input, output, server){

  # ========================================================================================================================== 
  # CALVING RATES:
  
  # Counts of kidding outcomes for period, by age:
  CalvingOutcomes <- reactive({
    source("./code/fCalvingOutcomes.r")
    fCalvingOutomes(cstart = input$cstart, cstop = input$cstop, 
                    tcalvings = "./data/Meredith/calvings.csv", 
                    tcows = "./data/Meredith/cows.csv")
  })
  
  output$tblCalvingOutcomes <- renderTable({
    CalvingOutcomes()
  }, rownames = TRUE, striped = TRUE)
  
  # Number of kidding events by week following Planned Start of Kidding, by age:
  CalvingCounts <- reactive({
    source("./code/fCalvingCounts.r")
    fCalvingCounts(cstart = input$cstart, cstop = input$cstop, 
                   tcalvings = "./data/Meredith/calvings.csv", 
                   tcows = "./data/Meredith/cows.csv")
  })
  
  output$tblCalvingCounts <- renderTable({
    CalvingCounts()
  }, rownames = TRUE, striped = TRUE)
  
  # Percentage of kidding events by week following Planned Start of Kidding, by age:
  CalvingPercent <- reactive({
    
    source("./code/fCalvingPercent.r")
    fCalvingPercent(cstart = input$cstart, cstop = input$cstop, 
                    tcalvings = "./data/Meredith/calvings.csv", 
                    tcows = "./data/Meredith/cows.csv")
  })
  
  output$tblCalvingPercent <- renderTable({
    CalvingPercent()
  }, rownames = TRUE, striped = TRUE)
  
  # Frequency histogram showing the number of kidding events by day for period:
  output$pltCalvings <- renderPlot({
    source("./code/fpltCalvings.r")
    fpltCalvings(cstart = input$cstart, cstop = input$cstop, 
                 tcalvings = "./data/Meredith/calvings.csv")
  })
  
  
  # ========================================================================================================================== 
  # SUBMISSION RATES:
  
  # Number of submission events by week following Planned Start of Mating:
  SubmissionCounts <- reactive({
    source("./code/fSubmissionCounts.r")
    fSubmissionCounts(sstart = input$sstart, sstop = input$sstop, 
                      tpsm = "./data/Massey-01_1993/psm.csv", 
                      tservs = "./data/Massey-01_1993/servs.csv", 
                      tcows  = "./data/Massey-01_1993/cows.csv")
  })
  
  output$tblSubmissionCounts <- renderTable({
    SubmissionCounts()
  }, rownames = TRUE, striped = TRUE)
  
  # Percentage of submission events by week following Planned Start of Mating:
  SubmissionPercent <- reactive({
    source("./code/fSubmissionPercent.r")
    fSubmissionPercent(sstart = input$sstart, sstop = input$sstop, 
                       tpsm = "./data/Massey-01_1993/psm.csv", 
                       tservs = "./data/Massey-01_1993/servs.csv", 
                       tcows  = "./data/Massey-01_1993/cows.csv")
  })
  
  output$tblSubmissionPercent <- renderTable({
    SubmissionPercent()
  }, rownames = TRUE, striped = TRUE)
  
  # Cumulative proportion of does to be mated following Planned Start of Mating:
  output$pltSubmission <- renderPlot({
    source("./code/fpltSubmission.r")
    fpltSubmission(sstart = input$sstart, sstop = input$sstop, 
                   tpsm = "./data/Massey-01_1993/psm.csv", 
                   tservs = "./data/Massey-01_1993/servs.csv", 
                   tcows  = "./data/Massey-01_1993/cows.csv")
  })
  
  
  # ========================================================================================================================== 
  # RETURN INTERVALS:
  
  ReturnIntervalCountsPercent <- reactive({
    source("./code/fReturnIntervalCountsPercent.r")
    fReturnIntervalCountsPercent(rstart = input$sstart, rstop = input$sstop, 
                                 theats = "./data/Massey-01_1993/psm.csv", 
                                 tservs = "./data/Massey-01_1993/servs.csv", 
                                 tcows  = "./data/Massey-01_1993/cows.csv")
  })
  
  output$tblReturnIntervalCountsPercent <- renderTable({
    ReturnIntervalCountsPercent()
  }, rownames = TRUE, striped = TRUE)
  
  output$pltReturnInterval <- renderPlot({
    source("./code/fpltReturnInterval.r")
    fpltReturnInterval(rstart = input$sstart, rstop = input$sstop, 
                       theats = "./data/Massey-01_1993/psm.csv", 
                       tservs = "./data/Massey-01_1993/servs.csv", 
                       tcows  = "./data/Massey-01_1993/cows.csv")  
  })
  
  
  # ==========================================================================================================================   
  # CONCEPTION RATES:
  
  ConceptionRates <- reactive({ 
    source("./code/fConceptionRates.r")
    fConceptionRates(crstart = input$crstart, crstop = input$crstop, 
                     tservs = "./data/Massey-01_1993/servs.csv", 
                     tvexams = "./data/Massey-01_1993/vetReproExams.csv", 
                     tcows  = "./data/Massey-01_1993/cows.csv")  
  })
  
  output$tblConceptionRates <- renderTable({
    ConceptionRates()
  }, rownames = TRUE, striped = TRUE)
  
  
  # ==========================================================================================================================  
  # IN-CALF RATES:
  
  # Number of submission events that ended in pregnancy by week following Planned Start of Mating:
  InCalfCounts <- reactive({
    source("./code/fInCalfCounts.r")
    # fInCalfCounts(icstart = "01-01-2016", icstop = "28-02-2016",
    fInCalfCounts(icstart = input$icstart, icstop = input$icstop, 
                  tjoin = "./data/Meredith/GPN_joinings_121216.csv",
                  tcalv = "./data/Meredith/GPN_calv_121216.csv",
                  tvexams = "./data/Meredith/GPN_pregCheck_121216.csv", 
                  tcows  = "./data/Meredith/GPN_cows_121216.csv")  
  })
  
  output$tblInCalfCounts <- renderTable({
    InCalfCounts()
  }, rownames = TRUE, striped = TRUE)
  
  # Percentage of conception events by week following Planned Start of Mating:
  InCalfPercent <- reactive({
    source("./code/fInCalfPercent.r")
    fInCalfPercent(icstart = input$icstart, icstop = input$icstop, 
                   tjoin = "./data/Meredith/GPN_joinings_121216.csv",
                   tcalv = "./data/Meredith/GPN_calv_121216.csv",
                   tvexams = "./data/Meredith/GPN_pregCheck_121216.csv", 
                   tcows  = "./data/Meredith/GPN_cows_121216.csv") 
  })
  
  output$tblInCalfPercent <- renderTable({
    InCalfPercent()
  }, rownames = TRUE, striped = TRUE)
  
  # Cumulative proportion of does to be mated following Planned Start of Mating:
  output$pltInCalf <- renderPlot({
    source("./code/fpltInCalf.r")
    fpltInCalf(icstart = input$icstart, icstop = input$icstop, 
               tjoin = "./data/Meredith/GPN_joinings_121216.csv",
               tcalv = "./data/Meredith/GPN_calv_121216.csv",
               tvexams = "./data/Meredith/GPN_pregCheck_121216.csv", 
               tcows  = "./data/Meredith/GPN_cows_121216.csv") 
  })
  
  
  # ==========================================================================================================================  
  # PRODUCTION BY GROUP:
  
  GrpDailyYield <- reactive({
    source("./code/fGrpDailyYield.r")
    # fGrpDailyYield(gpstart = "01-01-2016", gpstop = "31-12-2016", 
    fGrpDailyYield(gpstart = input$gpstart, gpstop = input$gpstop, 
                   tcows = "./data/Meredith/cows.csv",
                   tcalvings = "./data/Meredith/calvings.csv",              
                   tproduction = "./data/Meredith/production.csv") 
  })
  
  output$tblGrpDailyYield <- renderTable({
    GrpDailyYield()
  }, rownames = FALSE, striped = TRUE)  
  
  GrpLactYield <- reactive({
    source("./code/fGrpLactYield.r")
    # fGrpLactYield(gpstart = "01-01-2016", gpstop = "31-12-2016", 
    fGrpLactYield(gpstart = input$gpstart, gpstop = input$gpstop, 
                  tcows = "./data/Meredith/cows.csv",
                  tcalvings = "./data/Meredith/calvings.csv",              
                  tproduction = "./data/Meredith/production.csv") 
  })
  
  output$tblGrpLactYield <- renderTable({
    GrpLactYield()
  }, rownames = TRUE, striped = TRUE)  
  
  output$pltGrpDailyYield <- renderPlot({
    source("./code/fpltGrpDailyYield.r")
    # fpltGrpDailyYield(gpstart = "01-01-2016", gpstop = "31-12-2016", 
    fpltGrpDailyYield(gpstart = input$gpstart, gpstop = input$gpstop, 
                      tcows = "./data/Meredith/cows.csv",
                      tcalvings = "./data/Meredith/calvings.csv",              
                      tproduction = "./data/Meredith/production.csv") 
  })
  
  
  # ==========================================================================================================================    
  # PRODUCTION BY INDIVIDUAL:
  
  IndDailyYield <- reactive({
    source("./code/fIndDailyYield.r")
    fIndDailyYield(ipstart = input$ipstart, ipstop = input$ipstop, ipcowid = input$ipcowid,  
                   tcows = "./data/Meredith/cows.csv",
                   tcalvings = "./data/Meredith/calvings.csv",              
                   tproduction = "./data/Meredith/production.csv")   
  })
  
  output$tblIndDailyYield <- renderTable({
    IndDailyYield()
  }, rownames = FALSE, striped = TRUE)  
  
  IndLactYield <- reactive({
    source("./code/fIndLactYield.r")
    fIndLactYield(ipstart = input$ipstart, ipstop = input$ipstop, ipcowid = input$ipcowid,  
                  tcows = "./data/Meredith/cows.csv",
                  tcalvings = "./data/Meredith/calvings.csv",              
                  tproduction = "./data/Meredith/production.csv")   
    
  })
  
  output$tblIndLactYield <- renderTable({
    IndLactYield()
  }, rownames = TRUE, striped = TRUE)  
  
  output$pltIndDailyYield <- renderPlot({
    source("./code/fpltIndDailyYield.r")
    fpltIndDailyYield(ipstart = input$ipstart, ipstop = input$ipstop, ipcowid = input$ipcowid,  
                      tcows = "./data/Meredith/cows.csv",
                      tcalvings = "./data/Meredith/calvings.csv",              
                      tproduction = "./data/Meredith/production.csv")    
  })
  
  
  # ==========================================================================================================================
  # SOMATIC CELL COUNTS BY GROUP:
  
  output$pltGrpICSCC <- renderPlot({
    source("./code/fpltGrpICSCC.r")
    fpltGrpICSCC(icscstart = input$icscstart, icscstop = input$icscstop,  
                 tcows = "./data/other/DW06_cows.csv",
                 tcalvings = "./data/other/DW06_calvings.csv",              
                 tproduction = "./data/other/DW06_production.csv")    
  })
  
  
  # ==========================================================================================================================
  # SOMATIC CELL COUNTS BY INDIVIDUAL:
  
  output$pltIndICSCC <- renderPlot({
    source("./code/fpltIndICSCC.r")
    fpltIndICSCC(picscstart = input$picscstart, picscstop = input$picscstop, picsccowid = input$picsccowid,  
                 tcows = "./data/other/DW06_cows.csv",
                 tcalvings = "./data/other/DW06_calvings.csv",              
                 tproduction = "./data/other/DW06_production.csv")    
  })
  

  # ========================================================================================================================== 
  # HEALTH EVENTS BY GROUP:
  
  output$pltDisFreqDim <- renderPlot({
    source("./code/fpltDisFreqDim.r")
    fpltDisFreqDim(dfstart = input$dfstart, dfstop = input$dfstop, dtype = input$dtype, 
                   tcows = "./data/other/DW06_cows.csv",
                   tcalvings = "./data/other/DW06_calvings.csv",              
                   tdiseases = "./data/other/DW06_diseases.csv")    
  })
  
  
  # ==========================================================================================================================    
  # WEIGHT BY GROUP:  
  
  GrpWeight <- reactive({
    source("./code/fGrpWeight.r")
    fGrpWeight(ipstart = input$gwstart, ipstop = input$gwstop,  
               tcows = "./data/Meredith/cows.csv",
               tcalvings = "./data/Meredith/calvings.csv",              
               twhcs = "./data/Meredith/weightHeightCS.csv") 
  })
  
  output$tbltblGrpWeight <- renderTable({
    GrpWeight()
  }, rownames = FALSE, striped = TRUE)  
  
  output$pltGrpWgt <- renderPlot({
    source("./code/fpltGrpWgt.r")
    fpltGrpWgt(ipstart = input$gwstart, ipstop = input$gwstop, 
               tcows = "./data/Meredith/cows.csv",
               tcalvings = "./data/Meredith/calvings.csv",              
               twhcs = "./data/Meredith/weightHeightCS.csv") 
  })
  
  
  # ==========================================================================================================================
  # WEIGHT BY INDIVIDUAL:
  
  IndWeight <- reactive({
    source("./code/fIndWeight.r")
    fIndWeight(iwstart = input$iwstart, iwstop = input$iwstop, iwcowid = input$iwcowid,  
               tcows = "./data/Meredith/cows.csv",
               tcalvings = "./data/Meredith/calvings.csv",              
               twhcs = "./data/Meredith/weightHeightCS.csv")
  })
  
  output$tblIndWeight <- renderTable({
    IndWeight()
  }, rownames = FALSE, striped = TRUE)  
  
  output$pltIndWeight <- renderPlot({
    source("./code/fpltIndWeight.r")
    fpltIndWeight(iwstart = input$iwstart, iwstop = input$iwstop, iwcowid = input$iwcowid,  
                  tcows = "./data/Meredith/cows.csv",
                  tcalvings = "./data/Meredith/calvings.csv",              
                  twhcs = "./data/Meredith/weightHeightCS.csv")
  })
  
  
  # ==========================================================================================================================
  # DEMOGRAPHICS:
  
  output$pltPreweaningMortality <- renderPlot({
    period <- c("June-2015", "Sep-2015", "Nov-2015", "Mar-2016", "Jun-2016", "Sep-2016")
    nborn <- c((15541-15301+1), (15805-15575+1), 566, (152916-152551+1), (162240-162001+1), (162675-162241+1))
    nsurv <- c(211,197,505,335,203,324)
    ndead <- nborn - nsurv
    
    dat <- data.frame(period, ndead, nborn)
    
    rval <- epi.conf(as.matrix(cbind(dat$ndead, dat$nborn)), ctype = "prevalence", method = "exact", N = 1000, design = 1, conf.level = 0.95) * 100
    rval <- round(rval, digits = 0)
    rval <- data.frame(period, est = rval$est, low = rval$lower, upp = rval$upper)
    
    ggplot(rval, aes(x = period, y = est)) +
      geom_errorbar(aes(ymin = low, ymax = upp), width = 0.1) +
      geom_point() +
      xlab("Date") +
      ylab("Preweaning mortality rate")
  })
  
  
  # ==========================================================================================================================  
}

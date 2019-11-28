fpltInCalf <- function(icstart = input$icstart, icstop = input$icstop, tjoin, tcalv, tvexams, tcows){
  # icstart <- "01-01-2016"
  # icstop <- "28-02-2016"
  
  # Read in the join table and the calv table:
  join <- read.table(tjoin, header = TRUE, sep = ",")
  join <- join[,1:4]
  join$jeDate <- as.Date(as.character(join$jeDate), format = "%Y-%m-%d")
  join$jsDate <- as.Date(as.character(join$jsDate), format = "%Y-%m-%d")
  join$psmDate <- join$jsDate
  
  calv <- read.table(tcalv, header = TRUE, sep = ",")
  calv <- calv[,1:3]
  names(calv)[2] <- 'cowKey'
  calv$calvDate <- as.Date(as.character(calv$calvDate), format = "%Y-%m-%d")
  
  # Read in the vetRepExam table
  vexams <- read.table(tvexams, header = TRUE, sep = ",")
  vexams <- vexams[,1:5]
  vexams$vreDate <- as.Date(as.character(vexams$vreDate), format = "%Y-%m-%d")
  
  # Read om cows table
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,6,8,9,10,7,8)]
  names(cows)[1] <- 'cowKey'
  # cows$RemovalDate <- as.Date(as.character(cows$RemovalDate), format = "%Y/%m/%d")
  cows$BirthDate <- as.Date(as.character(cows$BirthDate), format = "%Y-%m-%d")
  
  # Look up birth date from cows table using cowkey as the key:
  join$birdate <- cows$BirthDate[match(join$CowKey, cows$cowKey)]
  
  # Calculate age (in years) at the time of PSM:
  join$page <- round(as.numeric((join$jsDate - join$birdate)/365), digits = 0)
  join$page[join$page > 8] <- NA
  
  # Find the corresponding pregTest
  index <- neardate(join$CowKey, vexams$cowKey, join$jsDate, vexams$vreDate, best = "after", nomatch = NA_integer_)
  
  join$vredate <- vexams$vreDate[index]
  join$diagnosis <- vexams$diagnosis[index]
  
  # Find the corresponding calvDate
  index <- neardate(join$CowKey, calv$cowKey, join$jsDate, calv$calvDate, best = "after", nomatch = NA_integer_)
  
  join$calvDate <- calv$calvDate[index]
  
  # Substract joining date from calving date and keep true data
  join$jscalv <- as.numeric(join$calvDate - join$jsDate)
  join$calvDate[join$jscalv < 120 | join$jscalv > 210] <- NA
  
  # Back calculate conception date
  join$conDate <- join$calvDate - 150
  
  # Calculate PSM to conception interval
  join$psmcon <- as.numeric(join$conDate - join$psmDate)
  
  # Now cut the data down to the PSM date range of interest:
  rval <- join %>%
    filter(psmDate >= as.Date(icstart, format = "%d-%m-%Y") & psmDate <= as.Date(icstop, format = "%d-%m-%Y"))
  
  rval$status <- ifelse(!is.na(rval$psmcon), 1, 0)
  rval$status <- ifelse(rval$psmcon<0, 0, 1)
  
  # Censor at 60 days:
  rval$psmcon[rval$status == 0] <- 60  
  rval$psmcon[rval$psmcom < 0] <- 0  
  
  # Create age categories:
  rval$cpage <- rval$page
  rval$cpage[rval$page >  4] <- 4
  
  rval$cpage <- factor(rval$cpage, levels = c(1:4), labels = c("1 yo", "2 yo", "3 yo", "4+ yo")) 
  
  # Cumulative proportion submitted for service, all ages:
  rval.km <- survfit(Surv(psmcon, status) ~ 1, conf.type = "log", type = "kaplan-meier", data = rval)
  rval.km <- data.frame(time = rval.km$time, sest = rval.km$surv, slow = rval.km$lower, supp = rval.km$upper) 
  
  ggplot(rval.km, aes(x = time)) + 
    geom_step(aes(y = 1-sest)) +
    geom_step(aes(y = 1-slow), linetype = 3) +
    geom_step(aes(y = 1-supp), linetype = 3) +
    # geom_ribbon(aes(ymin = slow, ymax = supp), alpha = 0.2) +
    ylim(0,1)+
    xlab("PSM to conception interval (days)") +
    ylab("Cumulative proportion to experience event") +
    scale_color_brewer(name = "Method", palette = "Set1")+
    geom_hline(yintercept = 0.82, linetype = "dashed", color = "red")
}
fInCalfPercent <- function(icstart = input$icstart, icstop = input$icstop, tjoin, tcalv, tvexams, tcows){
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
  
  # Censor at 60 days
  rval$psmcon[rval$status == 0] <- 60  
  rval$psmcon[rval$psmcom < 0] <- 0  
  
  # Create age categories:
  rval$cpage <- rval$page
  rval$cpage[rval$page >  4] <- 4
  
  rval$cpage <- factor(rval$cpage, levels = c(1:4), labels = c("1 yo", "2 yo", "3 yo", "4+ yo")) 
  
  # Cumulative proportion submitted for service and pregnant, by age:
  rval.km <- survfit(Surv(psmcon, status) ~ cpage, conf.type = "log", type = "kaplan-meier", data = rval)
  rval.km <- data.frame(strata = summary(rval.km)$strata, time = summary(rval.km)$time, sest = summary(rval.km)$surv, 
                        slow = summary(rval.km)$lower, supp = summary(rval.km)$upper)
  rval.km$strata <- gsub(pattern = 'cpage=', replacement = '', x = rval.km$strata)
  
  # Define a set of intervals for in-kid rate reporting:
  bins <- seq(from = 7, to = 49, by = 7)
  
  # Create data frame and then attach the nearest prior value from rval.km
  tdat <- data.frame(
    time =  rep(bins[1:length(bins)], length(unique(rval.km$strata))),
    age.group = rep(1:n_distinct(rval.km$strata), each = length(bins)), 
    value = NA
  )
  
  tdat$age.group <- factor(tdat$age.group, levels = c(1,2,3,4), labels = c("1 yo", "2 yo", "3 yo", "4+ yo")) 
  
  # Attach proportion pregnant from rval.km
  index <- neardate(tdat$age.group, rval.km$strata, tdat$time, rval.km$time, best = "prior", nomatch = NA_integer_)
  
  tdat$value <- rval.km$sest[index]
  tdat$value[is.na(tdat$value)] <- 1
  
  # Cumulative proportion submitted for service and pregnant, all ages:
  rval.km2 <- survfit(Surv(psmcon, status) ~ 1, conf.type = "log", type = "kaplan-meier", data = rval)
  rval.km2 <- data.frame(time = rval.km2$time, sest = rval.km2$surv, slow = rval.km2$lower, supp = rval.km2$upper)
  
  tdat.all <- data.frame(
    time =  rep(bins[1:length(bins)]),
    age.group = rep('Total', each = length(bins)), 
    value = NA
  )
  
  index <- NA
  for(i in 1:length(bins)){
    index[i] <-   which.min(tdat.all$time[i] - rval.km2$time > 0)
  }
  
  tdat.all$value <- rval.km2$sest[index]
  tdat.all$value[is.na(tdat.all$value)] <- 1
  
  tdat <- rbind(tdat, tdat.all)
  rval <- as.data.frame(matrix(tdat$value, ncol = length(bins), nrow = 5, byrow = TRUE))
  
  rval$TOTAL <- c(
    min(rval.km$sest[rval.km$strata == '1 yo']),
    min(rval.km$sest[rval.km$strata == '2 yo']),
    min(rval.km$sest[rval.km$strata == '3 yo']),
    min(rval.km$sest[rval.km$strata == '4+ yo']),
    min(rval.km2$sest))
  
  names(rval)[1:length(bins)] <- paste("WK ", 1:length(bins), sep = "")
  row.names(rval) <- c("1 yo", "2 yo", "3 yo", "4+ yo", "TOTAL")
  rval <- (1 - rval) * 100
  rval
}
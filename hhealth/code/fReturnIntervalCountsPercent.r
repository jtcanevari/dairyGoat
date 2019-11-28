fReturnIntervalCountsPercent <- function(rstart = input$rstart, rstop = input$rstop, theats, tservs, tcows){
  # rstart <- "01/06/1993"
  # rstop <- "10/08/1993"
  
  heats <- read.table(theats, header = TRUE, sep = ",")
  heats <- heats[,1:3]
  names(heats) <- c("hkey", "cowkey", "headate")
  heats$etype <- "HEAT"
  heats$headate <- as.Date(as.character(heats$headate), format = "%d/%m/%y")
  
  servs <- read.table(tservs, header = TRUE, sep = ",")
  servs <- servs[,1:6]
  names(servs) <- c("skey", "cowkey", "serdate", "sertype", "sersire","sertech")
  servs$etype <- "SERVE"
  servs$serdate <- as.Date(as.character(servs$serdate), format = "%d/%m/%y")
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,9,10)]
  names(cows) <- c("cowkey", "herdid", "lid", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  hcows <- cows %>%
    left_join(x = cows, y = heats, by = "cowkey") %>%
    select(cowkey, headate) %>%
    filter(!is.na(headate))
  names(hcows) <- c("cowkey","edate")
  
  # Another left join --- include all of the rows of x [cows], and the matching rows of y [servs]. 
  scows <- cows %>%
    left_join(x = cows, y = servs, by = "cowkey") %>%
    select(cowkey,serdate) %>%
    filter(!is.na(serdate))
  names(scows) <- c("cowkey","edate")
  
  # Append the heats and services tables:
  thserv <- rbind(hcows, scows)
  
  # Cut the data down to the date range of interest:
  thserv <- thserv %>%
    filter(edate >= as.Date(rstart, format = "%d/%m/%Y") & edate <= as.Date(rstop, format = "%d/%m/%Y"))
  
  # Sort the data in order of cowkey then edate:
  thserv <- thserv[order(thserv$cowkey, thserv$edate),] 
  
  # Work out the length of the previous heat-service interval:
  cuniq <- unique(thserv$cowkey)
  thserv$int <- NA
  rval <- thserv[1,]
  
  for(i in 1:length(cuniq)){
    id <- thserv$cowkey == cuniq[i]
    trval <- thserv[id,]
    
    if(nrow(trval) > 1){  
      for(j in 2:nrow(trval)){
        trval$int[j] <- as.numeric(trval$edate[j] - trval$edate[j - 1])     
      }
    }
    rval <- rbind(rval, trval)
  }
  
  # Drop the first row:
  thserv <- rval[-1,]
  # id <- thserv$cowkey == 633; thserv[id,]
  
  id <- !is.na(thserv$int)
  thserv <- thserv[id,]
  
  # Bin the heat-service intervals into 2-17 days, 18-24 days, 25-38 days, 39-45 days and 45+days:
  thserv$icat <- NA
  thserv$icat[thserv$int >= 2  & thserv$int <= 17] <- "02-17 days"
  thserv$icat[thserv$int >= 18 & thserv$int <= 24] <- "18-24 days"
  thserv$icat[thserv$int >= 25 & thserv$int <= 38] <- "25-38 days"
  thserv$icat[thserv$int >= 39 & thserv$int <= 45] <- "39-45 days"
  thserv$icat[thserv$int >  45] <- "45+ days"
  
  rval <- by(data = thserv$int, INDICES = thserv$icat, FUN = length)
  rval <- data.frame(n = as.vector(rval))
  row.names(rval) <- levels(factor(thserv$icat))
  rval$PERCENT <- round(rval$n / sum(rval$n) * 100, digits = 0)
  rval
}
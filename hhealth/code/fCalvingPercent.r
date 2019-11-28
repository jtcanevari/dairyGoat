fCalvingPercent <- function(cstart = input$cstart, cstop = input$cstop, tcalvings, tcows){
  # cstart <- "01/6/2016"
  # cstop <- "31/8/2016"
  
  # Read in the calvings table and the cows table:
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%y")
  
  id <- calvings$clvdate >= as.Date(cstart, format = "%d/%m/%Y") & calvings$clvdate <= as.Date(cstop, format = "%d/%m/%Y")
  calvings <- calvings[id,1:5]
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,9,10)]
  names(cows) <- c("cowkey", "herdid", "lid", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  # Look up birth date from stock table using akey as the key:
  calvings$birdate <- cows$birdate[match(calvings$cowkey, cows$cowkey)]
  
  # Calculate age (in years) at the time of calving:
  calvings$cage <- round(as.numeric((calvings$clvdate - calvings$birdate)/365), digits = 0)
  calvings$cage[calvings$cage > 20] <- NA
  
  # Create age categories:
  calvings$ccage <- calvings$cage
  calvings$ccage[calvings$cage >= 4 & calvings$cage <= 8] <- 4
  calvings$ccage[calvings$cage >  8] <- 5
  # table(calvings$ccage)
  calvings$ccage <- factor(calvings$ccage, levels = c(2,3,4,5), labels = c("2 yo", "3 yo", "4-8 yo", "8+ yo"))    
  
  # Counts of kidding events by week and age:
  date.bins <- seq(from = as.Date(cstart, format = "%d/%m/%Y"), to = as.Date(cstop, format = "%d/%m/%Y"), by = "1 week")
  
  clvbyweek.2yo <- hist(calvings$clvdate[calvings$ccage == "2 yo"], breaks = date.bins, plot = FALSE)$counts
  clvbyweek.3yo <- hist(calvings$clvdate[calvings$ccage == "3 yo"], breaks = date.bins, plot = FALSE)$counts
  clvbyweek.4yo <- hist(calvings$clvdate[calvings$ccage == "4-8 yo"], breaks = date.bins, plot = FALSE)$counts
  clvbyweek.5yo <- hist(calvings$clvdate[calvings$ccage == "8+ yo"], breaks = date.bins, plot = FALSE)$counts
  clvbyweek.all <- hist(calvings$clvdate, breaks = date.bins, plot = FALSE)$counts
  
  rval <- data.frame(rbind(clvbyweek.2yo, clvbyweek.3yo, clvbyweek.4yo, clvbyweek.5yo, clvbyweek.all))
  names(rval) <- paste("WK ", 1:dim(rval)[2], sep = "")
  rval$TOTAL <- apply(rval, MARGIN = 1, FUN = sum)
  rval <- round((rval / rval$TOTAL) * 100, digits = 0)
  row.names(rval) <- c("2 yo", "3 yo", "4-8 yo", "8+ yo", "TOTAL")
  rval
} 
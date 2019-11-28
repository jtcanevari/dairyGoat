fCalvingOutomes <- function(cstart = input$cstart, cstop = input$cstop, tcalvings, tcows){
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
  table(calvings$ccage)
  calvings$ccage <- factor(calvings$ccage, levels = c(2,3,4,5), labels = c("2 yo", "3 yo", "4-8 yo", "8+ yo"))    
  
  # Fix up calving type:
  calvings$ctype <- as.character(calvings$ctype)
  calvings$ctype[calvings$ctype == ""] <- "UNK"
  calvings$ctype <- factor(calvings$ctype, levels = c("NORMAL", "INDUCE", "PREM", "ABORT", "UNK"))
  
  
  # Tabulate number of calving events by age group (as rows) and calving type (as columns):
  rval <- table(calvings$ccage, calvings$ctype)
  rval <- addmargins(rval)
  dimnames(rval)[[1]][5] <- "TOTAL"
  dimnames(rval)[[2]][6] <- "TOTAL"
  as.data.frame.matrix(rval)
}
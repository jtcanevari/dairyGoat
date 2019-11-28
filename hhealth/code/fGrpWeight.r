fGrpWeight <- function(gwstart = input$gwstart, gwstop = input$gwstop, tcows, tcalvings, twhcs){
  # gwstart <- "01-01-2016"
  # gwstop <- "30-06-2016"
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,9,10)]
  names(cows) <- c("cowkey", "herdid", "lid", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%y")
  
  whcs <- read.table(twhcs, header = TRUE, sep = ",")
  whcs <- whcs[,c(1,2,3,5,4)]
  names(whcs) <- c("wkey", "cowkey", "whcdate", "value", "etype")
  whcs$etype <- "WHCS"    
  whcs$whcdate <- as.Date(as.character(whcs$whcdate), format = "%d/%m/%y")
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [whcs].
  twhcs <- whcs %>%
    left_join(x = calvings, y = whcs, by = "cowkey") %>%
    filter(whcdate >= as.Date(gwstart, format = "%d-%m-%Y") & whcdate <= as.Date(gwstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(whcdate - clvdate)) %>%
    filter(dim >= 0 & dim < 300)
  
  # Attach birth date:
  twhcs$birdate <- cows$birdate[match(twhcs$cowkey, cows$cowkey)]
  
  # Calculate age (in years) on date of herd test event:
  twhcs$wage <- round(as.numeric((twhcs$whcdate - twhcs$birdate)/365), digits = 0)
  
  # Create age categories:
  twhcs$cwage <- twhcs$wage
  twhcs$cwage[twhcs$wage >= 4 & twhcs$wage <= 8] <- 4
  twhcs$cwage[twhcs$wage >  8] <- 5
  # table(twhcs$cwage)
  twhcs$cwage <- factor(twhcs$cwage, levels = c(1,2,3,4,5), labels = c("1 yo", "2 yo", "3 yo", "4-8 yo", "8+ yo"))
  
  id <- !is.na(twhcs$cwage)
  twhcs <- twhcs[id,]
  
  twhcs$cdim <- NA
  twhcs$cdim[twhcs$dim >= 0   & twhcs$dim < 100] <- "0-100 days"
  twhcs$cdim[twhcs$dim >= 100 & twhcs$dim < 200] <- "100-200 days"
  twhcs$cdim[twhcs$dim >= 200] <-                   ">200 days"
  twhcs$cdim <- factor(twhcs$cdim, levels = c("0-100 days", "100-200 days", ">200 days"))
  
  # Body weight by days in milk and age group:
  stwhcs <- twhcs %>%
    select(cowkey, value, cwage, cdim) %>%
    group_by(cwage, cdim)
  srval <- summarise(stwhcs, n = length(unique(cowkey)), mean = round(mean(value), digits = 0), min = round(min(value), digits = 0), max = round(max(value), digits = 0))
  srval <- data.frame(srval)
  
  # Body weight by days in milk:
  atwhcs <- twhcs %>%
    select(cowkey, value, cwage, cdim) %>%
    group_by(cdim)
  arval <- summarise(atwhcs, n = length(unique(cowkey)), mean = round(mean(value), digits = 0), min = round(min(value), digits = 0), max = round(max(value), digits = 0))
  arval <- data.frame(arval)
  arval$cwage <- "TOTAL"
  arval <- arval[,c(6,1,2,3,4,5)]
  
  spacer <- data.frame(cwage = "", cdim = "", n = "", mean = "", min = "", max = "")
  rval <- rbind(srval[1:3,], spacer, srval[4:6,], spacer, srval[7:9,], spacer, srval[10:12,], spacer, srval[13:15,], spacer, arval)
  names(rval) <- c("Age", "Days in milk", "n", "Mean", "Min", "Max")
  tGrpWeight <- rval
  tGrpWeight 
}
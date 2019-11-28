fIndWeight <- function(iwstart = input$iwstart, iwstop = input$iwstop, iwcowid = input$iwcowid, tcows, tcalvings, twhcs){
  # iwstart <- "01-06-2015"
  # iwstop <- "30-06-2016"
  # iwcowid <- 131717
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,8,9,10)]
  names(cows) <- c("cowkey", "herdid", "lid", "tag", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  # What is the cowkey for this animal?
  icowkey <- cows$cowkey[cows$tag == iwcowid]
  
  cows <- cows %>%
    filter(cowkey == icowkey)
  
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%y")
  
  calvings <- calvings %>%
    filter(cowkey == icowkey)
  
  whcs <- read.table(twhcs, header = TRUE, sep = ",")
  whcs <- whcs[,c(1,2,3,5,4)]
  names(whcs) <- c("wkey", "cowkey", "whcdate", "value", "etype")
  whcs$etype <- "WHCS"    
  whcs$whcdate <- as.Date(as.character(whcs$whcdate), format = "%d/%m/%y")
  
  whcs <- whcs %>%
    filter(cowkey == icowkey)
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [production].
  twhcs <- whcs %>%
    left_join(x = calvings, y = whcs, by = "cowkey") %>%
    filter(whcdate >= as.Date(iwstart, format = "%d-%m-%Y") & whcdate <= as.Date(iwstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(whcdate - clvdate)) %>%
    filter(dim >= 0 & dim < 300)
  
  twhcs$cdim <- NA
  twhcs$cdim[twhcs$dim >= 0   & twhcs$dim < 100] <- "0-100"
  twhcs$cdim[twhcs$dim >= 100 & twhcs$dim < 200] <- "100-200"
  twhcs$cdim[twhcs$dim >= 200] <-                   ">200"
  twhcs$cdim <- factor(twhcs$cdim, levels = c("0-100", "100-200", ">200"))
  
  # Milk yields by days in milk and age group:
  stwhcs <- twhcs %>%
    select(whcdate, value, cdim) %>%
    group_by(cdim)
  rval <- summarise(stwhcs, n = length(unique(whcdate)), mean = round(mean(value), digits = 0), min = round(min(value), digits = 0), max = round(max(value), digits = 0))
  
  rval <- cbind(tag = iwcowid, rval)
  names(rval) <- c("Tag", "DIM", "n", "Mean", "Min", "Max")
  rval
}
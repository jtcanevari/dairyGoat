fIndDailyYield <- function(ipstart = input$ipstart, ipstop = input$ipstop, ipcowid = input$ipcowid, tcows, tcalvings, tproduction){
  # ipstart <- "01-01-2016"
  # ipstop <- "01-01-2017"
  # ipcowid <- 100058
  # cows[cows$cowkey == 6492,]
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,8,9,10)]
  names(cows) <- c("cowkey", "herdid", "lid", "tag", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  # What is the cowkey for this animal?
  icowkey <- cows$cowkey[cows$tag == ipcowid]
  
  cows <- cows %>%
    filter(cowkey == icowkey)
  
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%y")
  
  calvings <- calvings %>%
    filter(cowkey == icowkey)
  
  production <- read.table(tproduction, header = TRUE, sep = ",")
  production <- production[,c(1:10)]
  names(production) <- c("pkey", "cowkey", "prodate", "ltddate", "htdatpc", "htpropc", "htlacpc", "htvolam", "htvolpm", "htvoltot")
  production$etype <- "PROD"
  production$prodate <- as.Date(as.character(production$prodate), format = "%d/%m/%y")
  
  production <- production %>%
    filter(cowkey == icowkey)
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [production].
  tprod <- production %>%
    left_join(x = calvings, y = production, by = "cowkey") %>%
    filter(prodate >= as.Date(ipstart, format = "%d-%m-%Y") & prodate <= as.Date(ipstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(prodate - clvdate)) %>%
    filter(dim >= 0 & dim < 300)
  
  tprod$cdim <- NA
  tprod$cdim[tprod$dim >= 0   & tprod$dim < 100] <- "0-100"
  tprod$cdim[tprod$dim >= 100 & tprod$dim < 200] <- "100-200"
  tprod$cdim[tprod$dim >= 200] <-                   ">200"
  tprod$cdim <- factor(tprod$cdim, levels = c("0-100", "100-200", ">200"))
  
  # Milk yields by days in milk and age group:
  stprod <- tprod %>%
    select(prodate, htvoltot, cdim) %>%
    group_by(cdim)
  rval <- summarise(stprod, n = length(unique(prodate)), mean = round(mean(htvoltot), digits = 0), min = round(min(htvoltot), digits = 0), max = round(max(htvoltot), digits = 0))
  
  rval <- cbind(tag = ipcowid, rval)
  names(rval) <- c("Tag", "DIM", "n", "Mean", "Min", "Max")
  rval
}
fIndLactYield <- function(ipstart = input$ipstart, ipstop = input$ipstop, ipcowid = input$ipcowid, tcows, tcalvings, tproduction){
  # ipstart <- "01-01-2016"
  # ipstop <- "31-12-2016"
  # ipcowid <- 100058
  
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
  
  # How many calving events for this animal?
  clvdate <- unique(tprod$clvdate)
  nclv <- length(unique(tprod$clvdate))
  ltd <- c()
  
  # Process each lactation:
  for(i in 1:nclv){
    tclvdate <- clvdate[i]
    
    .tprod <- tprod %>%
      filter(clvdate == tclvdate & htvoltot > 0)
    
    # Only proceed if more than 100 days data:
    if(nrow(.tprod) > 100){
      
      # Wood's curve:
      tmp.lm <- lm(log(htvoltot) ~ I(dim + 1) + I(log(dim + 1)), data = .tprod) 
      a <- exp(coefficients(tmp.lm)[1])
      b <- -coefficients(tmp.lm)[2]
      c <- coefficients(tmp.lm)[3]
      
      dim <- 1:305
      tltd <- round(sum(exp(log(a) - (b * (dim + 1)) + c * log(dim + 1))), digits = 0)
      
    }
    ltd <- c(ltd, tltd)
  }
  
  # Nasty hack. Drop implausible 305-day milk yields:
  ltd <- ifelse(ltd > 3500, 0, ltd)
  rval <- data.frame(Tag = rep(ipcowid, times = nclv), Kidding = as.character(clvdate), LTD = ltd)
  names(rval) <- c("Tag", "Kidding date", "305-day yield")
  rval
}
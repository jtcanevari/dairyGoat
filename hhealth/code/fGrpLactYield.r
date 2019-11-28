fGrpLactYield <- function(gpstart = input$gpstart, gpstop = input$gpstop, tcows, tcalvings, tproduction){
  # gpstart <- "01-01-2016"
  # gpstop <- "31-12-2016"

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
  
  production <- read.table(tproduction, header = TRUE, sep = ",")
  production <- production[,c(1:10)]
  names(production) <- c("pkey", "cowkey", "prodate", "ltddate", "htdatpc", "htpropc", "htlacpc", "htvolam", "htvolpm", "htvoltot")
  production$etype <- "PROD"
  production$prodate <- as.Date(as.character(production$prodate), format = "%d/%m/%y")
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [production].
  tprod <- production %>%
    left_join(x = calvings, y = production, by = "cowkey") %>%
    filter(prodate >= as.Date(gpstart, format = "%d-%m-%Y") & prodate <= as.Date(gpstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(prodate - clvdate)) %>%
    filter(dim >= 0 & dim < 350)
  
  # Attach birth date:
  tprod$birdate <- cows$birdate[match(tprod$cowkey, cows$cowkey)]
  
  # Calculate age (in years) on date of herd test event:
  tprod$page <- round(as.numeric((tprod$prodate - tprod$birdate)/365), digits = 0)
  
  # Create age categories:
  tprod$cpage <- tprod$page
  tprod$cpage[tprod$page >= 4 & tprod$page <= 8] <- 4
  tprod$cpage[tprod$page >  8] <- 5
  # table(tprod$cpage)
  tprod$cpage <- factor(tprod$cpage, levels = c(1,2,3,4,5), labels = c("1 yo", "2 yo", "3 yo", "4-8 yo", "8+ yo"))
  
  id <- !is.na(tprod$cpage)
  tprod <- tprod[id,]
  
  # Calculate the estimated 305-day milk yield for each cow:
  ucow <- unique(tprod$cowkey)
  cowkey <- c(); clvdate <- c(); cpage <- c(); ltd <- c()
  
  for(i in 1:length(ucow)){
    tcowkey <- ucow[i]
    .tprod <- tprod %>%
      filter(cowkey == tcowkey)
    
    # How many calving events for this animal?
    uclvdate <- unique(.tprod$clvdate)

    # Process each lactation:
    for(j in 1:length(uclvdate)){
      ..tprod <- .tprod %>%
        filter(clvdate == uclvdate[j] & htvoltot > 0)
      
      # Only proceed if more than 100 days data:
      if(nrow(..tprod) > 100){
        
        # Wood's curve:
        tmp.lm <- lm(log(htvoltot) ~ I(dim + 1) + I(log(dim + 1)), data = ..tprod) 
        a <- exp(coefficients(tmp.lm)[1])
        b <- -coefficients(tmp.lm)[2]
        c <- coefficients(tmp.lm)[3]
        
        dim <- 1:305
        tltd <- sum(exp(log(a) - (b * (dim + 1)) + c * log(dim + 1)))
        
        cowkey <- c(cowkey, tcowkey)
        cpage <- c(cpage, as.character(..tprod$cpage[1]))
        clvdate <- c(clvdate, as.character(uclvdate[j]))
        ltd <- c(ltd, tltd)
      
      } else {
        cowkey <- c(cowkey, tcowkey)
        cpage <- c(cpage, as.character(..tprod$cpage[1]))
        clvdate <- c(clvdate, as.character(uclvdate[j]))
        ltd <- c(ltd, NA)
      }
      
      
    }
  }
  
  rval <- data.frame(cowkey, clvdate, cpage, ltd)
  
  # Nasty hack. Drop implausible 305-day milk yields:
  rval <- rval %>%
    select(cpage, ltd) %>%
    filter(ltd < 2500) %>%
    group_by(cpage)
  rval <- data.frame(summarise(rval, n = length(ltd), Mean = round(mean(ltd), digits = 0), Min = round(min(ltd), digits = 0), Max = round(max(ltd), digits = 0)))
  names(rval) <- c("Age", "n", "Mean", "Min", "Max")
  rval
}
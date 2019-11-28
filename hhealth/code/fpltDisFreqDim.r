fpltDisFreqDim <- function(dfstart = input$dfstart, dfstop = input$dfstop, dtype = input$dtype, tcows, tcalvings, tdiseases){
  # dfstart <- "01-07-1998"
  # dfstop <- "30-04-1999"
  # dtype <- "DLAME"
  # tcows = "./data/other/DW06_cows.csv"
  # tcalvings = "./data/other/DW06_calvings.csv"
  # tdiseases = "./data/other/DW06_diseases.csv"
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,9,10)]
  names(cows) <- c("cowkey", "herdid", "lid", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%Y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%Y")
  
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%Y")
  
  diseases <- read.table(tdiseases, header = TRUE, sep = ",")
  diseases <- diseases[,1:6]
  names(diseases) <- c("diskey", "cowkey", "disdate", "distype", "vf", "reason")
  diseases$etype <- "DIS"
  diseases$disdate <- as.Date(as.character(diseases$disdate), format = "%d/%m/%Y")

  # Filter by selected disease type and selected dates:
  diseases <- diseases %>%
    filter(distype == dtype & disdate >= as.Date(dfstart, format = "%d-%m-%Y") & disdate <= as.Date(dfstop, format = "%d-%m-%Y"))
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [diseases].
  tdis <- left_join(x = calvings, y = diseases, by = "cowkey") %>%
    mutate(disdate = ifelse(is.na(disdate), as.Date(dfstop, format = "%d-%m-%Y"), disdate)) %>%
    mutate(disdate = as.Date(disdate, origin = "1970-01-01")) %>%
    mutate(dim = as.numeric(disdate - clvdate)) %>%
    filter(dim >= 0 & dim <= 400) %>%
    mutate(status = ifelse(!is.na(distype), 1, 0))

  # Kaplan-Meier survival curve:
  rval.km <- survfit(Surv(dim, status) ~ 1, conf.type = "log", type = "kaplan-meier", data = tdis)
  rval.haz <- epi.insthaz(rval.km)
  
  rval.haz$est <- lowess(rval.haz$time, rval.haz$est, f = 0.20)$y * 100
  rval.haz$lower <- lowess(rval.haz$time, rval.haz$low, f = 0.20)$y * 100
  rval.haz$upper <- lowess(rval.haz$time, rval.haz$upp, f = 0.20)$y * 100
  
  ggplot() +
    geom_line(data = rval.haz, aes(x = time, y = est), size = 1) +
    scale_x_continuous(breaks = seq(from = 0, to = 300, by = 50), limits = c(0, 300), name = "Days in milk") +
    scale_y_continuous(limits = c(0,0.1), name = "Disease events per 100 does per day") +
    scale_color_brewer(name = "Method", palette = "Set1") 
  
 
}
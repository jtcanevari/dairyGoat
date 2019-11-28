fpltGrpICSCC <- function(icscstart = input$icscstart, icscstop = input$icscstop, tcows, tcalvings, tproduction){
  # icscstart <- "01-09-1998"
  # icscstop <- "14-04-1999"
  # tcows = ".\\data\\other\\DW06_cows.csv"
  # tcalvings = ".\\data\\other\\DW06_calvings.csv"             
  # tproduction = ".\\data\\other\\DW06_production.csv"
  
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
  
  production <- read.table(tproduction, header = TRUE, sep = ",")
  production <- production[,c(1:10,13)]
  names(production) <- c("pkey", "cowkey", "prodate", "ltddate", "htdatpc", "htpropc", "htlacpc", "htvolam", "htvolpm", "htvoltot", "htscc")
  production$etype <- "PROD"
  production$prodate <- as.Date(as.character(production$prodate), format = "%d/%m/%Y")
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [production].
  tprod <- production %>%
    left_join(x = calvings, y = production, by = "cowkey") %>%
    filter(prodate >= as.Date(icscstart, format = "%d-%m-%Y") & prodate <= as.Date(icscstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(prodate - clvdate)) %>%
    filter(dim >= 0 & dim < 300)
  
  # spkey <- base::sample(tprod$pkey, size = 100, replace = FALSE)
  # id <- tprod$pkey %in% spkey
  # tprod <- tprod[id,]
  
  # tprod <- tprod %>% 
  #  ungroup() %>%
  #  sample_n(size = 1000, replace = FALSE)
  # dim(tprod) # 1000
  
  # Attach birth date:
  tprod$birdate <- cows$birdate[match(tprod$cowkey, cows$cowkey)]
  
  # Calculate age (in years) on date of weigh event:
  tprod$page <- round(as.numeric((tprod$prodate - tprod$birdate) / 365), digits = 0)
  
  # Create age categories:
  tprod$cpage <- tprod$page
  tprod$cpage[tprod$page >= 4 & tprod$page <= 8] <- 4
  tprod$cpage[tprod$page >  8] <- 5
  # table(tprod$cpage)
  tprod$cpage <- factor(tprod$cpage, levels = c(1,2,3,4,5), labels = c("1 yo", "2 yo", "3 yo", "4-8 yo", "8+ yo"))
  
  # id <- !is.na(tprod$cpage)
  # tprod <- tprod[id,]
  
  tprod$htscc[tprod$htscc > 1000] <- 1100
  
  ggplot(data = tprod, aes(x = htscc)) +
    geom_histogram(color = "grey", fill = "dark blue", binwidth = 100) +
    scale_x_continuous(breaks = seq(from = 100, to = 1100, by = 100), 
       labels = c(seq(from = 100, to = 1000, by = 100), "1000+"), limits = c(0,1100), 
       name = "Somatic cell count (x 1000 cells/mL)") +
    scale_y_continuous(name = "Frequency")
}
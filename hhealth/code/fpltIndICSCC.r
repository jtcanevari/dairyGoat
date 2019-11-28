fpltIndICSCC <- function(picscstart = input$picscstart, picscstop = input$picscstop, picsccowid = input$picsccowid, tcows, tcalvings, tproduction){
  # picscstart <- "01-09-1998"
  # picscstop <- "14-04-1999"
  # picsccowid <- 103
  # tcows = ".\\data\\other\\DW06_cows.csv"
  # tcalvings = ".\\data\\other\\DW06_calvings.csv"             
  # tproduction = ".\\data\\other\\DW06_production.csv"
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,35,7,8)]
  names(cows) <- c("cowkey", "herdid", "lid", "tag", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%Y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%Y")
  
  # What is the cowkey for this animal?
  icowkey <- cows$cowkey[cows$tag == picsccowid]
  
  # Select the ear tag without a removal date. TODO: modify function to allow data for culls to be reported.
  cows <- cows %>%
    filter(cowkey == icowkey & is.na(remdate))
  icowkey <- cows$cowkey
  
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%Y")
  
  calvings <- calvings %>%
    filter(cowkey == icowkey)
  
  production <- read.table(tproduction, header = TRUE, sep = ",")
  production <- production[,c(1:10,13)]
  names(production) <- c("pkey", "cowkey", "prodate", "ltddate", "htdatpc", "htpropc", "htlacpc", "htvolam", "htvolpm", "htvoltot", "htscc")
  production$etype <- "PROD"
  production$prodate <- as.Date(as.character(production$prodate), format = "%d/%m/%Y")
  
  production <- production %>%
    filter(cowkey == icowkey)
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [production].
  tprod <- production %>%
    left_join(x = calvings, y = production, by = "cowkey") %>%
    filter(prodate >= as.Date(picscstart, format = "%d-%m-%Y") & prodate <= as.Date(picscstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(prodate - clvdate)) %>%
    filter(dim >= 0 & dim < 300)
  
  ggplot(tprod, aes(x = dim, y = htscc)) + 
    geom_point() +
    geom_smooth() +
    xlab("Days in milk") +
    ylab("Somatic cell count (x 1000) cells/mL") +
    xlim(0,300) +
    ylim(0,1000) +
    scale_color_brewer(name = "Method", palette = "Set1")
}
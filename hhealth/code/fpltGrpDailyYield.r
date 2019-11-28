fpltGrpDailyYield <- function(gpstart = input$gpstart, gpstop = input$gpstop, tcows, tcalvings, tproduction){
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
    filter(dim >= 0 & dim < 300)
  
  spkey <- base::sample(tprod$pkey, size = 100, replace = FALSE)
  id <- tprod$pkey %in% spkey
  tprod <- tprod[id,]
  
  # tprod <- tprod %>% 
  #  ungroup() %>%
  #  sample_n(size = 1000, replace = FALSE)
  # dim(tprod) # 1000
  
  # Attach birth date:
  tprod$birdate <- cows$birdate[match(tprod$cowkey, cows$cowkey)]
  
  # Calculate age (in years) on date of weigh event:
  tprod$page <- round(as.numeric((tprod$prodate - tprod$birdate)/365), digits = 0)
  
  # Create age categories:
  tprod$cpage <- tprod$page
  tprod$cpage[tprod$page >= 4 & tprod$page <= 8] <- 4
  tprod$cpage[tprod$page >  8] <- 5
  # table(tprod$cpage)
  tprod$cpage <- factor(tprod$cpage, levels = c(1,2,3,4,5), labels = c("1 yo", "2 yo", "3 yo", "4-8 yo", "8+ yo"))
  
  id <- !is.na(tprod$cpage)
  tprod <- tprod[id,]
  
  ggplot(tprod, aes(x = dim, y = htvoltot)) + 
    geom_point() +
    # geom_smooth() +
    # facet_grid(cpage ~ .) +
    xlim(0,300) +
    ylim(0,10) +
    xlab("Days in milk") +
    ylab("Average daily milk volume (L)") +
    scale_color_brewer(name = "Method", palette = "Set1")
  
}
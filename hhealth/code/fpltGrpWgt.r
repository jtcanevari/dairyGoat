fpltGrpWgt <- function(gwstart = input$gwstart, gwstop = input$gwstop, tcows, tcalvings, twhcs){
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
  
  # swkey <- base::sample(twhcs$wkey, size = 1000, replace = FALSE)
  # id <- twhcs$pkey %in% swkey
  # twhcs <- twhcs[id,]
  
  # Attach birth date:
  twhcs$birdate <- cows$birdate[match(twhcs$cowkey, cows$cowkey)]
  
  # Calculate age (in years) on date of weigh event:
  twhcs$wage <- round(as.numeric((twhcs$whcdate - twhcs$birdate)/365), digits = 0)
  
  # Create age categories:
  twhcs$cwage <- twhcs$wage
  twhcs$cwage[twhcs$wage >= 4 & twhcs$wage <= 8] <- 4
  twhcs$cwage[twhcs$wage >  8] <- 5
  # table(twhcs$cwage)
  twhcs$cwage <- factor(twhcs$cwage, levels = c(1,2,3,4,5), labels = c("1 yo", "2 yo", "3 yo", "4-8 yo", "8+ yo"))
  
  id <- !is.na(twhcs$cwage)
  twhcs <- twhcs[id,]
  
  ggplot(twhcs, aes(x = dim, y = value)) + 
    geom_point() +
    # geom_smooth() +
    # facet_grid(cpage ~ .) +
    xlim(0,300) +
    ylim(0,150) +
    xlab("Days in milk") +
    ylab("Body weight (kg)") +
    scale_color_brewer(name = "Method", palette = "Set1")
}
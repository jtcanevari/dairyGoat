fpltIndWeight <- function(iwstart = input$iwstart, iwstop = input$iwstop, iwcowid = input$iwcowid, tcows, tcalvings, twhcs){
  # iwstart <- "01-01-2016"
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
  
  # Left join. Include all of the rows of x [calvings], and the matching rows of y [whcs].
  twhcs <- whcs %>%
    left_join(x = calvings, y = whcs, by = "cowkey") %>%
    filter(whcdate >= as.Date(iwstart, format = "%d-%m-%Y") & whcdate <= as.Date(iwstop, format = "%d-%m-%Y")) %>%
    mutate(dim = as.numeric(whcdate - clvdate)) %>%
    filter(dim >= 0 & dim < 300)
  
  ggplot(twhcs, aes(x = dim, y = value)) + 
    geom_point() +
    geom_smooth() +
    xlim(0,300) +
    ylim(0,150) +
    xlab("Days in milk") +
    ylab("Body weight (kg)") +
    scale_color_brewer(name = "Method", palette = "Set1")
}
fpltCalvings <- function(cstart = input$cstart, cstop = input$cstop, tcalvings){
  # cstart <- "01/3/2016"
  # cstop <- "01/5/2016"
  
  # Read in the calvings table and the cows table:
  calvings <- read.table(tcalvings, header = TRUE, sep = ",")
  calvings <- calvings[,1:5]
  names(calvings) <- c("calvkey", "cowkey", "clvdate", "ctype", "assist")
  calvings$etype <- "CALVE"
  calvings$clvdate <- as.Date(as.character(calvings$clvdate), format = "%d/%m/%y")
  
  id <- calvings$clvdate >= as.Date(cstart, format = "%d/%m/%Y") & calvings$clvdate <= as.Date(cstop, format = "%d/%m/%Y")
  calvings <- calvings[id,1:5]
  date.bins <- seq(from = as.Date(cstart, format = "%d/%m/%Y"), to = as.Date(cstop, format = "%d/%m/%Y"), by = "1 week")
  
  ggplot(calvings, aes(x = clvdate)) +
    geom_histogram(binwidth = 1, colour = "gray") +
    labs(x = "Date", y = "Number of calving events per day") +  
    scale_fill_manual(values = c("#2f4f4f", "red")) + 
    guides(fill = FALSE) +
    ylim(0,100) +
    scale_x_date(breaks = date_breaks("1 week"), labels = date_format("%d %b %Y")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
}
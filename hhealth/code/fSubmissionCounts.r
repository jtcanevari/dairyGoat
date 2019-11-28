fSubmissionCounts <- function(sstart = input$sstart, sstop = input$sstop, tpsm, tservs, tcows){
  # sstart <- "01/06/1993"
  # sstop <- "10/08/1993"
  
  # Read in the psm table and the servs table:
  psm <- read.table(tpsm, header = TRUE, sep = ",")
  psm <- psm[,1:4]
  names(psm) <- c("psmkey", "cowkey", "psmcdate", "psmdate")
  psm$etype <- "PSM"
  psm$psmcdate <- as.Date(as.character(psm$psmcdate), format = "%d/%m/%y")
  psm$psmdate <- as.Date(as.character(psm$psmdate), format = "%d/%m/%y")
  
  servs <- read.table(tservs, header = TRUE, sep = ",")
  servs <- servs[,1:6]
  names(servs) <- c("skey", "cowkey", "serdate", "sertype", "sersire","sertech")
  servs$etype <- "SERVE"
  servs$serdate <- as.Date(as.character(servs$serdate), format = "%d/%m/%y")
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,7,8)]
  names(cows) <- c("cowkey", "herdid", "lid", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  # Look up birth date from cows table using cowkey as the key:
  psm$birdate <- cows$birdate[match(psm$cowkey, cows$cowkey)]
  
  # Calculate age (in years) at the time of PSM:
  psm$page <- round(as.numeric((psm$psmdate - psm$birdate)/365), digits = 0)
  psm$page[psm$page > 20] <- NA
  
  # Include all of the rows of x [psm], and the matching rows of y [servs]. Left join important because some cows with a PSM may remain unmated.
  tpsm <- psm %>%
    left_join(x = psm, y = servs, by = "cowkey")
  
  # Check:
  # id <- psm$cowkey == 150; psm[id,]
  # id <- servs$cowkey == 150; servs[id,]
  # id <- psm$cowkey == 150; psm[id,]
  
  # Subtract serdate from psmdate:
  tpsm$psmser <- as.numeric(tpsm$serdate - tpsm$psmdate)
  
  # Drop those psm-service pairs with psmser intervals that are less than -7 days and greater than 150 days.
  # Need to fix. PSM to service intervals could be greater than 150 days.
  tpsm <- tpsm %>%
    filter(psmser >= -7 & psmser <= 150 | is.na(psmser)) %>%
    select(psmkey, cowkey, page, psmcdate, psmdate, skey, serdate, sertype, sersire, psmser) %>%
    arrange(cowkey, psmdate, psmser)
  
  # Now cut the data down to the PSM date range of interest:
  # table(tpsm$psmdate)
  tpsm <- tpsm %>%
    filter(psmdate >= as.Date(sstart, format = "%d/%m/%Y") & psmdate <= as.Date(sstop, format = "%d/%m/%Y"))
  
  # Group by cowkey and select the minimum of psmser interval:
  tpsm <- tpsm %>%
    group_by(cowkey)
  
  rval <- data.frame(summarise(tpsm, page = min(page), psmdate = min(psmdate), 
                               serdate = min(serdate), stype = n_distinct(sertype), psmser = min(psmser)))
  rval$status <- ifelse(!is.na(rval$psmser), 1, 0)
  
  # Create age categories:
  rval$cpage <- rval$page
  rval$cpage[rval$page >= 4 & rval$page <= 8] <- 4
  rval$cpage[rval$page >  8] <- 5
  # table(rval$cpage)
  rval$cpage <- factor(rval$cpage, levels = c(2,3,4,5), labels = c("2 yo", "3 yo", "4-8 yo", "8+ yo"))    
  
  # Counts of submission events by week and age following PSM:
  bins <- c(seq(from = 0, to = 42, by = 7), 100)
  
  subbyweek.2yo <- hist(rval$psmser[rval$cpage == "2 yo"], breaks = bins, plot = FALSE)$counts
  subbyweek.3yo <- hist(rval$psmser[rval$cpage == "3 yo"], breaks = bins, plot = FALSE)$counts
  subbyweek.4yo <- hist(rval$psmser[rval$cpage == "4-8 yo"], breaks = bins, plot = FALSE)$counts
  subbyweek.5yo <- hist(rval$psmser[rval$cpage == "8+ yo"], breaks = bins, plot = FALSE)$counts
  subbyweek.all <- hist(rval$psmser, breaks = bins, plot = FALSE)$counts
  
  rval <- data.frame(rbind(subbyweek.2yo, subbyweek.3yo, subbyweek.4yo, subbyweek.5yo, subbyweek.all))
  names(rval) <- paste("WK ", 1:dim(rval)[2], sep = "")
  rval$TOTAL <- apply(rval, MARGIN = 1, FUN = sum)
  # row.names(rval) <- c("2 yo", "3 yo", "4-8 yo", "8+ yo", "TOTAL")
  row.names(rval) <- c("1 yo", "2 yo", "3 yo", "4+ yo", "TOTAL")
  rval
}
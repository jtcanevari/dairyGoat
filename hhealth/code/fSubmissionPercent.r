fSubmissionPercent <- function(sstart = input$sstart, sstop = input$sstop, tpsm, tservs, tcows){
  # sstart <- "01-06-1993"
  # sstop <- "10-08-1993"
  
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
    filter(psmdate >= as.Date(sstart, format = "%d-%m-%Y") & psmdate <= as.Date(sstop, format = "%d-%m-%Y"))
  
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
  
  # Cumulative proportion submitted for service, by age:
  rval.km <- survfit(Surv(psmser, status) ~ cpage, conf.type = "log", type = "kaplan-meier", data = rval)
  rval.km <- data.frame(strata = summary(rval.km)$strata, time = summary(rval.km)$time, sest = summary(rval.km)$surv, 
                        slow = summary(rval.km)$lower, supp = summary(rval.km)$upper)
  
  # Define a set of intervals for submission rate reporting:
  bins <- seq(from = 0, to = 42, by = 7)
  
  sest.2yo <- round(spline(x = rval.km$time[rval.km$strata == "cpage=2 yo"], y = 1 - rval.km$sest[rval.km$strata == "cpage=2 yo"], xout = bins)$y * 100, digits = 0)
  sest.3yo <- round(spline(x = rval.km$time[rval.km$strata == "cpage=3 yo"], y = 1 - rval.km$sest[rval.km$strata == "cpage=3 yo"], xout = bins)$y * 100, digits = 0)
  sest.4yo <- round(spline(x = rval.km$time[rval.km$strata == "cpage=4-8 yo"], y = 1 - rval.km$sest[rval.km$strata == "cpage=4-8 yo"], xout = bins)$y * 100, digits = 0)
  sest.5yo <- round(spline(x = rval.km$time[rval.km$strata == "cpage=8+ yo"], y= 1 - rval.km$sest[rval.km$strata == "cpage=8+ yo"], xout = bins)$y * 100, digits = 0)
  
  sest.2yo <- ifelse(sest.2yo > 100, 100, sest.2yo)
  sest.3yo <- ifelse(sest.3yo > 100, 100, sest.3yo)
  sest.4yo <- ifelse(sest.4yo > 100, 100, sest.4yo)
  sest.5yo <- ifelse(sest.5yo > 100, 100, sest.5yo)
  
  # Cumulative proportion submitted for service, all ages:
  rval.km <- survfit(Surv(psmser, status) ~ 1, conf.type = "log", type = "kaplan-meier", data = rval)
  rval.km <- data.frame(time = rval.km$time, sest = rval.km$surv, slow = rval.km$lower, supp = rval.km$upper)
  sest.all <- round(spline(x = rval.km$time, y = 1 - rval.km$sest, xout = bins)$y * 100, digits = 0)
  
  rval <- data.frame(rbind(sest.2yo, sest.3yo, sest.4yo, sest.5yo, sest.all))
  names(rval) <- paste("WK ", 1:dim(rval)[2], sep = "")
  # row.names(rval) <- c("2 yo", "3 yo", "4-8 yo", "8+ yo", "TOTAL")
  row.names(rval) <- c("1 yo", "2 yo", "3 yo", "4+ yo", "TOTAL")
  rval
}
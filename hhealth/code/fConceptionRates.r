fConceptionRates <- function(crstart = input$crstart, crstop = input$crstop, tservs, tvexams, tcows){
  # crstart <- "01-06-1993"
  # crstop <- "01-11-1993"
  
  servs <- read.table(tservs, header = TRUE, sep = ",")
  servs <- servs[,1:6]
  names(servs) <- c("skey", "cowkey", "serdate", "sertype", "sersire","sertech")
  servs$etype <- "SERVE"
  servs$serdate <- as.Date(as.character(servs$serdate), format = "%d/%m/%y")
  
  vexams <- read.table(tvexams, header = TRUE, sep = ",")
  vexams <- vexams[,1:6]
  names(vexams) <- c("rkey", "cowkey", "repdate", "rreason", "rdx1", "rdx2")
  vexams$etype <- "REPRO"
  vexams$repdate <- as.Date(as.character(vexams$repdate), format = "%d/%m/%y")
  
  cows <- read.table(tcows, header = TRUE, sep = ",")
  cows <- cows[,c(1,2,4,7,8)]
  names(cows) <- c("cowkey", "herdid", "lid", "remdate", "birdate")
  cows$etype <- "STOCK"
  cows$remdate <- as.Date(as.character(cows$remdate), format = "%d/%m/%y")
  cows$birdate <- as.Date(as.character(cows$birdate), format = "%d/%m/%y")
  
  # Select all services for all cows: 
  tservs <- servs %>%
    select(skey, cowkey, etype, serdate) %>%
    filter(!is.na(serdate)) %>%
    arrange(cowkey, serdate)
  # dim(tservs) # 1341
  
  # Select all repro events for all cows. Drop those ttrepro exams with DDUNC (due date unchanged):
  tvexams <- vexams %>%
    select(rkey, cowkey, etype, repdate, rreason, rdx1, rdx2) %>%
    filter(!rdx2 == "DDUNC") %>%
    arrange(cowkey, repdate)
  
  # Make a list of unique cow keys:
  ucow <- unique(tservs$cowkey)
  
  # Record the cowkey and the service key of the successful service events:
  sskey <- c()
  
  # Setup a data frame to save details for unrecorded services:
  uservs <- data.frame(skey = 0, cowkey = 0, etype = "SERVE", serdate = as.Date("1/01/1900", format = "%d/%m/%Y"), conc = 0)
  
  for(i in 1:length(ucow)){
    # List all service events for the cow of interest:
    ttservs <- tservs %>%
      filter(cowkey == ucow[i])
    names(ttservs) <- c("ekey","cowkey","etype","edate")
    ttservs$rdx2 <- ttservs$rdx1 <- ttservs$rreason <- NA   
    
    # Create a list of candidate repro exams for this service event:
    ttvexams <- tvexams %>%
      filter(cowkey == ucow[i])
    names(ttvexams) <- c("ekey", "cowkey", "etype", "edate", "rreason", "rdx1", "rdx2")
    
    # Merge the service and repro exam tables and sort in order of edate:
    tmp <- rbind(ttservs, ttvexams)
    tmp$repserint <- NA
    
    tmp <- tmp %>%
      arrange(edate)
    
    # Work your way through each of the repro exams listed for the cow of interest. Only proceed if at least one reproductive exam is present:
    if(dim(ttvexams)[1] > 0){
      
      for(j in 1:nrow(ttvexams)){
        ttmp <- tmp %>%
          mutate(repserint = as.numeric(ttvexams$edate[j] - edate)) %>%
          filter(repserint >= 0)
        
        if(ttvexams$rdx2[j] == "LASTSERVICE" | ttvexams$rdx2[j] == "L SER"){
          ttmp <- ttmp %>%
            filter(etype == "SERVE")
          
          tsskey <- ttmp$ekey[nrow(ttmp)]
          sskey <- c(sskey,tsskey)
        }
        
        if(ttvexams$rdx2[j] == "2LASTSERVICE" | ttvexams$rdx2[j] == "SL SER"){
          ttmp <- ttmp %>%
            filter(etype == "SERVE")
          
          tsskey <- ttmp$ekey[nrow(ttmp) - 1]
          sskey <- c(sskey,tsskey)
        }
        
        if(ttvexams$rdx2[j] == "3LASTSERVICE" | ttvexams$rdx2[j] == "TL SER"){
          ttmp <- ttmp %>%
            filter(etype == "SERVE")
          
          tsskey <- ttmp$ekey[nrow(ttmp) - 2]
          sskey <- c(sskey,tsskey)
        }
        
        # If animal is pregnant and the number of weeks pregnant has been estimated:
        trdx2 <- grep(pattern = "WKS", x = ttvexams[j,"rdx2"], value = TRUE)
        trdx2 <- as.character(substr(trdx2, start = 1, stop = 6))
        
        if(length(trdx2) > 0){
          nwkspreg <- as.numeric(substr(trdx2, start = 1, stop = 2))
          
          # Find the repro-service interval within (nwkspreg * 7) - 7 to (nwkspreg * 7) + 7 and record the (successful) service key:
          id <- ttmp$repserint > ((nwkspreg * 7) - 7) & ttmp$repserint < ((nwkspreg * 7) + 7)
          
          # If the pregnancy can't be matched up with a service, we create one in the uservs (unmatched services table):
          if(sum(id) == 0){
            tuservs <- data.frame(skey = 0, cowkey = ttmp$cowkey[1], 
               etype = "SERVE", serdate = ttmp$edate[ttmp$etype == "REPRO" & ttmp$repserint == 0] - (nwkspreg * 7), conc = 1)
            uservs <- rbind(uservs, tuservs)
            
            tsskey <- 0
            sskey <- c(sskey, tsskey)  
          }
          
          # If the pregnancy can be matched up with a service, we add the identified successful service key to sskey:
          else if(sum(id) > 0){
            
            tsskey <- ttmp$ekey[id]
            sskey <- c(sskey, tsskey)        
          }
        }
      }
    }
  }
  
  uservs <- uservs[-1,]
  rval <- list(sskey = sskey, uservs = uservs)
  
  tservs$conc <- 0
  tservs$conc[tservs$skey %in% rval$sskey] <- 1
  
  # Cut the data down to the time frame of interest:
  tservs <- tservs %>%
    filter(serdate >= as.Date(crstart, format = "%d-%m-%Y") & serdate <= as.Date(crstop, format = "%d-%m-%Y"))
  
  # sum(tservs$conc) / nrow(tservs)
  
  # Cow age: 
  tservs$birdate <- cows$birdate[match(tservs$cowkey, cows$cowkey)]
  
  # Calculate age (in years) on date of service:
  tservs$sage <- round(as.numeric((tservs$serdate - tservs$birdate)/365), digits = 0)
  tservs$sage[tservs$sage > 20] <- NA
  
  # Create age categories:
  tservs$csage <- tservs$sage
  tservs$csage[tservs$sage >= 4 & tservs$sage <= 8] <- 4
  tservs$csage[tservs$sage >  8] <- 5
  # table(tservs$csage)
  tservs$csage <- factor(tservs$csage, levels = c(1,2,3,4,5), labels = c("1 yo", "2 yo", "3 yo", "4-8 yo", "8+ yo"))
  
  tservs$serv <- 1
  
  rval <- tservs %>%
    group_by(csage)
  rval <- data.frame(summarise(rval, conc = sum(conc), nserv = sum(serv)))[,2:3]
  rval[6,] <- c(sum(rval$conc), sum(rval$nserv))
  rval$crate <- round((rval$conc / rval$nserv) * 100, digits = 0)
  rownames(rval) <- c(levels(tservs$csage), "TOTAL")
  names(rval) <- c("Conceptions", "Services", "CR (%)")
  rval
}
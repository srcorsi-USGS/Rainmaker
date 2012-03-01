##########################################################################################
## Function RMevents

#Compute rainfall event variables based on time series of rain data with only one rain
#gage or one mean radar rain column.
#
# Usage: RMevents  (df,             # Data frame with rainfall
#                  ieHr=6,          # Interevent period in hours
#                  rainthresh=5.1   # minimum event depth in units of the rain column
#                                   # default is given as 5.1 assuming millimeters (0.2")
#                  rain="rain",     # column of rainfall unit values
#                  time="pdate"     # column with POSIX date
#                  ) 


###########

RMeventsSamples <- function(df,ieHr=6,rain="rain",time="pdate",dfsamples,bdate="bdate",edate="edate"){
  
  df <- df.orig 
  ieHr <- 6
  rain <- "rain"
  time <- "pdate"
  dfsamples <- events
  bdate <- "bdate"
  
  df <- subset(df,df[,rain]>0.0)
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  
  SearchWindow <- 21*24*3600 # 21 days converted to seconds
  dfsamples$BSdate <- dfsamples[,bdate] - SearchWindow
  
  for i in 1:nrow(dfsamples)){
    subdf <- subset(df,time >=dfsamples[i,"BSdate"] & time < dfsamples[i,edate])
    subdf$timediff <- difftime(subdf[1:(nrow(subdf)-1),time],subdf[2:(nrow(subdf)),time])
    #First look for the beginning of the event
    #Start looking at the beginning of the hydrograph and look backwards for the first
    #dry period of ieHr
    #use difftime and step back to the first value that exceeds ieSec
    bevent <- max(which(subdf[,time]<=dfsamples[i,bdate]))
    ie <- 0
    for (j in bevent:1) {
      if
    
    
    
    
    #Next, look for the end of the event.
    #Start at the end of the hydrograph and go backwards to the first rainfall
    
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  # Initiate variables
  StartRow <- 1
  EndRow <- 1
  StartDryRow <- 1
  dry <- TRUE
  stormnum <- 0
  continue.dry <- TRUE
  sumrain <- 0
  
  # Loop through rain data and define event periods
  for (i in 2:nrow(df)) {
    
    # During dry period, look for start of event
    if(dry) {
      
      # Event initiation
      if(df[i,rain]>0) {  
        dry=FALSE
        StartRow <- i
      }
    }
    # Define event period
    if(!dry) {
      
      # Search for end of event period
      if(df[i,rain]==0) {
        if(!continue.dry){
          continue.dry <- TRUE
          dryduration <- difftime(df[i,time],
                                  df[StartDryRow,time],
                                  units="secs")
        }
        
        # Continue checking for end of event (dry duration >= interevent period)
        if(continue.dry){                   
          dryduration <- difftime(df[i,time],df[StartDryRow,time],units="secs")
          if(dryduration >= ieSec) {
            EndRow <- StartDryRow
            stormnum <- stormnum + 1
            
            # After event period ends, save start and end dates/times and rain depth
            current.storm <- data.frame(stormnum=stormnum,
                                        StartDate=df[StartRow,time],
                                        EndDate=df[EndRow,time],
                                        rain=sumrain)
            dry <- TRUE
            if(stormnum>1) storms <- rbind(storms,current.storm)
            else storms <- current.storm        
            sumrain <- 0
            
          }
        }
      }
      # add current rain to event depth
      if (df[i,rain]!=0) {
        sumrain <- sumrain + df[i,rain]
        EndRow <- i
        StartDryRow <- EndRow
        continue.dry <- FALSE
      }
    }
  }
  
  
  return(storms)
}

##########################################################################################
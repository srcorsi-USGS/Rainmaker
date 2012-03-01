# Function to compute maximum x-minute rainfall intensities in units of depth/hr

# This funtion requires package TTR to be installed

# Input: unit value rain file
# Input: Storms file
# Output: X-hour maximum rainfall intensities

#Usage:  RMIntense(df,
#                  date,           Date column in df as POSIX
#                  rain,           Column in df with instantaneous rain values
#                  df.events,      Date frame with start and end dates/times for events
#                  sdate,          Start date column in df.events rain file as POSIX
#                  edate,          End date column in df.events rain file as POSIX
#                  xmin)           Vector of values representing X-minute max 
#                                  rainfall requested
                   

RMIntense <- function(df,date="r.date",rain = "rain",
                      df.events,sdate="sdate",edate="edate",
                      xmin=c(60,180,360)) {

  # Compute overall event intensity
  df.events$duration <- (1+as.numeric(difftime(df.events[,edate],df.events[,sdate],units="hours")))
  df.events$Ievent <- df.events$rain/df.events$duration

  # Determine x-minute intensities for each of the intensities specified  

  for (i in 1:length(xmin)){
  x <- xmin[i]*60
  intensity.var <- paste("I",xmin[i],sep="")
  df.events[,intensity.var] <- NA

  #   Isolate individual events and Compute max x-min intensity for each event 
  #   period: compute sum rain and divide by duration. Report x-min intensity 
  #   in units/hr

  for (j in 1:nrow(df.events)) {
    subdf <- subset(df,date >= df.events[j,sdate] & date <= df.events[j,edate])
    # Initialize intensity vector
    intensity <- numeric(length=nrow(subdf))
    
    for (k in 1:nrow(subdf)){
      enddate <- subdf[k,date]+x
      bdate <- subdf[k,date]

      subdf2 <- subset(subdf,date >= bdate & date < enddate)
      intensity[k] <- sum(subdf2[,rain])/(x/60/60)
      
#      k;bdate;enddate;intensity[k];max(subdf2$rain)
    }
    df.events[j,intensity.var] <- max(intensity,na.rm=TRUE)
  }
  }
   
  return(df.events)
}
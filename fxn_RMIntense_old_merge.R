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
  library(TTR)

#  Remove rows with duplicate times
  time.steps <- difftime(df[(2:nrow(df)),date],df[(1:(nrow(df)-1)),date])
  zap.me <- 1 + which(difftime(df[(2:nrow(df)),date],df[(1:(nrow(df)-1)),date])<=0)
  if(length(zap.me)>0) df <- df[-zap.me,]
  
# determine minimum time step and fill in missing time steps with assumed zero
# rainfall values
  time.steps <- difftime(df[(2:nrow(df)),date],df[(1:(nrow(df)-1)),date],units="secs")
  min.step <- min(time.steps)
  dates <- seq(min(df[,date]),max(df[,date]),min.step)
  df.dates <- data.frame(date.seq = dates)
  names(df.dates)=date

  df <- merge(df,df.dates,all=TRUE)
  df[,rain] <- ifelse(is.na(df[,rain]),0,df[,rain])
  
  # Initialize intensity vector
  intensity <- numeric(length=nrow(df.events))
  
  # Determine x-minute intensities for each of the intensities specified  

  df.events2 <- df.events
  for (i in 1:length(xmin)){
  x <- xmin[i]
  num.steps <- (x*60)/as.numeric(min.step)

  #   Isolate individual events and Compute max x-min intensity for each event 
  #   period: compute sum rain and divide by duration. Report x-min intensity 
  #   in units/hr

  for (j in 1:nrow(df.events)) {
    subdf <- subset(df,df[,date] >= df.events[j,sdate] & df[,date] <= df.events[j,edate])
    if(num.steps <= nrow(subdf)) {
      if(num.steps == 1) intensity[j] <- max(subdf[,rain],na.rm=T)/(x*60/3600)
      else intensity[j] <-max(runSum(subdf[,rain],num.steps),na.rm=T)/(x/60)
      } else
         intensity[j] <-sum(subdf[,rain],na.rm=T)/(x*60/3600)
  }
  ivar <- paste("I",x,sep="")
  df.events2 <- cbind(df.events2,intensity)
  names(df.events2)[ncol(df.events2)] <- ivar  
  }

  return(df.events2)
}
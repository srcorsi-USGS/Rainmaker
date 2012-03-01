###########################################################################################
###########################################################################################
##                                      RRainmaker                                       ##
##---------------------------------------------------------------------------------------##
##
## INSPIRING QUOTE INSERTED HERE
## Author: Steve corsi
## inspired by the original fortran version of Rainmaker 
##
## Functions included:
##
## 1. RMprep:        prepare data: transform to proper date format and change column names
## 2. RMThiessen:    FUNCTION NOT YET WRITTEN: Combine data from multiple rain gages using Thiessen polygon methods
## 3. RMevents:      Define events based on unit value rainfall and user defined parameters
## 4. RMevents.plot: produce one graph per event with specified window before and after
##                   each event for evaluating beginning and ending dates of events
## 5. RMmissing:     FUCTION NOT YET WRITTEN: Define periods of missing data and filter 
##                   event data accordingly.
## 6. RMIntense:     Compute x-minute maximum intensities
## 7. RMarf:         Compute x-day antecedent rainfall
##                                                                                       ##
###########################################################################################
###########################################################################################
## RMprep function
#
# This function is used to prepare data files for Rainmaker functions
#
# Dates are transformed to POSIX dates using the strptime function
# Multiple common date formats are included as options for tranformation
# The original date column is transformed to a character variable
#
# Column header names are changed to desired names

RMprep <- function(df,
                   prep.type=1,          #1=date to POSIX, 2=name change, 3=both
                   date.type=1,          #1=mm/dd/YYYY hh:mm, 
                                         #2=YYYY-mm-ddTHH:MM
                                         #3=RDB_example1: 2 columns, Date and Time
                                         # Date=m/d/Y; time=h:mm
                                         #4=RDB_example2: 4 columns, Year, Month, Day and Minute
                                         # Date=m/d/Y; time=h:mm    
                                         #5=Lake level from Great Lakes "Tides and Currents"
                                         # Date=YYYYMMDD; Time = H:MM
                   dates.in="default",   #Vector of column names for date/time definition
                                         # defaults are as follows for different date.type options
                                         # date.type=1: One column name -> "GMT.Time"
                                         # date.type=2: One column name _> "GMT.Time"
                                         # date.type=3: two column names -> c("DATE","TIME") 
                                         # date.type=4: four column names > c("YEAR","MONTH","DAY","MINUTE")
                                         # date.type=5: two column names -> c("Date","Time")
                                         # if no value is given, the defaults given above
                                         # are used. 
                                         #Enter value as c("name1","name2",...)
                   dates.out="date",
                   cnames.in="",
                   cnames.new="rain")
{
  
  date.options <- list (c("GMT.Time"),
                        c("GMT.Time"),
                        c("DATE","TIME"),
                        c("YEAR","MONTH","DAY","MINUTE"),
                        c("Date","Time"))
  
  if(dates.in=="default") dates.in <- date.options[[date.type]]
  for (i in 1:length(dates.in)) df[,dates.in[i]] <- as.character(df[,dates.in[i]])
  
  Date.style <- c("%m/%d/%Y %H:%M","%Y-%m-%d %H:%M",
                  "%m/%d/%Y %H:%M","%m/%d/%Y %H:%M",
                  "%Y%m%d %H:%M")
  
  # Convert to POSIX date/time
  if(prep.type==1 | prep.type==3){
    if(date.type==1) dates <- df[,dates.in]
    if(date.type==2) dates <- sub("T"," ",df[,dates.in])
    if(date.type==3 | date.type==5) dates <- paste(df[,dates.in[1]],df[,dates.in[2]])
    if(date.type==4) {
      hour <- trunc(date.options[4]/60)
      minute <- date.options[4]-hour*60
      dates <- paste(df[,dates.in[2]],"/",df[,dates.in[3]],"/",
                     df[,dates.in[1]]," ",
                     hour,":",minute)
    }
    pdate <-  strptime(dates,format=Date.style[date.type])
    df$pdate <- pdate
    names(df)[ncol(df)] <- dates.out
  }
  
  # Change column headers as specified
  if(prep.type==2 | prep.type==3){
    current.names <- names(df)
    name.locs <- which(current.names==cnames.in)
    names(df)[name.locs] <- cnames.new
  }
  return(df)
}



##########################################################################################


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

RMevents <- function(df,ieHr=6,rainthresh=5.1,rain="rain",time="pdate"){
  
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  
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
  
  # Subset based on defined event rain depth threshold        
  storms2 <- subset(storms,rain>=rainthresh,row.names=FALSE)
  
  return(list(storms2,storms))
}

##########################################################################################


##########################################################################################
## function RMevents.plot

# Function to graph rainfall for a given x-day window around specified event periods
#

# Usage:
#  RMevents.plot(df,             Data frame with unit value rainfall data
#                date,         Date column in df as POSIX
#                rain,           Column in df with instantaneous rain values
#                df.events,      Date frame with start and end dates/times for events
#                sdate,          Start date column in df.events rain file as POSIX
#                edate,          End date column in df.events rain file as POSIX
#                plot.buffer,    Used to define plotting window in days. Graphs will include
#                                data Time period preceding beginning of event for including
#                                in the graphs
#                site.name)

RMevents.plot <- function(df,date="pdate",rain = "rain",
                          df.events,sdate="StartDate",edate="EndDate",
                          plot.buffer=3,site.name="") {
  
  # Read file with event dates
  
  pdf("events.pdf")
  main.title <- paste(site.name,"Precipitation Event")
  for (i in 1:(nrow(df.events))) {
    p.sdate <- as.POSIXlt(df.events[i,sdate] - plot.buffer*24*3600/2)
    p.edate <- as.POSIXlt(df.events[i,edate] + plot.buffer*24*3600/2)
    subdf <- subset(df, date>=p.sdate & date<=p.edate)
    rmax <- max(subdf$rain + 0.3)
    subrain <- subdf[,rain]
    subdate <- as.POSIXct(subdf[,date])
    plot(subrain~subdate,
         #       data=subdf,
         type="h",
         xaxt="n",
         ylab="precipitation (mm)", 
         xlab="",
         col="blue",
         lwd=1,
         yaxs="i",
         ylim=c(0,rmax),
         xlim=as.POSIXct(range(subdf$date)),
         main = main.title)
    
    r <- as.POSIXct(trunc(range(subdf$date), "days"))
    r[2] <- r[2]+24*3600
    rhour <- seq(r[1], r[2], by=24*3600/4)
    rday <- seq(r[1], r[2], by="days")
    axis.POSIXct(1,subdf$date,at=rhour,format=" ",tcl=-0.2)
    axis.POSIXct(1,subdf$date,at=rday,format=" ",tcl=-0.5)
    axis.POSIXct(3,subdf$date,at=rhour,format=" ",tcl=0.2)
    axis.POSIXct(3,subdf$date,at=rday,format=" ",tcl=0.5)
    axis.POSIXct(1,subdf$date,format = "%m/%d/%y")
    arrows(df.events[i,sdate],(rmax-0.15),
           df.events[i,edate],(rmax-0.15),
           length=0.07,angle=20,col=colors()[84],
           code=3)
    #  abline(v=df.events[i,sdate])
    #  abline(v=df.events[i,edate])
    
    #  axis.POSIXct(3,subdf$date,format = "%m/%d/%y",
    #               at=c(df.events[i,sdate],df.events[i,edate]),
    #               tcl=2)
    mtext(paste("Event depth =",
                round(df.events[i,rain],2),"mm"),
          side=3,line=0.1,col=colors()[84])
  }
  
  dev.off()
  
  
}

##########################################################################################
## fucntion RMIntense
# Function to compute maximum x-minute rainfall intensities in units of depth/hr
#
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
                      df.events,sdate="StartDate",edate="EndDate",
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

##########################################################################################



##########################################################################################
## Function RMarf 
##
## This function computes antecedent rainfall for Rainmaker event files or any file with 
## a list of specified dates.
#
# Input files must have a POSIX formatted date/time column.
# This format can be achieved by using the RMprep function
# The name of the rainfall column can also be changed as desired using the RMprep funtion

# Subset the data by antecedent time period (can also assign to a df if you like)
# then define ARF values for the subset. Do this for all date periods
# in the sample file.

#Usage:  RMarf(df,           Unit values rain file
#              date,         Date column in df as POSIX
#              rain,         Column in df with instantaneous rain values
#              df.events,    Data frame with dates/times for events
#              sdate,        Name of start date column in df.events rain file as POSIX
#              days,         Vector of times in days for summing antecedent rainfall 
#              varnameout)   prefix for resulting antecedent rainfall variable names
# 
RMarf <- function(df, date="date", rain="rain",
                  df.events, sdate="StartDate", 
                  days=c(0.5,1,2,3,5,10,15,20),
                  varnameout="ARF") {
  
  arfdate <- "arfdate"
  
  #initialize varsum vector
  maxrows <- nrow(df.events)           #determine how many rows are in the dates data frame
  varsum=vector(length=maxrows)
  
  # compute the antecedent rain (ARF) for all identified durations
  for(j in 1:length(days)) {      
    df.events$arfdate <- df.events[,sdate] - days[j]*24*60*60
    
    # Compute ARF for all dates in the sample dates file
    for (i in 1:maxrows){
      subdata <- df[which(df[,date]>= df.events[i,arfdate]
                          & df[,date] < df.events[i,sdate]),]
      
      varsum[i] <- sum(subdata[,rain])
    }
    
    sumname <- paste(varnameout,days[j],sep="")
    
    df.events[,sumname] <- varsum
  }
  df.events <- df.events[,-which(names(df.events)==arfdate)]
  return(df.events)
}

##########################################################################################
##########################################################################################


##################################
## End of RRainmaker functions ###
##################################
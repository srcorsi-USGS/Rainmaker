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
                  "%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M",
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
    

    
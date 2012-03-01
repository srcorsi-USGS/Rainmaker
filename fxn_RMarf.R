
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
                   df.events, sdate="sdate", 
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
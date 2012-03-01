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
                          df.events,sdate="sdate",edate="edate",
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
  
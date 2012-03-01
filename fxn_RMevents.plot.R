# Function to graph rainfall for a given x-day window around specified event periods
#

# Usage:
#  RMevents.plot(df,             Data frame with unit value rainfall data
#                r.date,         Date column in df as POSIX
#                rain,           Column in df with instantaneous rain values
#                df.events,      Date frame with start and end dates/times for events
#                sdate,          Start date column in df.events rain file as POSIX
#                edate,          End date column in df.events rain file as POSIX
#                plot.buffer,    Used to define plotting window in days. Graphs will include
#                                data Time period preceding beginning of event for including
#                                in the graphs
#                site.name)

RMevents.plot <- function(df,r.date="pdate",rain = "rain",
                          df.events,sdate="sdate",edate="edate",
                          plot.buffer=3,site.name="") {


plot.buffer <- 3
site.name <- "Underwood Creek"

setwd(paste(Rlocal,"/virus1",sep=""))
#READ IN FILE WHILE BUILDING FUNCTION
#Read in radar rainfall file


# Read in unit values rain file
df <- read.delim("UnderwoodRRain.txt")
names(df)[2] <- "rain"
names(df)[8] <- "r.date"
names(df)[4] <- "maxrain"
sdate <- "StartDate"
edate <- "EndDate"
r.date <- "pdate"

rain <- "rain"
time <- "CST.Time"
rainthresh <- 0.2*25.4
GMToffset = 6*3600 # GMT offset for CST

ieHr <- 6 # define interevent period in hours
ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX

df$pdate <- strptime(df[,time],"%m/%d/%Y %H:%M",tz="GMT")  - GMToffset
mingap <- min(difftime(df$pdate[2:1000],df$pdate[1:999],units="secs"),na.rm=T)
mintime <- min(df$pdate,na.rm=T)
maxtime <- max(df$pdate,na.rm=T)

# Read file with event dates

df.events <- events[[1]]

pdf("events.pdf")
main.title <- paste(site.name,"Precipitation Event")
for (i in 1:(nrow(df.events))) {
  p.sdate <- df.events[i,sdate] - plot.buffer*24*3600/2
  p.edate <- df.events[i,edate] + plot.buffer*24*3600/2
  subdf <- subset(df, r.date>=p.sdate & r.date<=p.edate)
  rmax <- max(subdf$rain + 0.3)
  plot(rain~r.date,
       data=subdf,
       type="h",
       xaxt="n",
       ylab="precipitation (mm)", 
       xlab="",
       col="blue",
       lwd=1,
       yaxs="i",
       ylim=c(0,rmax),
       xlim=range(subdf$r.date),
       main = main.title)
  
  r <- as.POSIXct(trunc(range(subdf$r.date), "days"))
  r[2] <- r[2]+24*3600
  rhour <- seq(r[1], r[2], by=24*3600/4)
  rday <- seq(r[1], r[2], by="days")
  axis.POSIXct(1,subdf$r.date,at=rhour,format=" ",tcl=-0.2)
  axis.POSIXct(1,subdf$r.date,at=rday,format=" ",tcl=-0.5)
  axis.POSIXct(3,subdf$r.date,at=rhour,format=" ",tcl=0.2)
  axis.POSIXct(3,subdf$r.date,at=rday,format=" ",tcl=0.5)
  axis.POSIXct(1,subdf$r.date,format = "%m/%d/%y")
  arrows(df.events[i,sdate],(rmax-0.15),
         df.events[i,edate],(rmax-0.15),
         length=0.07,angle=20,col=colors()[84],
         code=3)
#  abline(v=df.events[i,sdate])
#  abline(v=df.events[i,edate])
  
#  axis.POSIXct(3,subdf$r.date,format = "%m/%d/%y",
#               at=c(df.events[i,sdate],df.events[i,edate]),
#               tcl=2)
  mtext(paste("Event depth =",
              round(df.events[i,rain],2),"mm"),
        side=3,line=0.1,col=colors()[84])
}

     dev.off()
  
  
  
  
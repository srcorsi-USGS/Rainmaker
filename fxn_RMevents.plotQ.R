# Function to graph rainfall and flow for a given x-day window around specified event periods
#

# Usage:
#  RMevents.plot(df,             Data frame with unit value rainfall data
#                dfQ,            Data frame with unit value Q data
#                date,           Date column in df as POSIX
#                Qdate,          Date column in dfQ as POSIX
#                rain,           Column in df with instantaneous rain values
#                Q,              Column in dfQ with instantaneous Q values
#                df.events,      Date frame with start and end dates/times for events
#                sdate,          Start date column in df.events rain file as POSIX
#                edate,          End date column in df.events rain file as POSIX
#                plot.buffer,    Used to define plotting window in days. Graphs will include
#                                data Time period preceding beginning of event for including
#                                in the graphs
#                logy,           "y" if log y-axis for Q or "" if linear axis. Will default to "n"
#                                if not specific or if minimum Q <= 0.
#                site.name)

RMevents.plotQ <- function(df,dfQ,date="pdate",Qdate="pdate",rain = "rain",Q="Q",
                          df.events,sdate="StartDate",edate="EndDate",
                          plot.buffer=3,logy="",site.name="") {

# Trouble shooting variable definitions  
# df <- dfRain
# dfQ <- dfQ
# date <- "pdate"
# Qdate <- "pdate"
# rain <- "rain"
# Q <- "VALUE"
# df.events <- events
# sdate<-"StartDate"
# edate<-"EndDate"
# plot.buffer<-3
# logy <- "y"
# site.name<-"MF"
  
# Read file with event dates

pdf(paste(site.name,"_events.pdf",sep=""))

  # Define plot layout: panel 1 for Q and panel 2 for FIB
  mylayout <- matrix(c(1,
                       1,
                       2,
                       2,
                       2),5,1,byrow=TRUE)
  layout(mylayout)
  
main.title <- paste(site.name,"Precipitation and Q Event")
for (i in 1:(nrow(df.events))) {
  ########################## Graph Precip  ###########################################
  p.sdate <- as.POSIXlt(df.events[i,sdate] - plot.buffer*24*3600/2,tz="")
  p.edate <- as.POSIXlt(df.events[i,edate] + plot.buffer*24*3600/2,tz="")
  subdf <- subset(df, df[,date]>=p.sdate & df[,date]<=p.edate)
  rmax <- max(subdf[,rain] + 0.3)
  subrain <- subdf[,rain]
  subdate <- as.POSIXct(subdf[,date])
  #Set Margins for first plot
  par(mar= c(0, 4, 4, 2) + 0.1)
  plot(subrain~subdate,
#       data=subdf,
       type="h",
       xaxt="n",
       ylab="precipitation (mm)", 
       xlab="",
       col="blue",
       lwd=1,
       yaxs="i",
       ylim=c(rmax,0),
       xlim=as.POSIXct(range(subdf[,date])),
       main = "")
  mtext(main.title,side=3,line=2,cex=1.5)

  mtext(paste("Event depth =",
              round(df.events[i,rain],2),"mm"),
        side=3,line=0.5,col=colors()[84])
  arrows(df.events[i,sdate],(rmax-0.15),
         df.events[i,edate],(rmax-0.15),
         length=0.07,angle=20,col=colors()[84],
         code=3) 
  
  
########################## Graph Q  ################################################
  subdfQ <- subset(dfQ, dfQ[,Qdate]>=p.sdate & dfQ[,Qdate]<=p.edate)
  Qmax <- max(subdfQ[,Q] *1.05)
  if(Qmax < 0) Qmax <- Qmax*0.95
  if(Qmax > 0) Qmax <- Qmax*1.05 
  Qmin <- min(c(subdfQ[,Q]))
  if(Qmin <= 0) Qmin <- Qmin*1.05; logy <- ""
  if(Qmin > 0) Qmin <- Qmin*0.95; logy <- logy
              
  subQ <- subdfQ[,Q]
  subdateQ <- as.POSIXct(subdfQ[,date])

  #Set Margins for second plot
  par(mar= c(5, 4, 0, 2) + 0.1)
  plot(subQ~subdateQ,
       #       data=subdf,
       type="l",
       xaxt="n",
       ylab="Flow (cfs)", 
       xlab="",
       col="red",
       lwd=1,
       yaxs="i",
       log=logy,
       ylim=c(Qmin,Qmax),
       xlim=as.POSIXct(range(subdf[,date])),
       main = "")
    
  r <- as.POSIXct(trunc(range(subdf[,date]), "days"))
  r[2] <- r[2]+24*3600
  rhour <- seq(r[1], r[2], by=24*3600/4)
  rday <- seq(r[1], r[2], by="days")
  axis.POSIXct(1,subdf[,date],at=rhour,format=" ",tcl=-0.2)
  axis.POSIXct(1,subdf[,date],at=rday,format=" ",tcl=-0.5)
  axis.POSIXct(3,subdf[,date],at=rhour,format=" ",tcl=0.2)
  axis.POSIXct(3,subdf[,date],at=rday,format=" ",tcl=0.5)
  axis.POSIXct(1,subdf[,date],format = "%m/%d/%y")

#  abline(v=df.events[i,sdate])
#  abline(v=df.events[i,edate])
  
#  axis.POSIXct(3,subdf$date,format = "%m/%d/%y",
#               at=c(df.events[i,sdate],df.events[i,edate]),
#               tcl=2)


}
par(mar=c(5,2,0,1))
     dev.off()
  
 
}
  
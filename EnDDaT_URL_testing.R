enddat_sdate <- "2001-06-01"
enddat_edate <- "2001-06-03"
enddat_url <- paste("http://igsarm-cida-javadev1.er.usgs.gov:8080/glos_portal/ProcessData.jsp?BeachName=No+beach+chosen&BeachLat=43.0799&BeachLon=-89.404&Lake=null&shapefile=upload%3Adane_co&shapefileFeature=AREA&precipOrigin=&maxTime0=&maxTime1=&maxTime2=&beginPosition=",
      enddat_sdate,"&endPosition=",enddat_edate,"2001-12-31",
      "&DateFormat=Excel&TZ=0_GMT&fill=&style=tab&timeInt=6&filterId=&GDP=precip3_MEAN_2000-06-15_2011-10-18",
      sep="")


enddat_url <- paste("http://igsarm-cida-javadev1.er.usgs.gov:8080/glos_portal/Data.jsp?style=tab&DateFormat=Excel&fill=&beginPosition=",
                    enddat_sdate,"&endPosition=",enddat_edate,
                    "&Lake=null&TZ=0_GMT&BeachName=No+beach+chosen&BeachLat=43.0799&BeachLon=-89.404&shapefile=upload%3AClaybanks&shapefileFeature=Id&precipOrigin=&maxTime0=&maxTime1=&maxTime2=&filterId=&timeInt=6&GDP=precip3&gdpId=ancrfcqpe_w_meta.ncml%3AMEAN%3A1328144748176OUTPUT.d832888a-0b97-4df2-947b-5dabf188609e",
                    sep="")


dane.url.test <- read.delim(file=enddat_url)



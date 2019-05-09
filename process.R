library(data.table)
library(fasttime)
tables<-function(file){
	colnames<-c("SegID","ICAO_orig","ICAO_dest","AC_type",
				"time1","time2","FL1","FL2",
				"Callsign","Date_st","Date_end",
				"Lat_st","Lon_st","Lat_end","Lon_end","FlID","seq","Dist")
	#colClasses<-list(factor=c("Time1","Time2","Date1","Date2"))
	colClasses<-list(character=c(5,6,11,12))
	message("Reading File")
	ddr2<-fread(file,drop=c(9,20),stringsAsFactors=TRUE,col.names=colnames,colClasses=colClasses)
	ddr2<-ddr2[!ICAO_orig %in% c("ZZZZ","AFIL") & !ICAO_dest %in% c("ZZZZ","AFIL")]

	message("Splitting IDs")
	ddr2[, c("PtID_st", "PtID_end") := tstrsplit(SegID, "_", fixed = TRUE)]
	message("Converting to dates")
	ddr2[, Date_st := fastPOSIXct(paste(substr(Date_st,1,2),"-",substr(Date_st,3,4),"-",substr(Date_st,5,6),"T", substr(time1,1,2),":",substr(time1,3,4),":",substr(time1,5,6)))]
	ddr2[, Date_end := fastPOSIXct(paste(substr(Date_end,1,2),"-",substr(Date_end,3,4),"-",substr(Date_end,5,6),"T", substr(time2,1,2),":",substr(time2,3,4),":",substr(time2,5,6)))]
	ddr2[,c("time1","time2"):=NULL]
	message("Cummulative distance")
	ddr2[,Dist_cum:=cumsum(Dist),by=FlID]
	
	
	# Flights Data (FlID, Callsign, ICAO_orig, ICAO_dest, Total_dist)
	message("Processing Flights Data")
	flights<-ddr2[, .(FlID,Callsign,ICAO_orig,ICAO_dest,Dist_cum)]
	ddr2[, c("ICAO_orig","ICAO_dest"):=NULL]
	flights<-flights[,.SD[.N],by=FlID]
	
	# Airplane Data (Callsign Date_min Date_max AC_type)
	message("Processing Airplane Data")
	planes<-ddr2[,.(Callsign,Date_st,Date_end,AC_type)]
	planes[,`:=` (minDate=min(Date_st),maxDate=max(Date_end)),by=c("Callsign","AC_type")]
	planes[, c("Date_st","Date_end"):=NULL]
	planes<-unique(planes)
	
	ddr2[,c("AC_type","Callsign"):=NULL]
	
	
	# Trajectory Data
	message("Processing Trajectory Data")
	trajstart<-ddr2[seq==1,.(FlID,PtID_st,Date_st,Lat_st,Lon_st)]
	trajrest<-ddr2[,.(FlID,PtID_end,Date_end,Lat_end,Lon_end,seq,Dist,Dist_cum)]
	trajstart[,c("seq","Dist","Dist_cum"):=list(0,0,0)]
	TrajColnames<-c("FlID","PtID","Date","Lat","Lon","seq","Dist_to_prev","Dist_cum")
	names(trajstart)<-TrajColnames
	names(trajrest)<-TrajColnames
	trajectory<-rbindlist(list(trajstart,trajrest))
	setorder(trajectory,"FlID","seq")
	
	return(list(trajectory,flights,planes,points))
}

system.time(
tb<-tables("data/20160222_20160226_0000_2359_____m3.so6")
)
format(object.size(tb),units="MiB")

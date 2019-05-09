library(data.table)
library(fasttime)
tables<-function(file){
	colClasses<-list(factor=c("PtID_st","PtID_end","ICAO_orig","ICAO_dest","AC_type","Callsign"))
	message("Reading File")
	ddr2<-fread(file,stringsAsFactors=FALSE,colClasses=colClasses)
	
	if(class(ddr2$Date_st)[1]!="POSIXct"){
		ddr2[,c("Date_st","Date_end") := list(fastPOSIXct(Date_st),fastPOSIXct(Date_end))]
	}
	ddr2[,Dist_cum:=cumsum(Length),by=FlID]
	
	
	# Flights Data (FlID, Callsign, ICAO_orig, ICAO_dest, Total_dist)
	message("Processing Flights Data")
	flights<-ddr2[, .(FlID,Callsign,ICAO_orig,ICAO_dest,Dist_cum)]
	ddr2[, c("ICAO_orig","ICAO_dest"):=NULL]
	flights<-flights[,.SD[.N],by=FlID]


	# Points Data (PtID Date_min Date_max Lon Lat)
	message("Processing Points Data")
	ptsstart<-ddr2[seq==1,.(PtID_st,Date_st,Lat_st,Lon_st)]
	ptsrest<-ddr2[,.(PtID_end,Date_end,Lat_end,Lon_end)]
	PtcolNames<-c("PtID","Date","Lat","Lon")
	names(ptsstart)<-PtcolNames
	names(ptsrest)<-PtcolNames
	points=rbindlist(list(ptsstart,ptsrest))
	setorder(points,"PtID","Date")
	points[, `:=` (minDate=min(Date),maxDate=max(Date)),by=c("PtID","Lat","Lon")]
	points[,Date:=NULL]
	points<-unique(points)
	
	ddr2[,c("Lat_st","Lon_st","Lat_end","Lon_end"):=NULL]

	# Airplane Data (Callsign Date_min Date_max AC_type)
	message("Processing Airplane Data")
	planes<-ddr2[,.(Callsign,Date_st,Date_end,AC_type)]
	planes[,`:=` (minDate=min(Date_st),maxDate=max(Date_end)),by=c("Callsign","AC_type")]
	planes[, c("Date_st","Date_end"):=NULL]
	planes<-unique(planes)
	
	ddr2[,c("AC_type","Callsign"):=NULL]
	
	
	# Trajectory Data
	message("Processing Trajectory Data")
	trajstart<-ddr2[seq==1,.(FlID,PtID_st,Date_st)]
	trajrest<-ddr2[,.(FlID,PtID_end,Date_end,seq,Length,Dist_cum)];
	trajstart[,c("seq","Length","Dist_cum"):=list(0,0,0)]
	TrajColnames<-c("FlID","PtID","Date","seq","Dist_to_prev","Dist_cum")
	names(trajstart)<-TrajColnames
	names(trajrest)<-TrajColnames
	trajectory<-rbindlist(list(trajstart,trajrest))
	setorder(trajectory,"FlID","seq")
	
	
	return(list(trajectory,flights,planes,points))
}
system.time(
tb<-tables("data/20160222_20160226_0000_2359_____m3.so6.pre")
)
format(object.size(tb),units="MiB")

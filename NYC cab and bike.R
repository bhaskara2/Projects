library(ggplot2)
library(ggmap)
library(dplyr)
library(geosphere)
library(dplyr)
library(base)

#Read Files
#cbk = read.table('C:/Users/Bhaskara/Downloads/Summer/Work/NYC Bike and cab/201605-citibike-tripdata.csv', header = T,sep=',')
#nyc_dt = read.table('C:/Users/Bhaskara/Downloads/Summer/Work/NYC Bike and cab/yellow_tripdata_2016-05.csv', header = T,sep=',')
#cbk_sub=read.table('C:/Users/Bhaskara/Downloads/Summer/Work/NYC Bike and cab/Bike Data Subset.csv', header = T,sep=',')

summary(nyc)
summary(cbk)

#Test Missing Values
str(cbk)
str(nyc)
#********************************************************************************************************
#Try to test busy hours b/w 7 to 11                                                                     *
#Hours = format(as.POSIXct(strptime(nyc$tpep_pickup_datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")*
#********************************************************************************************************

#Find the distance between the bike station and near pickups.(Average distance between 2 Avenues in Manhattan is 0.152KMS)
mylist = distHaversine(cbind(nyc$pickup_longitude, nyc$pickup_latitude), cbind(cbk$start.station.longitude,cbk$start.station.latitude))
nyc_bkdist=cbind(nyc,mylist)
mylist
#Taxiride pickups within 500ft of a bike station.
ri_ne_bk <- nyc_bkdist %>% filter(mylist<=228.8)
nrow(ri_ne_bk)
summary(ri_ne_bk)

#visualizations
hist(ri_ne_bk$total_amount,breaks=100,xlim = c(0,100),col="violet", main = "taxi fare")
hist(ri_ne_bk$total_amount,breaks=100,xlim = c(0,100),col="violet", main = "taxi fare")
hist(ri_ne_bk$trip_distance,breaks=200,xlim = c(0,10),col="violet", main = "taxi fare")
a=hist(cbk$start.station.id,breaks=273,xlim = c(0,3000),col="violet", main = "taxi fare")
a
a$counts
a$breaks
dim(a)
#Visualizations on NewYork Map
library(ggmap)
library(Rcpp)
isnear=('');isnear

#___________________________________________________________________________________________
names(nyc_bkdist)
for (x in 1:1048575)
{
  if(nyc_bkdist[x,'mylist']<=228)
  {
    isnear[x]=1
  }else isnear[x]=0
}

flg_data=cbind(isnear,nyc_bkdist)
str(flg_data)


#To see if all the rides are near bike stand 285
plot(ri_ne_bk$pickup_longitude,ri_ne_bk$pickup_latitude,xlab="lon",ylab="lat")

names(cbk_sub)
#_____________________________________________________________________________

library(ggmap)
mh_map = get_googlemap(center = c(-73.978, 40.74486), zoom = 15, style = "feature:poi|visibility:off")
#mht_map
ggmap(mh_map, extent = "device") +
  geom_point(data=ri_ne_bk,
             aes(x = pickup_longitude, y=pickup_latitude),
             alpha = 0.0275,
             size = 2,
             color = "#cc0000") +
  geom_point(data=cbk_sub,aes(x=start.station.longitude,y=start.station.latitude),size=3,color='blue',alpha=1)
  title_with_subtitle("Murray Hill Bridge and Tunnel", "Drop offs for Saturday evening taxi rides originating at Penn Station") +
  theme_tws_map(base_size = 20)
add_credits()
dev.off()


#______________________________________________________
#Test DBSC
library(dplyr)
nyc_samp=sample_n(nyc_dt,100000)
nrow(nyc_samp)

#_

#install.packages("fpc")
#install.packages("dbscan")

nyc_lon=nyc_samp$pickup_longitude
nyc_lat=nyc_samp$pickup_latitude
nyc_lon_lat=data.frame(nyc_lon,nyc_lat)

library(dbscan)
library(fpc)

dbsc_clus=dbscan(nyc_lon_lat, eps=0.0030, MinPts = 1, borderPoints = T, search = "kdtree")
dbsc_clus


nyc$
plot(lat ~ lng, data = data, col = cluster.dbscan$cluster + 1L, pch = 20)



#VISUALIZATIONS
##________________________________________________________________________________________________________
#nyc_fin1 = read.table('C:/Users/Bhaskara/Downloads/Summer/Work/NYC Bike and cab/final_data_nyc.csv', header = T,sep=',')
#cbk_fin = read.table('C:/Users/Bhaskara/Downloads/Summer/Work/NYC Bike and cab/201605-citibike-tripdata.csv', header = T,sep=',')

str(nyc_fin1)
nyc_fin1$isnear_bk_st=nyc_fin1$start_station_id
nyc_fin1$isnear_bk_st=as.factor(nyc_fin1$isnear_bk_st)
nyc_fin1[["isnear_bk_st"]][is.na(nyc_fin1[["isnear_bk_st"]])] <- 0
nyc_fin1[["isnear_bk_st"]][!is.na(nyc_fin1[["isnear_bk_st"]])] <- 1


#stra_fin1 <- nyc_fin1 %>% filter(isnear_bk_st==1)
#stra_fin1$isnear_bk_st
#nrow(stra_fin1)
#stra_fin2 <- nyc_fin1 %>% filter(isnear_bk_st==0)
#cbk_fin_samp=sample_n(stra_fin2,2218)
#
stra_fin_samp=rbind(stra_fin1,cbk_fin_samp)
nrow(stra_fin_samp)
#_____________
nyc_fin1$pickup_datetime <- mdy_hm(nyc_fin1$tpep_pickup_datetime)
nyc_fin1$dropoff_datetime=mdy_hm(nyc_fin1$tpep_dropoff_datetime)

plt_data1 <- nyc_fin1 %>%
  mutate(hourofday = hour(pickup_datetime)) %>%
  group_by(hourofday) %>%
  count() 

cbk_fin_samp=sample_n(cbk_fin,10059)
nrow(nyc_fin1)

cbk_fin_samp$starttime <- mdy_hm(cbk_fin_samp$starttime)
plt_data2 <- cbk_fin_samp %>%
  mutate(hourofday = hour(starttime)) %>%
  group_by(hourofday) %>%
  count()

#_________________________________________________________ 
  ggplot(plt_data1, aes(hourofday, n)) +
  geom_point(aes(colour="cab"),size=4) +
  geom_line(aes(colour='cab')) +
  geom_point(data = plt_data2, aes(y = n, colour = 'bike'), size = 4) +
  geom_line(data = plt_data2,aes(colour='bike')) +
    ggtitle("Hourly comparison of cab rides and bike rides") + 
    theme(plot.title = element_text(hjust = 0.5))+
scale_colour_manual("", 
                        values = c("cab"="blue", "bike"="red"))
    
    
#________________________________________________END of 1st Visual.______________________
#day of week.


plt_cab_wkdat <- nyc_fin1 %>%
    mutate(dow = weekdays(pickup_datetime)) %>%
    group_by(dow) %>%
    count()

plt_bk_wkdat <- cbk_fin_samp %>%
  mutate(dow = weekdays(starttime)) %>%
  group_by(dow) %>%
  count()

plt_bk_wkdat;plt_cab_wkdat

ggplot(plt_cab_wkdat, aes(dow, n,group=1)) +
  geom_point(aes(colour="cab"),size=4) +
  geom_line(data = plt_cab_wkdat,aes(colour="cab")) +
  geom_point(data = plt_bk_wkdat, aes(y = n, colour = "bike"), size = 4) +
  geom_line(data = plt_bk_wkdat,aes(colour="bike")) +
  ggtitle("Weekly comparison of cab rides and bike rides") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("", 
                      values = c("cab"="blue", "bike"="red"))
#_____________________________________END of 2nd visual________________________________________
#Money
 
nrow(stra_fin_samp)

library(gridExtra)
univar_graph <- function(univar_name, univar, data, output_var,tit) {
  g_1 <- ggplot(data, aes(x=univar)) + geom_density() +scale_x_log10()+ xlab(univar_name)+ggtitle(tit) +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_colour_manual("", values = c("No Bike St"="0", " Bike St"="1"))
  g_2 <- ggplot(data, aes(x=univar, fill=output_var)) + geom_density(alpha=0.4) + scale_x_log10()+xlab(univar_name) + 
    ggtitle(tit)+theme(plot.title = element_text(hjust = 0.5))+
    scale_colour_manual("", values = c("No Bike St"="0", " Bike St"="1"))
  
  #grid.arrange(g_1, g_2, ncol=2, top=paste(univar_name,"variable", "/ [ Skew:",skewness(univar),"]"))
}

a=univar_graph(names(stra_fin_samp)[26], stra_fin_samp[,'total_amount'], stra_fin_samp, stra_fin_samp[,'isnear_bk_st'],"Money spent on Commutes")
a

#______________________________  End of 3rd visual______________________________________________________
#calculation of velocities and trip durations.

stra_fin_samp$trip_duration=((stra_fin_samp$dropoff_datetime)-(stra_fin_samp$pickup_datetime))/60
str(stra_fin_samp$trip_duration)

stra_fin_samp$trip_duration=as.numeric(stra_fin_samp$trip_duration)
stra_fin_samp$trip_duration=(stra_fin_samp$trip_duration)/60
str(stra_fin_samp$trip_duration)
stra_fin_samp$velocity=(stra_fin_samp$trip_distance)/(stra_fin_samp$trip_duration)

#str(nyc_fin1$isnear_bk_st)
#______________________________________________________________________________________________
#velocity
hist(stra_fin_samp$velocity,breaks=2000,xlim = c(0,40),col="violet", main = "velocity in km/hr")
b=univar_graph(names(stra_fin_samp)[28], stra_fin_samp[,'velocity'], stra_fin_samp, stra_fin_samp[,'isnear_bk_st'],"Speed comparison of bikes and cabs ")
b

names(stra_fin_samp)
#____________________________End of 4th visual__________________________________________________
#trip duration in mins
c=univar_graph(names(stra_fin_samp)[27], stra_fin_samp[,'trip_duration'], stra_fin_samp, stra_fin_samp[,'isnear_bk_st'],"Trip duration of bikes and cabs")
c

#____________________________End of 5th visual__________________________________________________
#trip distance in KMS
stra_fin_samp %>%
  ggplot(aes(trip_distance, fill = isnear_bk_st)) +
  geom_histogram(bins = 50) +
  scale_x_log10()+
  ggtitle("Distance covered by cab and bike rides") + 
  theme(plot.title = element_text(hjust = 0.5))

#____________________________End of 6th visual____________________________________________________
#try to get most most_famous_stations in the stra_fin_samp and plot density of cab rides near them.
#sort(table(stra_fin_samp$start_station_id))
tail(names(sort(table(stra_fin_samp$start_station_id))), 5)

library(ggmap)
mh_map = get_googlemap(center = c(-73.978, 40.74486), zoom = 13, style = "feature:poi|visibility:off")
mh = filter(nyc_fin1, start_station_id %in% c(3249, 3238, 3179, 3221, 3237))
nrow(mh)
#mht_map
ggmap(mh_map, extent = "device")+
  geom_contour(data=mh,
             aes(x = pickup_longitude, y=pickup_latitude),
             alpha = 0.0275,
             size = 2,
             color = "#cc0000")+
  ggtitle("Cab rides near BIKE stations") + 
  theme(plot.title = element_text(hjust = 0.5))


#library(ggmap)
mh_map = get_googlemap(center = c(-73.978, 40.74486), zoom = 13, style = "feature:poi|visibility:off")
mhnot = filter(nyc_fin1, !start_station_id %in% c(3249, 3238, 3179, 3221, 3237))
nrow(mhnot)

hist(mh$trip_distance,breaks=100,xlim = c(0,20),col="violet", main = "taxi fare")
#mht_map
ggmap(mh_map, extent = "device")+
  geom_point(data=mhnot,
             aes(x = pickup_longitude, y=pickup_latitude),
             alpha = 0.0275,
             size = 2,
             color = "#cc0000")+
  ggtitle("Cab rides without any nearby BIKE stations") + 
  theme(plot.title = element_text(hjust = 0.5))
#_______________________End of 6th visual_________________________________________________________________
e=univar_graph(names(stra_fin_samp)[7], stra_fin_samp[,'passenger_count'], stra_fin_samp, stra_fin_samp[,'isnear_bk_st'],"Number of passengers ")
e
#___________
#pollution
library(geosphere)
trp_dist = distHaversine(cbind(cbk_fin$start.station.latitude, cbk_fin$start.station.longitude), cbind(cbk_fin$end.station.latitude,cbk_fin$end.station.longitude))

mean(cbk_fin$trp_dist)

#Find correlaton plot

library(tidyr)
dim(stra_fin_samp1)
stra_fin_samp1=na.omit(stra_fin_samp)

stra_fin_samp1[!is.finite(stra_fin_samp1)] <- 0
nrow(stra_fin_samp1)
stra_fin_samp1$isnear_bk_st=as.factor(stra_fin_samp1$isnear_bk_st)

summary(model.lm)
##############Libraries that are going to be used#################
library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(shapefiles)
library(geosphere)
library(plyr)
library(sp)
library(rgdal)
library(data.table)
library(ggmap)
library(plotly)
library(tidyr)
############Data processing################
setwd("...")
dat<-read.csv("airport information.csv",header=T)
location<-read.csv("location.csv",header=T)
id<-read.csv("airport id.csv",header=T)
lineinworld<-(dat$ORIGIN_AIRPORT_ID%in%location$AIRPORT_ID)&(dat$DEST_AIRPORT_ID%in%location$AIRPORT_ID)#Check if the airport in our dataset is in location database.
worldline<-dat[lineinworld,c("ORIGIN_AIRPORT_ID","DEST_AIRPORT_ID")]#Only choose airport id.
flights.ag<-ddply(worldline, c("ORIGIN_AIRPORT_ID","DEST_AIRPORT_ID"), function(x) count(x$DEST_AIRPORT_ID))
flights.ll<-merge(flights.ag,location, by.x="DEST_AIRPORT_ID", by.y="AIRPORT_ID", all.x=T)
flights.ll<-flights.ll[order(flights.ll$ORIGIN_AIRPORT_ID,flights.ll$DEST_AIRPORT_ID),]
flights.lf<-merge(flights.ag,location, by.x="ORIGIN_AIRPORT_ID", by.y="AIRPORT_ID", all.x=T)
flights.lf<-flights.lf[order(flights.lf$ORIGIN_AIRPORT_ID,flights.lf$DEST_AIRPORT_ID),]
rts<-gcIntermediate(flights.lf[,c('LONGITUDE','LATITUDE')],flights.ll[,c('LONGITUDE','LATITUDE')],100,breakAtDateLine=FALSE,addStartEnd=TRUE,sp=TRUE)
rts<-as(rts,"SpatialLinesDataFrame")
rts.ff<-fortify(rts)
flights.ll$id<-as.character(c(1:nrow(flights.ll)))
gcircles<-merge(rts.ff,flights.ll,all.x=T,by="id")
center<-260 #260 is for USA.
gcircles$long.recenter<-ifelse(gcircles$long<center-180,gcircles$long+360,gcircles$long)
RegroupElements<-function(longcol,idcol){  
  g<-rep(1, length(longcol))
  if(diff(range(longcol))>300){          
    d<-longcol>mean(range(longcol))
    g[!d]<-1     
    g[d]<-2  
  }
  g<-paste(idcol, g, sep=".")
  return(g)
}
gcircle<-data.table(gcircles)
setkey(gcircle,id)
gcircle[,group.regroup:=RegroupElements(long.recenter,id),by="id"]
gcircles.rg<-as.data.frame(gcircle)
setwd("C://Users//Jingqi//Desktop//503//project//map")
urbanareasin<-readShapePoly('ne_10m_urban_areas.shp')
worldmapsin<-readShapePoly('ne_10m_admin_0_countries.shp')
worldmap<-fortify(worldmapsin)
urbanareas<-fortify(urbanareasin)
lineinworld<-(dat$ORIGIN_AIRPORT_ID%in%location$AIRPORT_ID)&(dat$DEST_AIRPORT_ID%in%location$AIRPORT_ID)
worldmap$long.recenter<-ifelse(worldmap$long<center-180,worldmap$long+360,worldmap$long)
urbanareas$long.recenter<-ifelse(urbanareas$long<center-180,urbanareas$long+360,urbanareas$long)
###########################################
worldmap.mean<-aggregate(x=worldmap[,c("long.recenter")],by=list(worldmap$group),FUN=mean)
worldmap.min<-aggregate(x=worldmap[,c("long.recenter")],by=list(worldmap$group),FUN=min)
worldmap.max<-aggregate(x=worldmap[,c("long.recenter")],by=list(worldmap$group),FUN=max)
worldmap.md<-cbind(worldmap.mean,worldmap.min[,2],worldmap.max[,2])
colnames(worldmap.md)<-c("group","mean","min","max")
worldmapt<-join(x=worldmap,y=worldmap.md,by=c("group"))
worldmapt$group.regroup<-1
worldmapt[(worldmapt$max>180)&(worldmapt$min<180)&(worldmapt$long.recenter>180),c("group.regroup")]<-2
worldmapt$group.regroup<-paste(worldmapt$group,worldmapt$group.regroup,sep=".")
worldmap.rg<-worldmapt
########################################
urbanareas.mean<-aggregate(x=urbanareas[,c("long.recenter")],by=list(urbanareas$group),FUN=mean)
urbanareas.min<-aggregate(x=urbanareas[,c("long.recenter")],by=list(urbanareas$group),FUN=min)
urbanareas.max<-aggregate(x=urbanareas[,c("long.recenter")],by=list(urbanareas$group),FUN=max)
urbanareas.md<-cbind(urbanareas.mean,urbanareas.min[,2],urbanareas.max[,2])
colnames(urbanareas.md)<-c("group","mean","min","max")
urbanareast<-join(x=urbanareas,y=urbanareas.md,by = c("group"))
urbanareast$group.regroup<-1
urbanareast[(urbanareast$max>180)&(urbanareast$min<180)&(urbanareast$long.recenter>180),c("group.regroup")]<-2
urbanareast$group.regroup<-paste(urbanareast$group,urbanareast$group.regroup,sep=".")
urbanareas.rg<-urbanareast
#####################################################################################
worldmap.rg<-worldmap.rg[order(worldmap.rg$group.regroup,worldmap.rg$order),]
worldmap.begin<-worldmap.rg[!duplicated(worldmap.rg$group.regroup),]
worldmap.end<-worldmap.rg[c(!duplicated(worldmap.rg$group.regroup)[-1],TRUE),]
worldmap.flag<-(worldmap.begin$long.recenter==worldmap.end$long.recenter)&(worldmap.begin$lat==worldmap.end$lat)
worldmap.plus<-worldmap.begin[!worldmap.flag,]
worldmap.plus$order<-worldmap.end$order[!worldmap.flag]+1
worldmap.cp<-rbind(worldmap.rg,worldmap.plus)
worldmap.cp<-worldmap.cp[order(worldmap.cp$group.regroup,worldmap.cp$order),]
###############
urbanareas.rg<-urbanareas.rg[order(urbanareas.rg$group.regroup,urbanareas.rg$order),]
urbanareas.begin<-urbanareas.rg[!duplicated(urbanareas.rg$group.regroup),]
urbanareas.end<-urbanareas.rg[c(!duplicated(urbanareas.rg$group.regroup)[-1],TRUE),]
urbanareas.flag<-(urbanareas.begin$long.recenter==urbanareas.end$long.recenter)&(urbanareas.begin$lat==urbanareas.end$lat)
urbanareas.plus<-urbanareas.begin[!urbanareas.flag,]
urbanareas.plus$order<-urbanareas.end$order[!urbanareas.flag]+1
urbanareas.cp<-rbind(urbanareas.rg,urbanareas.plus)
urbanareas.cp<-urbanareas.cp[order(urbanareas.cp$group.regroup,urbanareas.cp$order),]
############################################
range(worldmap.cp$long)
wrld<-c(geom_polygon(aes(long.recenter,lat,group=group.regroup), 
                      size=0.1, 
                      colour="#090D2A", 
                      fill="#090D2A", 
                      alpha=1, 
                      data=worldmap.cp))
urb<-c(geom_polygon(aes(long.recenter,lat,group=group.regroup),
                      size=0.3,
                      color="#FDF5E6",
                      fill="#FDF5E6",
                      alpha=1,    
                      data=urbanareas.cp))
#########################################################################???
bigmap<-1
airline<-geom_line(aes(long.recenter,lat,group=group.regroup,alpha=max(freq)^0.6*freq^0.4,color=0.9*max(freq)^0.6*freq^0.4),
                   color="#FFFFFF", 
                   size=0.2*bigmap, 
                   data=gcircles.rg)
airlinep<-geom_line(aes(long.recenter,lat,group=group.regroup,alpha=0.04*max(freq)^0.6*freq^0.4),
                    color="#FFFFFF",
                    size=2*bigmap,
                    #alpha=0.08,
                    data=gcircles.rg)
airlinepp<-geom_line(aes(long.recenter,lat,group=group.regroup,alpha=0.02*max(freq)^0.6*freq^0.4),
                     color="#ECFFFF",
                     size=4*bigmap,
                     #alpha=0.015,
                     data=gcircles.rg)
airlineppp<-geom_line(aes(long.recenter,lat,group=group.regroup,alpha=0.010*max(freq)^0.6*freq^0.4),
                      color="#ECFFFF",
                      size=8*bigmap,
                      #alpha=0.015,
                      data=gcircles.rg)
#gcircles.rg[gcircles.rg$group.regroup==1.1,]
#plot,plot2.png
png(6000*(bigmap^0.8),2000*(bigmap^0.8),file="plot1.png",bg="white")
ggplot()+
  wrld+
  urb+
  airline+
  scale_colour_gradient(low="#D9FFFF", high="#ECFFFF")+
  scale_alpha(range=c(0,1))+
  airlineppp+
  #scale_alpha_discrete(range = c(0.001, 0.005))+
  airlinepp+
  #scale_alpha(range = c(0.005, 0.015))+
  airlinep+
  #scale_alpha(range = c(0.015, 0.08))+
  #geom_polygon(aes(long,lat,group=group), size = 0.2, fill="#f9f9f9", colour = "grey65", data=worldmap) +
  #geom_line(aes(long.recenter,lat,group=group.regroup, color=freq, alpha=freq), size=0.4, data= gcircles.rg) +        # set transparency here                                                              # set color gradient here
  theme(panel.background=element_rect(fill='#00001C',color='#00001C'))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major=element_blank())+  
  theme(axis.ticks=element_blank())+ 
  theme(axis.title.x=element_blank())+ 
  theme(axis.title.y=element_blank())+
  theme(axis.text.x=element_blank())+ 
  theme(axis.text.y=element_blank())+
  theme(legend.position="none") +
  ylim(-65,75)+
  coord_equal()
dev.off()
###############################################################
dat$del<-dat$DEP_DEL15+dat$ARR_DEL15
dat$del[which(dat$del>0)]<-1
dat_del<-dat[which(dat$del==1),]
dat_arr<-dat[which(dat$ARR_DEL15==1),]
dat_dep<-dat[which(dat$DEP_DEL15==1),]
####################################
locat<-c(-110,40)
map<-get_map(locat,zoom=3,source="google",maptype="roadmap")
p<-ggmap(map)
lineinworld<-(dat$ORIGIN_AIRPORT_ID%in%location$AIRPORT_ID)&(dat$DEST_AIRPORT_ID%in%location$AIRPORT_ID)
worldinline<-(location$AIRPORT_ID%in%dat$ORIGIN_AIRPORT_ID)&(location$AIRPORT_ID%in%dat$DEST_AIRPORT_ID)
loc<-location[worldinline,]
p+geom_point(aes(x=LONGITUDE, y=LATITUDE),data=loc)
##############################################
################
data<-data.table(dat)
setkey(data,ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID,DEP_DEL15,ARR_DEL15)
dest_dep<-data[,.(freq_del=.N),by=.(DEST_AIRPORT_ID,DEP_DEL15)]
dest_dep<-spread(dest_dep,DEP_DEL15,freq_del)
names(dest_dep)<-c("ID","Not_delay","delay")
dest_dep[is.na(dest_dep)]<-0
dest_dep$delaypercent<-dest_dep$delay/(dest_dep$Not_delay+dest_dep$delay)
dest_dep$ID<-as.factor(dest_dep$ID)
d_d<-plot_ly(dest_dep,x=~ID,y=~Not_delay,type='bar',name='Not delay')%>%
  add_trace(y=~delay,name='Delay')%>%
  layout(title="Destination departure delay",yaxis=list(title='Count'),barmode='stack')
d_dp<-plot_ly(
  x=dest_dep$ID,
  y=dest_dep$delaypercent,
  name="delay percent",
  type="bar")%>%
  layout(title="Destination departure delay percent")
dest_arr<-data[,.(freq_del=.N),by=.(DEST_AIRPORT_ID,ARR_DEL15)]
dest_arr<-spread(dest_arr,ARR_DEL15,freq_del)
names(dest_arr)<-c("ID","Not_delay","delay")
dest_arr[is.na(dest_arr)]<-0
dest_arr$delaypercent<-dest_arr$delay/(dest_arr$Not_delay+dest_arr$delay)
dest_arr$ID<-as.factor(dest_arr$ID)
d_a<-plot_ly(dest_arr,x=~ID,y=~Not_delay,type='bar',name='Not delay')%>%
  add_trace(y=~delay,name='Delay')%>%
  layout(title="Destination arrival delay percent",yaxis=list(title='Count'),barmode='stack')
d_ap<-plot_ly(
  x=dest_arr$ID,
  y=dest_arr$delaypercent,
  name="delay percent",
  type="bar")%>%
  layout(title="Destination arrival delay percent")
ori_dep<-data[,.(freq_del=.N),by=.(ORIGIN_AIRPORT_ID,DEP_DEL15)]
ori_dep<-spread(ori_dep,DEP_DEL15,freq_del)
names(ori_dep)<-c("ID","Not_delay","delay")
ori_dep[is.na(ori_dep)]<-0
ori_dep$delaypercent<-ori_dep$delay/(ori_dep$Not_delay+ori_dep$delay)
ori_dep$ID<-as.factor(ori_dep$ID)
o_d<-plot_ly(ori_dep,x=~ID,y=~Not_delay,type='bar',name='Not delay')%>%
  add_trace(y=~delay,name='Delay')%>%
  layout(title="Origin departure delay",yaxis=list(title='Count'),barmode='stack')
o_dp<-plot_ly(
  x=ori_dep$ID,
  y=ori_dep$delaypercent,
  name="delay percent",
  type="bar")%>%
  layout(title="Origin departure delay percent")
ori_arr<-data[,.(freq_del=.N),by=.(ORIGIN_AIRPORT_ID,ARR_DEL15)]
ori_arr<-spread(ori_arr,ARR_DEL15,freq_del)
names(ori_arr)<-c("ID","Not_delay","delay")
ori_arr[is.na(ori_arr)]<-0
ori_arr$delaypercent<-ori_arr$delay/(ori_arr$Not_delay+ori_arr$delay)
ori_arr$ID<-as.factor(ori_arr$ID)
o_a<-plot_ly(ori_arr,x=~ID,y=~Not_delay,type='bar',name='Not delay')%>%
  add_trace(y=~delay,name='Delay')%>%
  layout(title="Origin arrival delay",yaxis=list(title='Count'),barmode='stack')
o_ap<-plot_ly(
  x=ori_arr$ID,
  y=ori_arr$delaypercent,
  name="delay percent",
  type="bar")%>%
  layout(title="Origin arrival delay percent")
dest<-data[,.(freq_del=.N),by=.(DEST_AIRPORT_ID,del)]
dest<-spread(dest,del,freq_del)
names(dest)<-c("ID","Not_delay","delay")
dest[is.na(dest)]<-0
dest$delaypercent<-dest$delay/(dest$Not_delay+dest$delay)
dest$ID<-as.factor(dest$ID)
d<-plot_ly(dest,x=~ID,y=~Not_delay,type='bar',name='Not delay')%>%
  add_trace(y=~delay,name='Delay')%>%
  layout(title="Destination delay",yaxis=list(title='Count'),barmode='stack')
d_p<-plot_ly(
  x=dest$ID,
  y=dest$delaypercent,
  name="delay percent",
  type="bar")%>%
  layout(title="Destination delay percent")
orig<-data[,.(freq_del=.N),by=.(ORIGIN_AIRPORT_ID,del)]
orig<-spread(orig,del,freq_del)
names(orig)<-c("ID","Not_delay","delay")
orig[is.na(orig)]<-0
orig$delaypercent<-orig$delay/(orig$Not_delay+orig$delay)
orig$ID<-as.factor(orig$ID)
o<-plot_ly(orig,x=~ID,y=~Not_delay,type='bar',name='Not delay')%>%
  add_trace(y=~delay,name='Delay')%>%
  layout(title="Origin delay",yaxis=list(title='Count'),barmode='stack')
o_p<-plot_ly(
  x=orig$ID,
  y=orig$delaypercent,
  name="delay percent",
  type="bar")%>%
  layout(title="Origin delay percent")
#############################################################
d_dep<-id[,1]%in%(dest_dep[which(dest_dep$delaypercent>=0.25),]$ID)
id[d_dep,]
d_arr<-id[,1]%in%(dest_arr[which(dest_arr$delaypercent>=0.25),]$ID)
id[d_arr,]
o_dep<-id[,1]%in%(ori_dep[which(ori_dep$delaypercent>=0.25),]$ID)
id[o_dep,]
o_arr<-id[,1]%in%(ori_arr[which(ori_arr$delaypercent>=0.25),]$ID)
id[o_arr,]
####################plot some specific airports#########################################
dat_ALT<-rbind(dat[which(dat$ORIGIN_AIRPORT_ID==10397),],dat[which(dat$DEST_AIRPORT_ID==10397),])
worldline<-dat_DEN[,c("ORIGIN_AIRPORT_ID","DEST_AIRPORT_ID","del")]##***************
flights.ag<-ddply(worldline, c("ORIGIN_AIRPORT_ID","DEST_AIRPORT_ID","del"), function(x) count(x$DEST_AIRPORT_ID))
flights.ll<-merge(flights.ag,location, by.x="DEST_AIRPORT_ID", by.y="AIRPORT_ID", all.x=T)
flights.ll<-flights.ll[order(flights.ll$ORIGIN_AIRPORT_ID,flights.ll$DEST_AIRPORT_ID),]
flights.lf<-merge(flights.ag,location, by.x="ORIGIN_AIRPORT_ID", by.y="AIRPORT_ID", all.x=T)
flights.lf<-flights.lf[order(flights.lf$ORIGIN_AIRPORT_ID,flights.lf$DEST_AIRPORT_ID),]
rts<-gcIntermediate(flights.lf[,c('LONGITUDE','LATITUDE')],flights.ll[,c('LONGITUDE','LATITUDE')],100,breakAtDateLine=FALSE,addStartEnd=TRUE,sp=TRUE)
rts<-as(rts,"SpatialLinesDataFrame")
rts.ff<-fortify(rts)
flights.ll$id<-as.character(c(1:nrow(flights.ll)))
gcircles<-merge(rts.ff,flights.ll,all.x=T,by="id")
gcircle<-data.table(gcircles)
setkey(gcircle,id)
gcircle[,group.regroup:=RegroupElements(long,id),by="id"]
gcircles.rg<-as.data.frame(gcircle)
locat<-c(-100,40)
map<-get_map(locat,zoom=3,source="google",maptype="roadmap")
p<-ggmap(map)
airline<-geom_line(aes(long,lat,group=group.regroup,colour=as.factor(del)),data=gcircles.rg)
p+airline+ggtitle("ORD AIRLINE")+scale_colour_discrete(name=NULL,breaks=c("0", "1"),labels=c("Not delay", "Delay"))
dat_LAX<-rbind(dat[which(dat$ORIGIN_AIRPORT_ID==12892),],dat[which(dat$DEST_AIRPORT_ID==12892),])
dat_ORD<-rbind(dat[which(dat$ORIGIN_AIRPORT_ID==13930),],dat[which(dat$DEST_AIRPORT_ID==13930),])
dat_DEN<-rbind(dat[which(dat$ORIGIN_AIRPORT_ID==11292),],dat[which(dat$DEST_AIRPORT_ID==11292),])
#############classification##########################
library(e1071)
library(randomForest)
library(DMwR)
dat_LAX<-dat[which(dat$ORIGIN_AIRPORT_ID==12892),]
dat_OAK<-dat[which(dat$ORIGIN_AIRPORT_ID==13796),]
dat_SFO<-dat[which(dat$ORIGIN_AIRPORT_ID==14771),]
dat_SAN<-dat[which(dat$ORIGIN_AIRPORT_ID==14679),]
#################
dat_SAN<-dat_SAN[,c(3,5,6,8,11,18,20,25,33,16,23)]
for(i in 1:9){
  dat_OAK[,i]<-as.factor(dat_OAK[,i])
}

model_OAK<-randomForest(DEP_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                        +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                        +DEST_AIRPORT_ID+DISTANCE_GROUP,data=dat_OAK_tr)
dat_OAK_tr<-dat_OAK[-seq(1,dim(dat_OAK)[1],by=5),]
dat_OAK_te<-dat_OAK[seq(1,dim(dat_OAK)[1],by=5),]
SMOTE(DEP_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
      +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
      +DEST_AIRPORT_ID+DISTANCE_GROUP,dat_OAK_tr,)
table(predict(model_OAK,dat_OAK_te),dat_OAK_te$DEP_DEL15)
table(dat_OAK_te$DEP_DEL15)

dat_svm<-svm(data=dat_tr1,kernel="linear",cost=1)




dat_OAK<-rbind(dat[which(dat$ORIGIN_AIRPORT_ID==13796),],dat[which(dat$DEST_AIRPORT_ID==13796),])
for(i in 1:9){
  dat_OAK[,i]<-as.factor(dat_OAK[,i])
}
dat_OAK1<-SMOTE(DEP_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                +DEST_AIRPORT_ID+DISTANCE_GROUP,dat_OAK,prec.over=100,
                prec.under=200)
dat_OAK2<-SMOTE(ARR_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                +DEST_AIRPORT_ID+DISTANCE_GROUP,dat_OAK,prec.over=100,
                prec.under=200)
dat_OAK_tr<-dat_OAK[-seq(1,dim(dat_OAK)[1],by=5),]
dat_OAK_te<-dat_OAK[seq(1,dim(dat_OAK)[1],by=5),]
for(i in 1:9){
  dat_OAK[,i]<-as.factor(dat_OAK[,i])
}
model_OAK_reg<-randomForest(DEP_DELAY~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                        +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                        +DEST_AIRPORT_ID+DISTANCE_GROUP,data=dat_OAK_tr)
xy$x<-round(xy$x)
xy$origin<-as.character(dat_OAK_te$ORIGIN_AIRPORT_ID)
xy$origin[which(xy$origin!="13796")]<-"Arrive at OAK"
xy$origin[which(xy$origin=="13796")]<-"Departure from OAK"
ggplot()+geom_point(aes(x=y,y=x,colour=as.factor(origin)),data=xy)+ggtitle("Regression on OAK Departure delay")+scale_colour_discrete(name="Airport")

state<-strsplit(as.character(id$Description)," |  ")
c<-list()
for(i in 1:length(state)){
  result<-grepl(":",state[[i]],fixed=TRUE)
  c[[i]]<-state[[i]][which(result==TRUE)]
}
state<-gsub("[[:punct:]]","",c)
id<-cbind(id,state)
library(dplyr)
#############################################
dat_SFO<-dat[which(dat$ORIGIN_AIRPORT_ID==14771),]
dat_SFO<-inner_join(dat_SFO,id[,c(1,3)],by=c("DEST_AIRPORT_ID"="Code"))
dat_SFO<-dat_SFO[,c(3,5,6,8,11,18,20,25,33,16,23,34)]
for(i in 1:12){
  dat_SFO[,i]<-as.character(dat_SFO[,i])
}
dat_SFO$state[which(dat_SFO$state=="CA")]<-dat_SFO[which(dat_SFO$state=="CA"),"DEST_AIRPORT_ID"]
dat_SFO$DEST_AIRPORT_ID<-dat_SFO$state
dat_SFO<-dat_SFO[,c(-12,-13)]
for(i in 1:9){
  dat_SFO[,i]<-as.factor(dat_SFO[,i])
}
dat_SFO_tr<-dat_SFO[-seq(1,dim(dat_SFO)[1],by=5),]
dat_SFO_te<-dat_SFO[seq(1,dim(dat_SFO)[1],by=5),]
model_SFO3<-randomForest(DEP_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                         +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                         +DEST_AIRPORT_ID+DISTANCE_GROUP,data=dat_SFO_tr)
model_SFO4<-randomForest(ARR_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                         +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                         +DEST_AIRPORT_ID+DISTANCE_GROUP,data=dat_SFO_tr)
############################################
dat_LAX<-dat[which(dat$ORIGIN_AIRPORT_ID==12892),]
dat_LAX<-inner_join(dat_LAX,id[,c(1,3)],by=c("DEST_AIRPORT_ID"="Code"))
dat_LAX<-dat_LAX[,c(3,5,6,8,11,18,20,25,33,16,23,34)]
for(i in 1:12){
  dat_LAX[,i]<-as.character(dat_LAX[,i])
}
dat_LAX$state[which(dat_LAX$state=="CA")]<-dat_LAX[which(dat_LAX$state=="CA"),"DEST_AIRPORT_ID"]
dat_LAX$DEST_AIRPORT_ID<-dat_LAX$state
dat_LAX<-dat_LAX[,c(-12,-13)]
for(i in 1:9){
  dat_LAX[,i]<-as.factor(dat_LAX[,i])
}
dat_LAX_tr<-dat_LAX[-seq(1,dim(dat_LAX)[1],by=5),]
dat_LAX_te<-dat_LAX[seq(1,dim(dat_LAX)[1],by=5),]
model_LAX3<-randomForest(DEP_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                         +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                         +DEST_AIRPORT_ID+DISTANCE_GROUP,data=dat_LAX_tr)
model_LAX4<-randomForest(ARR_DEL15~MONTH+DAY_OF_WEEK+DEP_TIME_BLK
                         +UNIQUE_CARRIER+ORIGIN_AIRPORT_ID
                         +DEST_AIRPORT_ID+DISTANCE_GROUP,data=dat_LAX_tr)
dat<-inner_join(dat,id[,c(1,3)],by=c("ORIGIN_AIRPORT_ID"="Code"))
dat_OTH<-dat[which(dat$state.x=="CA"),]
dat_OTH<-dat_OTH[,c(3,5,6,8,11,18,20,25,33,16,23,34)]
for(i in 1:12){
  dat_OTH[,i]<-as.character(dat_OTH[,i])
}
dat_OTH<-dat_OTH[-which(dat_OTH$ORIGIN_AIRPORT_ID==12892),]
dat_OTH<-dat_OTH[-which(dat_OTH$ORIGIN_AIRPORT_ID==13796),]
dat_OTH<-dat_OTH[-which(dat_OTH$ORIGIN_AIRPORT_ID==14771),]
dat_OTH<-dat_OTH[-which(dat_OTH$ORIGIN_AIRPORT_ID==14679),]
for(i in 1:9){
  dat_OTH[,i]<-as.factor(dat_OTH[,i])
}
dat_OTH_tr<-dat_OTH[-seq(1,dim(dat_OTH)[1],by=5),]
dat_OTH_te<-dat_OTH[seq(1,dim(dat_OTH)[1],by=5),]
###########
write.table(dat_SFO,file="dat_SFO.csv",sep = ",",col.names=names(dat_SFO))
###########
write.table(dat_LAX,file="dat_LAX.csv",sep = ",",col.names=names(dat_LAX))
###########
write.table(dat_OTH,file="dat_OTH.csv",sep = ",",col.names=names(dat_OTH))
###########
write.table(dat_SAN,file="dat_SAN.csv",sep = ",",col.names=names(dat_SAN))
###########
write.table(dat_OAK,file="dat_OAK.csv",sep = ",",col.names=names(dat_OAK))
###########
dir()
OAK<-read.csv(dir()[14],header = T)
OAK$pred<-round(OAK$pred)
OTH<-read.csv(dir()[15],header = T)
OTH$pred<-round(OTH$pred)
SAN<-read.csv(dir()[16],header = T)
SAN$pred<-round(SAN$pred)
LAX<-read.csv(dir()[14],header = T)
LAX$pred<-round(LAX$pred)
SFO<-read.csv(dir()[18],header = T)
SFO$pred<-round(SFO$pred)
ggplot()+geom_point(aes(x=TRUE.,y=pred,colour="red"),data=OAK)+ggtitle("Regression of OAK Airport")+geom_abline(intercept=0,slope=1,colour="blue")
ggplot()+geom_point(aes(x=TRUE.,y=pred,colour="red"),data=OTH)+ggtitle("Regression of OTH Airport")+geom_abline(intercept=0,slope=1,colour="blue")
ggplot()+geom_point(aes(x=TRUE.,y=pred,colour="red"),data=SAN)+ggtitle("Regression of SAN Airport")+geom_abline(intercept=0,slope=1,colour="blue")
ggplot()+geom_point(aes(x=TRUE.,y=pred,colour="red"),data=SFO)+ggtitle("Regression of SFO Airport")+geom_abline(intercept=0,slope=1,colour="blue")
ggplot()+geom_point(aes(x=TRUE.,y=pred,colour="red"),data=LAX)+ggtitle("Regression of LAX Airport")+geom_abline(intercept=0,slope=1,colour="blue")
OAK_MSE<-mean((OAK$pred-OAK$TRUE.)^2)
OTH_MSE<-mean((OTH$pred-OTH$TRUE.)^2)
SAN_MSE<-mean((SAN$pred-SAN$TRUE.)^2)
SFO_MSE<-mean((SFO$pred-SFO$TRUE.)^2)
LAX_MSE<-mean((LAX$pred-LAX$TRUE.)^2)
##########################################
TrueClass=factor(c(0,0,1,1))
PredictedClass=factor(c(0,1,0,1))
Y=c(26500,2857,7061,1674)
df=data.frame(TrueClass,PredictedClass,Y)
ggplot(data=df,mapping=aes(x=TrueClass,y=PredictedClass))+
  geom_tile(aes(fill=Y),colour="white")+
  geom_text(aes(label=sprintf("%1.0f",Y)),vjust=1,size=12)+
  scale_fill_gradient(low="skyblue",high="skyblue4")+
  theme_bw()+theme(legend.position="none")+
  ggtitle("LAX")
#########################
LAX<-matrix(c(26444,2913,7081,1654),c(2,2))
OAK_before<-matrix(c(6896,89,1699,46),c(2,2))
OAK_after<-matrix(c(6090,895,1473,272),c(2,2))
SAN<-matrix(c(10244,1427,1972,268),c(2,2))
SFO<-matrix(c(22729,1277,6080,521),c(2,2))          
OTH<-matrix(c(23869,7861,4064,2309),c(2,2))
c<-c()
b<-list(OAK_before,OAK_after,LAX,SAN,SFO,OTH)
for(i in 1:6){
  c[i]<-(b[[i]][1,2]+b[[i]][2,1])/sum(b[[i]])
}
a<-c()
for(i in 1:6){
  a[i]<-b[[i]][2,2]/sum(b[[i]][,2])
}
k<-matrix(c(c,a),c(6,2),dimnames=list(c("OAK_before","OAK_after","LAX","SAN","SFO","OTH"),c("Total Error Rate","Precision of Delay")))


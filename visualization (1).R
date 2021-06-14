#install.packages("ggmap")
#install.packages("maptools")
#install.packages("ggthemes")
#install.packages("ggplot2")
#install.packages("sp")
library(maptools)
library(ggmap)
library(ggthemes)
library(ggplot2)
library(sp)

data1 <- read.csv("~/R/01_District_wise_crimes_committed_IPC_2001_2012.csv")
#---------------Recomputing total ipc crimes without the 'other IPCs' field-------------
for (i in 1:nrow(data1)){
  data1$TOTAL.IPC.CRIMES[i]<-sum(data1[i,4:31])
}

############################### CRIMES PER YEAR ACROSS INDIA ################
# segregating data year-wise
b<-lapply(unique(data1$YEAR), function(x) data1[data1$YEAR == x,])
# retrieving only Totals of each state
for (i in b){
  i<-subset(i,i$DISTRICT!="TOTAL" & i$DISTRICT!="DELHI UT TOTAL")
}
totcrime<-c()
# Computing total crimes commited per year
for (i in b){
  totcrime<-append(totcrime,sum(i$TOTAL.IPC.CRIMES))
  
}
# Creating a dataframe for year-wise crime count and visualizing the data as a bar plot
peryear<-data.frame(YEAR=c(2001:2012),crimecount=totcrime)
ggplot(peryear)+geom_bar(aes(YEAR,crimecount,fill=YEAR),stat="identity")+xlab("Year") +ylab("Total Crimes") + ggtitle("Crimes per Year (India 2001-2012)") + guides(fill=FALSE) + theme_economist_white()






















####################### CRIMES PER STATE OVER THE YEARS ############################
# Retrieving only totals per state
newdata<-subset(data1,data1$DISTRICT=="TOTAL"| data1$DISTRICT=="DELHI UT TOTAL")
# Segregating data based on states/Union territories
a<-lapply(unique(data1$STATE.UT), function(x) data1[data1$STATE.UT == x,])
totperstate<-c()
states_and_ut<-newdata$STATE.UT
# Calculate total crime count per state throughout the years 2001-2012
for (i in a){
  j<-subset(i,i$DISTRICT=="TOTAL"| i$DISTRICT=="DELHI UT TOTAL")
  totperstate<-append(totperstate,sum(j$TOTAL.IPC.CRIMES))
}
# Creating a dataframe for state/UT-wise crime count throughout the years.
top_states<-data.frame(States_and_UT=unique(states_and_ut),Totalcrimes=totperstate)
# Sorting the data in descending order of crime count and retrieving Top 5 states with highest crime count over the years
top_states <- top_states[order(-top_states$Totalcrimes),]
top_five<-head(top_states,5)
#-------------------------Top 5 States with high crime rate------------------------
#MAHARASHTRA
#ANDHRA PRADESH
#UTTAR PRADESH
#MADHYA PRADESH
#RAJASTHAN

# Retrieving data regarding the Top 5 states with highest crime and plotting line graphs of various crimes per state
top_five_data<-subset(newdata,newdata$STATE.UT %in% top_five$States_and_UT)
states_data<-data.frame(STATE.UT=top_five_data$STATE.UT,YEAR=top_five_data$YEAR,MURDER=top_five_data$MURDER,RAPE=top_five_data$RAPE,Kidnapping_and_Abduction=top_five_data$KIDNAPPING.and.ABDUCTION,DRBT=(top_five_data$DACOITY+top_five_data$ROBBERY+top_five_data$BURGLARY+top_five_data$THEFT),Cheating=top_five_data$CHEATING,Arson=top_five_data$ARSON)
year_wise_five<-lapply(unique(states_data$STATE.UT), function(x) states_data[states_data$STATE.UT == x,])

for (i in year_wise_five){
  plot(i[,c(2,3)],type='o',ylim=c(min(i[,c(3,4,5,6,7,8)]),max(i[,c(3,4,5,6,7,8)])+20000),xlim=c(2000,2013),col='red',ylab='Crime count',xlab='YEAR',main=unique(i$STATE.UT))
  lines(i[,c(2,4)],type='o',col='blue')
  lines(i[,c(2,5)],type='o',col='green')
  lines(i[,c(2,6)],type='o',col='purple')
  lines(i[,c(2,7)],type='o',col='cyan')
  lines(i[,c(2,8)],type='o',col='black')
  legend("topright",pch=1, c('Murder','Rape','Kidnapping and abduction','DRBT (Dacoity, Robbery, Burglary, Theft)','Cheating','Arson'), lty=c(1,1), lwd=c(2.5,2.5),col=c('red','blue','green','purple','cyan','black')) 
  }


############################ CRIME COUNT PER DISTRICT ################################# 
# Retrieve district wise data but exclude the redundant 'TOTAL' fields
newdata<-subset(data1,data1$DISTRICT!="TOTAL" & data1$DISTRICT!="DELHI UT TOTAL")
totperdistrict<-c()
districts<-newdata$DISTRICT
# Segregating data district-wise
d<-lapply(unique(newdata$DISTRICT), function(x) newdata[newdata$DISTRICT == x,])
# Calculating total crimes committed per district over the years
for (i in d){
  totperdistrict<-append(totperdistrict,sum(i$TOTAL.IPC.CRIMES))
}
# Creating a dataframe for District-wise crime count while ignoring certain vague fields
crimeperdistrict<-data.frame(DISTRICT=unique(districts),Totalcrimes=totperdistrict)
crimeperdistrict<-subset(crimeperdistrict,crimeperdistrict$DISTRICT!="NORTH" & crimeperdistrict$DISTRICT!="SOUTH" & crimeperdistrict$DISTRICT!="EAST" & crimeperdistrict$DISTRICT!="WEST" & crimeperdistrict$DISTRICT!="CENTRAL" & crimeperdistrict$DISTRICT!="NORTH EAST" & crimeperdistrict$DISTRICT!="NORTH WEST" & crimeperdistrict$DISTRICT!="SOUTH WEST" & crimeperdistrict$DISTRICT!="SOUTH EAST" & crimeperdistrict$DISTRICT!="RURAL" & crimeperdistrict$DISTRICT!="URBAN")
# Retrieving latitude and longitude (geolocation) values for each district
for (i in 1:nrow(crimeperdistrict)) {
  latlon = geocode(as.character(crimeperdistrict[i,1]))
  crimeperdistrict$lon[i] = as.numeric(latlon[1])
  crimeperdistrict$lat[i] = as.numeric(latlon[2])

}
# Ignoring other erroneous rows where the District name might not make complete sense and hence provide wrong geolocation value
crimeperdistrict<-subset(crimeperdistrict,crimeperdistrict$lon < 100.0 & crimeperdistrict$lon > 60.0)

# Read a district-level outline map of India
ind2 <-readRDS("C:/Users/harsh/OneDrive/Documents/R/Project/IND_adm2.rds")
# Retrieve geocode for India and plot it's google map
india_center<-as.numeric(geocode("India"))
india<-ggmap(get_googlemap(center=india_center, scale=1, zoom=4), extent="normal")
# PS: Seen better in a larger window
india +  geom_point(data=crimeperdistrict,aes(x=lon, y=lat, col='red',size=Totalcrimes))+ylab('Longitude')+xlab('Latitude') + ggtitle("Crimes per District")

# Order Districts based on their crime count over the years and plot highest five
crimeperdistrict<-crimeperdistrict[order(-crimeperdistrict$Totalcrimes),]
headcrimeperdistrict<-head(crimeperdistrict,5)
ggplot(ind2) + geom_path(aes(x=long, y=lat,group=group), color='gray') + coord_equal()+ geom_point(data=headcrimeperdistrict,aes(x=lon, y=lat, col="red",size=Totalcrimes))  + geom_text(data=headcrimeperdistrict,aes(x = lon, y = lat, label = headcrimeperdistrict$DISTRICT), size = 2)+ylab('Longitude')+xlab('Latitude') + ggtitle("Crimes per District (TOP 5)")

# Order Districts based on their crime count over the years and plot lowest ten
tailcrimeperdistrict<-tail(crimeperdistrict,10)
ggplot(ind2) + geom_path(aes(x=long, y=lat,group=group), color='gray') + coord_equal()+ geom_point(data=tailcrimeperdistrict,aes(x=lon, y=lat, col="red",size=Totalcrimes))  + geom_text(data=tailcrimeperdistrict,aes(x = lon, y = lat, label = tailcrimeperdistrict$DISTRICT), size = 2)+ylab('Longitude')+xlab('Latitude') + ggtitle("Crimes per District (BOTTOM 10)")
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages(geosphere)
library(geosphere)
library(leaflet)
#library(gstat)
library(sp)
library(tidyverse)
#input the excel file into R
df = read_csv("Downloads/ANZ synthesised transaction dataset.csv")

#have a brief check 
str(df)
summary(df)

#check date format, it is better to implement with date format
str(df$date) 
df$date<-as.Date(df$date,format="%d.%m.%y")
min(df$date) #2018-08-01
max(df$date) #2018-10-31
dateRange<-seq(min(df$date),max(df$date),1)
dateRange[!dateRange %in% df$date] #2018-08-16 is missing

#get weekday and hour information
df$extraction
df$extraction<-as.character(df$extraction)
str(df$extraction)
x=substr(df$extraction,12,19)
str(as.POSIXlt(x))
df$extraction=as.POSIXct(substr(df$extraction,12,19),format="%H:%M:%OS")
df$hour=hour(df$extraction)
df$weekday=weekdays(df$date)

#check customer_id, account if they are one-to-one
df %>% select(account,customer_id) %>% unique() %>% nrow() #100

#longtitude & latitude manupulation
summary(df)
dfloc<-df[,c("long_lat","merchant_long_lat")]
dfloc<-dfloc %>% separate ("long_lat",c("cus_long","cus_lat"),sep=" ")
dfloc<-dfloc %>% separate ("merchant_long_lat",c("mer_long","mer_lat"),sep=" ")
dfloc<-data.frame(sapply(dfloc,as.numeric))
df<-cbind(df,dfloc)


#check the range of customer location 
# it should be in the range of long>`113,long<154; lat>-44, lat<-10
df_bridging<-df %>% filter(!(cus_lat>(-44)&cus_lat<(-10)&cus_long<154&cus_long>113))
length(unique(df_bridging$customer_id))  #only one 
#str(df_bridging)
unique(df_bridging$customer_id)
df1<-df_bridging %>% filter(customer_id=="CUS-1617121891")
#df1
#all transactions are within Australia

#check if any missing value 
apply(df,2, function(x) sum(is.na(x)|x==""))
# distinguish between NA and no value-""
apply(df,2, function(x) sum(is.na(x)))
apply(df,2, function(x) sum(x==""))

#check the number of unique vlaues of each column 
apply(df, 2, function(x) length(unique(x)))

df_temp<-df %>% filter(merchant_id!="")
df_temp1<-df %>% filter(is.nan(merchant_id))
summary(df_temp)
summary(df_temp1)
str(df_temp)
df_temp1
summary(df_method)

#plot the transaction amount excluding outliers 
par(mar = rep(2, 4))
hist(df_method$amount[!df_method$amount %in% boxplot.stats(df_method$amount)$out],
     xlab = "Transaction Amount",main = 'Histogram of purchase amount')

boxplot.stats(df_method$amount)
hist(df_method$amount[df_method$amount %in% boxplot.stats(df_method$amount)$stats],
     xlab = "Transaction Amount",main = 'Histogram of purchase amount')

hist(df$amount[!df$amount %in% boxplot.stats(df$amount)$out],
     xlab = "Transaction Amount",main = 'Histogram of Overall purchase amount')

#every customer monthly spend
df2<-df %>% group_by(customer_id) %>% summarise(cus_monthly_spend=round(n()/3,0))
hist(df2$cus_monthly_spend,xlab = "Monthly spend volume",
     ylab = "Number of customers ",main="Histogram of transaction volume")

df3<-df %>% select(date,weekday) %>% 
  group_by(date,weekday) %>% 
  summarise(daily_avg=n()) %>% 
  group_by(weekday) %>%
  summarise(avg_vol=mean(daily_avg,na.rm = TRUE))

df3$weekday<-factor(df3$weekday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                         "Friday", "Saturday", "Sunday"))

str(df3)

ggplot(df3,aes(x=weekday,y=avg_vol))+geom_point()+geom_line(aes(group=1))+
  ggtitle("Average transaction volumne by weekday")+
  labs(x="weekday",y="Transaction volumne")
  
#by time
names(df)
df4<-df %>% select(date,hour)%>% 
  group_by(date,hour) %>% 
  summarise(hour_avg=n()) %>% 
  group_by(hour) %>%
  summarise(hour_vol=mean(hour_avg,na.rm = TRUE))

df4
ggplot(df4,aes(x=hour,y=hour_vol))+geom_point()+geom_line(aes(group=1))+
  ggtitle("Average transaction volumne by hour")+
  labs(x="hour",y="Transaction volumne")
  
df_noforeign<-df %>%
  select(cus_long, cus_lat,mer_long, mer_lat) %>%
  filter(cus_long>113&cus_long<154&cus_lat>(-44)&cus_lat<(-10))
df_noforeign<-data.frame(lapply(df_noforeign, as.numeric))
names(df_noforeign)
df_noforeign$dist<-distHaversine(df_noforeign[,1:2], df_noforeign[,3:4])/1000
hist(df_noforeign$dist,main = "Distance between customers and merchants", xlab = "Distance (km)")

hist(df_noforeign$dist[df_noforeign$dist<100],
     main = "Distance between customers and merchants", 
     xlab = "Distance (km)")

mer_dist<-function(id){
  cus_icon<-makeAwesomeIcon(icon="home",markerColor="green")
  
  merchant_loc<-subset(df_temp[, c("customer_id","mer_long","mer_lat")],customer_id==id)
  merchant_loc<-merchant_loc[,c("mer_long", "mer_lat")]
  
  cus_loc<-unique(subset(df_temp[, c("customer_id", "cus_long","cus_lat")],customer_id==id))
  #cus_loc_simple<-cus_loc %>% separate("long_lat",c("cus_long", "cus_lat"),sep=" ")
  
  df_t<-data.frame(longtitude=as.numeric(merchant_loc$mer_long), 
                   latitude=as.numeric(merchant_loc$mer_lat))

  
  coordinates(df_t)<- ~longtitude+latitude
  leaflet(df_t) %>% addMarkers() %>% addTiles() %>%
    addAwesomeMarkers(
      lng=as.numeric(cus_loc$cus_long), lat=as.numeric(cus_loc$cus_lat),
      icon=cus_icon,
      
    )
  
  }
mer_dist("CUS-51506836")
names(df_temp)

cus_loc<-unique(subset(df_temp[, c("customer_id", "cus_long","cus_lat")],customer_id=="CUS-51506836"))
cus_loc


#Task2 Basic Prediction Task 
df_csmp <- df %>% filter (!(txn_description %in% c("PAY/SALARY",
                                                 "INTER BANK", 
                                                 "PHONE BANK","PAYMEN T")))

df_inc <- data.frame(customer_id=unique(df_csmp$customer_id))
df_inc

tabulate(c(2,4,5))
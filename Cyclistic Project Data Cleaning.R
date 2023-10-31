
##Installing and loading necessary packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(dplyr)

##Setting File Directory

print(getwd())
setwd("~/Documents/Combine")

##Importing data to Rstudio
Jan <- read.csv("202101-divvy-tripdata.csv")
Feb <- read.csv("202102-divvy-tripdata.csv")
Mar <- read.csv("202103-divvy-tripdata.csv")
Apr <- read.csv("202104-divvy-tripdata.csv")
May <- read.csv("202105-divvy-tripdata.csv")
Jun <- read.csv("202106-divvy-tripdata.csv")
Jul <- read.csv("202107-divvy-tripdata.csv")
Aug <- read.csv("202108-divvy-tripdata.csv")
Sep <- read.csv("202109-divvy-tripdata.csv")
Oct <- read.csv("202110-divvy-tripdata.csv")
Nov <- read.csv("202111-divvy-tripdata.csv")
Dec <- read.csv("202112-divvy-tripdata.csv")

##Merging data into a data frame
triprawdata <- rbind(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)


###Process Data
##Cleaning and Preparing Data
colnames(triprawdata) #column names of data

nrow(triprawdata) #no. of rows

ncol(triprawdata) #no. of columns

sum(is.na(triprawdata)) # sum of NA's in data

head(triprawdata)   #see first 6 rows of data frame

str(triprawdata)   #See list of columns and data type

summary(triprawdata)  #Statistical summary of data

##Removing Rows with NA
triprawdata<- na.omit(triprawdata)

##Making consistent Date formats in started_at
# Date format is inconsistent some dates are in YYYY-MM-DD format or MM/DD/YYYY
start<-as.POSIXlt(triprawdata$started_at, tz = "","%m/%d/%Y %H:%M")
start<- data.frame(start)
start<- (start[1:nrow(na.omit(start)),])
start<- data.frame(start)



start1<-as.POSIXlt(triprawdata$started_at, tz = "","%Y-%m-%d %H:%M")
start1<- data.frame(start1)
start1<- (start1[(nrow(start)+1):nrow(start1),])
start1<-data.frame(start1)


names(start)<- "start"
names(start1)<- "start"
start_time<- (rbind(start,start1))
triprawdata$started_at <- NULL
triprawdata['started_at']<- start_time

##Making consistent Date formats in ended_at
end<-(as.POSIXlt(triprawdata$ended_at, tz = "","%m/%d/%Y %H:%M"))
end<- data.frame(end)
end<- (end[1:nrow(na.omit(end)),])
end<-data.frame(end)


end1<-(as.POSIXlt(triprawdata$ended_at, tz = "","%Y-%m-%d %H:%M"))
end1<- data.frame(end1)
end1<- (end1[(nrow(end)+1):nrow(end1),])
end1<-data.frame(end1)


names(end)<- "end"
names(end1)<- "end"
end_time<- (rbind(end,end1))
triprawdata$ended_at<- NULL
triprawdata['ended_at']<- end_time

##Adding a column for Ride length
ride_length<- difftime(end_time[,], start_time[,], units = "mins")
ride_length<-data.frame(ride_length)
triprawdata <- cbind(triprawdata,ride_length)

##Checking consistency of Ride length & Ride ID
# Some Ride lengths are negative,start time and end time are found interchanged
clean_data <- triprawdata %>% filter(ride_length>0)

##checking duplicate ride id
sum(duplicated(clean_data$ride_id))

##Adding columns for the day of the week into the data frame
weekday<- weekdays(clean_data$started_at)
weekday<-  factor(weekday, levels =c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday")) 
weekday<- data.frame(weekday)
clean_data<- cbind(clean_data,weekday)

#write.csv(clean_data,"~/Downloads/clean_data.csv", row.names = FALSE)

##Rounding of coordinates to 4 digits
for (i in 7:10) {
  clean_data[,i]<- round(clean_data[,i],4)
  clean_data[,i]<- as.character(clean_data[,i])
}

##Replacing missing station names in start station
start_no_data<- clean_data%>% filter (start_station_name =="")%>% group_by(start_lat,start_lng,start_station_name,member_casual)%>% summarise(n=n())

# writing file to excel for further analysis
#write.csv(start_no_data,"start_no_data.csv",row.names = FALSE)
# filtering top data coordinates and assigning start station name 
#filtering out in excel top 50 coordinates and finding their station name from our database clean_data

start_no_data<-read.csv("start_no_data.csv")

start_data<- clean_data%>% filter (start_station_name !="") %>%group_by(start_lat,start_lng,start_station_name,member_casual)%>% summarise(n=n())

##Changing Station names (Some station names has been changed over the time)
# changing the names of 6 stations to their new names in excel 

#DuSable Lake Shore Dr & Monroe St
#Lake Shore Dr & Monroe St

#Lake Shore Dr & Diversey Pkwy
#DuSable Lake Shore Dr & Diversey Pkwy

#DuSable Lake Shore Dr & North Blvd
#Lake Shore Dr & North Blvd

#Dusable Lake Shore Dr & Ohio St    
#Lake Shore Dr & Ohio St

#Lake Shore Dr & Wellington Ave
#Dusable Lake Shore Dr & Wellington Ave


#Lake Shore Dr & Belmont Ave
#Dusable Lake Shore Dr & Belmont Ave


for ( i in 1:nrow(start_data)){
  if (start_data[i,3]== "Lake Shore Dr & Diversey Pkwy"){
    start_data[i,3]="DuSable Lake Shore Dr & Diversey Pkwy"
  }
}

for ( i in 1:nrow(start_data)){
  if (start_data[i,3]== "Lake Shore Dr & Monroe St"){
    start_data[i,3]="DuSable Lake Shore Dr & Monroe St"
  }
}

for ( i in 1:nrow(start_data)){
  if (start_data[i,3]== "Lake Shore Dr & Ohio St"){
    start_data[i,3]="Dusable Lake Shore Dr & Ohio St"
  }
}

for ( i in 1:nrow(start_data)){
  if (start_data[i,3]== "Lake Shore Dr & North Blvd"){
    start_data[i,3]="DuSable Lake Shore Dr & North Blvd"
  }
}

for ( i in 1:nrow(start_data)){
  if (start_data[i,3]== "Lake Shore Dr & Wellington Ave"){
    start_data[i,3]="Dusable Lake Shore Dr & Wellington Ave"
  }
}


for ( i in 1:nrow(start_data)){
  if (start_data[i,3]== "Lake Shore Dr & Belmont Ave"){
    start_data[i,3]="DuSable Lake Shore Dr & Belmont Ave"
  }
}

start_data <- data.frame(start_data)
start_data<- rbind(start_data,start_no_data)


###########start database created########

##Replacing missing station names in end station
end_no_data <- clean_data%>% filter(end_station_name=="")%>% group_by(end_lat,end_lng,end_station_name,member_casual)%>% summarise(n=n())
end_no_data<- na.omit(end_no_data)  

#write.csv(end_no_data,"end_no_data.csv",row.names = FALSE)
# filtering top data cordinates and assigning start station name 
#filtering out in excel top 25 coordinates and finding their station name from our database
end_no_data<- read.csv("end_no_data.csv")

##Changing Station names (Some station names has been changed over the time)
end_data_unclean <- clean_data%>% filter(end_station_name!="")%>% group_by(end_lat,end_lng,end_station_name,member_casual)%>% summarise(n=n())


#write.csv(end_data_unclean,"end_data_unclean.csv",row.names = FALSE)

# changing the names of 6 stations to their new names in excel 

end_data_unclean<- read.csv("end_data_unclean.csv")

end_data_unclean <- data.frame(end_data_unclean)
end_data<- rbind(end_data_unclean,end_no_data)

##Stack clean data into one data frame
final_data_clean <- bind_rows(clean_data, end_data, start_data)

##Export csv for further analysis 
write.csv(final_data_clean, "Cyclistic_Final.csv")

###########end database created########

##Analyze
#Key tasks
#Aggregate your data so it’s useful and accessible.
#Organize and format your data.
#Perform calculations.
#Identify trends and relationships.
library(ggplot2)

clean_data%>% group_by(member_casual)%>% summarise(n=n())%>%
  mutate(percent = n*100/sum(n))

ggplot(data = clean_data,mapping= aes(x= member_casual))+geom_bar() + labs(title="# of Casual Riders v # of Members", x="Type of Rider", y= "Count of Riders")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

cat('average duration of rides is : ', mean(clean_data$ride_length), "mins")
#average duration of rides is :  21.17667 mins

cat('maximum duration of rides is : ', round(max(clean_data$ride_length)/60/24), "days")
#maximum duration of rides is :  39 days

##Ride behavior of members and casual riders
bar<-clean_data%>% group_by(member_casual)%>% summarise(avg_ride_length=mean(ride_length))
bar

ggplot(bar, aes(x = member_casual, y = avg_ride_length, fill =member_casual))+geom_col() +geom_bar(stat="identity")+ labs(title="Avg Ride Duration in Mins Per Usertype", x="Type of Rider", y= "Avg Ride Duration in Mins", fill= "User Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))
## # A tibble: 2 x 2
##   member_casual avg_ride_length
##   <chr>         <drtn>         
## 1 casual        28.49893 mins  
## 2 member        13.06106 mins

#Comparing bikes
clean_data%>% group_by(rideable_type) %>% summarise(n=n())%>% mutate(percent = n*100/sum(n))
## # A tibble: 3 x 3
##   rideable_type       n percent
##   <chr>           <int>   <dbl>
## 1 classic_bike  2435347   56.5 
## 2 docked_bike    216447    5.02
## 3 electric_bike 1659439   38.5

ggplot(data = clean_data,mapping= aes(x= rideable_type,fill=rideable_type))+geom_bar() + labs(title="Biketype Use: Comparison", x="Type of Bike", y= "Count of Bikes", fill= "Bike Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

#Classic Bike is more preferred than Electric Bike.
#Docked Bike is the least preferred Bike.


#Bike Rides percent with riders analysis
rideable_type<-clean_data%>% group_by(rideable_type,member_casual) %>% summarise(n=n())%>% 
  mutate(percent = n*100/sum(n))
rideable_type
## # A tibble: 5 x 4
## # Groups:   rideable_type [3]
##   rideable_type member_casual       n percent
##   <chr>         <chr>           <int>   <dbl>
## 1 classic_bike  casual         952630    39.1
## 2 classic_bike  member        1482717    60.9
## 3 docked_bike   casual         216447   100  
## 4 electric_bike casual         753149    45.4
## 5 electric_bike member         906290    54.6

ggplot(data = as.data.frame(rideable_type),mapping= aes(x= rideable_type, y = n, fill =member_casual))+geom_bar(stat = 'identity') + labs(title="Biketype & User Type", x="Type of Bike", y= "Count of Bikes", fill= "User Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

##Choice of Bikes by Riders
member_type<-clean_data%>% group_by(member_casual,rideable_type) %>% summarise(n=n())%>%
  mutate(percent = n*100/sum(n))

member_type
## # A tibble: 5 x 4
## # Groups:   member_casual [2]
##   member_casual rideable_type       n percent
##   <chr>         <chr>           <int>   <dbl>
## 1 casual        classic_bike   952630    49.6
## 2 casual        docked_bike    216447    11.3
## 3 casual        electric_bike  753149    39.2
## 4 member        classic_bike  1482717    62.1
## 5 member        electric_bike  906290    37.9

ggplot(data = as.data.frame(member_type),mapping= aes(x= member_casual, y=n, fill =rideable_type))+geom_bar(stat = 'identity') + labs(title="User Biketype Preference", x="Type of User", y= "Count of Bikes", fill= "Bike Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

#Classic Bike is more preferred by members.
#Whereas Electric bike is almost equally preferred by both riders.
#Members don’t use Docked Bike.

##Weekday Rides (usage on different days)
clean_data$weekday<- factor(clean_data$weekday, levels= c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
weektable<- clean_data%>% group_by(weekday)%>% summarise(n=n())%>% mutate(percent = n*100/sum(n))

weektable
ggplot(data = clean_data,mapping= aes(x= weekday)) +geom_bar() + labs(title="Daily Total: Bike Use", x="Day of the Week", y= "Count of Bikes")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

# To deactivate scientific notation on y-axis:

p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# To activate scientific notation on y-axis:

p + scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

# To deactivate scientific notation on x-axis:

p + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

# To activate scientific notation on x-axis:

p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

# Here we define spaces as the big separator
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

# Plot it
p  <- ggplot(data = df, aes(x=x, y=y)) + geom_line() + geom_point()
p + scale_x_continuous(labels = point)


###Daily rides of members and casual riders
ggplot(data = clean_data,mapping= aes(x= weekday, fill = member_casual)) +geom_bar() +facet_wrap(~member_casual)+theme(axis.text.x = element_text(angle = 60, hjust =1)) +labs(title="Daily Rides of Casual v. Member", x="Day of the Week", y= "Count of Rides", fill= "User Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

#Member’s usage is quite similar throughout the week except for Sunday. We can infer that members are mostly working people.
#Casual Riders preferred to ride on weekends, especially Saturdays.

##Bikes Usage Pattern By Casual Riders
casual_riders<-clean_data%>% filter(member_casual == 'casual')%>%group_by(weekday,rideable_type)%>% summarise(n=n())%>% mutate(percent = n*100/sum(n))

ggplot(data= casual_riders, mapping= aes(x= weekday, y=n, fill =rideable_type)) + geom_bar(stat = 'identity') +labs(title="Casual Rider: Biketype Weekly Use", x="Day of the Week", y= "Count of Rides", fill= "Bike Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

##Bikes Usage Pattern By Members
members<-clean_data%>% filter(member_casual == 'member')%>%group_by(weekday,rideable_type)%>% summarise(n=n())%>% mutate(percent = n*100/sum(n)) 

ggplot(data= members, mapping= aes(x= weekday, y=n, fill =rideable_type)) + geom_bar(stat = 'identity')+ labs(title="Member: Biketype Weekly Use", x="Day of the Week", y= "Count of Rides", fill= "Bike Type")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark = ",", decimal.mark = "."))

install.packages("tinytex")
rmarkdown::render("Capstone 2.R")

xfun::broken_packages(reinstall = TRUE)
library(rmarkdown)

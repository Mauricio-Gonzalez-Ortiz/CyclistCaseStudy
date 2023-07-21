install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)

cyclist<-read_csv("cyclist_case_study.csv")
skim_without_charts(cyclist)

cyclist<-drop_na(cyclist[!duplicated(cyclist),])
skim_without_charts(cyclist)

colnames(cyclist)

cyclist_wd<-cyclist %>% arrange(started_at,ended_at) 
select(cyclist_wd[c(106,107),],!ride_id)

cyclist_clean<-cyclist[!duplicated(cyclist[c("started_at","ended_at")]),]
skim_without_charts(cyclist_clean)
View(arrange(cyclist_clean,started_at,ended_at))


filtered_cyclist<-cyclist_clean %>% filter(nchar(ride_id)==16)
skim_without_charts(filtered_cyclist)

cyclist_data<-filtered_cyclist %>% select(!ride_id) %>% select(!rideable_type) %>% 
  mutate(total_distance_km=ifelse(start_lat==end_lat&start_lng==end_lng,
                               0,distance(start_lat,start_lng,end_lat,end_lng))) %>% 
  select(!start_lat) %>% select(!end_lat) %>% select(!end_lng) %>% 
  select(!start_lng) %>% 
  mutate(started_at=mdy_hm(started_at),ended_at=mdy_hm(ended_at)) %>% 
  mutate(total_time_minutes=as.numeric(ended_at-started_at)/60)
skim_without_charts(cyclist_data)


member_summary<-cyclist_data %>% group_by(member_casual) %>% 
  summarise(
    average_distance=mean(total_distance_km),
    max_distance=max(total_distance_km),
    average_time=mean(total_time_minutes),
    max_time=max(total_time_minutes),
    min_time=min(total_time_minutes),
    most_used_station_start=mode(start_station_name),
    most_used_station_end=mode(end_station_name)) 
View(member_summary)

cyclist_data %>%  filter(total_time_minutes<=0) %>% View()

cyclist_data<- cyclist_data %>% filter(total_time_minutes>0) 
skim_without_charts(cyclist_data)

member_summary<-cyclist_data%>% group_by(member_casual) %>% 
  summarise(
    average_distance=mean(total_distance_km),
    max_distance=max(total_distance_km),
    average_time=mean(total_time_minutes),
    max_time=max(total_time_minutes),
    min_time=min(total_time_minutes),
    most_used_station_start=mode(start_station_name),
    most_used_station_end=mode(end_station_name)) 
View(member_summary)

ggplot(cyclist_data)+geom_bar(aes(x=(total_distance_km==0),fill=member_casual))+
  scale_fill_discrete(name="Types of rider",labels=c("Casuals", "Members"))+
  scale_x_discrete(name="Types of trips",labels=c("Single Trips", "Round Trips"))+
  ylab("Count")+labs(title="Cyclist distribution of trips",
                     subtitle = "Sample of two types of riders")+theme_minimal()+
  theme(plot.title = element_text(size=20))

ggplot(cyclist_data)+geom_smooth(aes(y=total_time_minutes,x=total_distance_km,
                                     color=member_casual,fill=member_casual))+
  xlab("Distance (km)")+ylab("Time (minutes)")+theme_minimal()+
  labs(title="Distance covered vs time spend", 
       subtitle="Sample of two types of riders")+
  theme(plot.title = element_text(size=20))+
  scale_fill_discrete(name="Types of rider",labels=c("Casuals", "Members"))+
  scale_color_discrete(name="Types of rider",labels=c("Casuals", "Members"))+
  geom_segment(aes(x = 0, y = 525, xend = 19, yend = 525),linetype="dashed")+
  geom_segment(aes(x = 10, y = 1000, xend = 11.8, yend = 525),arrow=
                 arrow(length = unit(0.3,"cm")))+ 
  annotate("text",x=7,y=1100,label="Time break between types of rider",
           fontface="bold",angle=30)

ggplot(cyclist_data,aes(x=total_distance_km,fill=member_casual))+
  
  geom_histogram(aes(y=..density..),col="black")+
  geom_density(alpha=0,lwd=0.5)+theme_minimal()+ylab("Density")+
  facet_wrap(~member_casual)+xlab("Total distance (km)")+
  labs(title='Distribution of distance cover by ride', 
       subtitle="Sample of two types of riders")+
  theme(legend.position = "none",plot.title = element_text(size=20))

ggplot(cyclist_data)+geom_bar(aes(x=start_station_id))+facet_wrap(~member_casual)
ggplot(cyclist_data)+geom_bar(aes(x=end_station_id))+facet_wrap(~member_casual)

sorted_start<-cyclist_data %>% select(start_station_name,member_casual) %>%
  filter(member_casual=='casual') %>% add_count(start_station_name) %>% 
  filter(n %in% tail(sort(unique(n)),5)) %>% 
  arrange(desc(n)) 
sorted_end<-cyclist_data %>% select(end_station_name,member_casual) %>%
  filter(member_casual=='casual')%>% add_count(end_station_name) %>% 
  filter(n %in% tail(sort(unique(n)),5)) %>% 
  arrange(desc(n)) 

cyclist_data %>% filter(start_station_name %in% as.matrix(
                          sorted_start[!duplicated(sorted_start$start_station_name),1])&
                          member_casual=='casual') %>% 
  ggplot(aes(x=start_station_name)) +labs(title="Most popular stations for casual riders",
                                          subtitle="Stations where they start their trip")+
  geom_bar(fill=2)+xlab("Name of the station")+ylab("Count")+theme_minimal()+
  theme(axis.text.x = element_text(angle=10),plot.title = element_text(size=20))
  
cyclist_data %>% filter(end_station_name %in% as.matrix(
  sorted_end[!duplicated(sorted_end$end_station_name),1])&
    member_casual=='casual') %>% 
  ggplot(aes(x=end_station_name)) +labs(title="Most popular stations for casual riders",
                                          subtitle="Stations where they end their trip")+
  geom_bar(fill=2)+xlab("Name of the station")+ylab("Count")+theme_minimal()+
  theme(axis.text.x = element_text(angle=10),plot.title = element_text(size=20))


cosd<-function(a){
  b=cos(a*pi/180)
  return(b)
}
sind<-function(a){
  b=sin(a*pi/180)
  return(b)
}
distance<-function(la1,lo1,la2,lo2){
  a=acos(sind(la1)*sind(la2)+cosd(la1)*cosd(la2)*cosd(lo2-lo1))*6371
  return(a)
}
mode <- function(x) {
  return(names(which.max(table(x))))
}
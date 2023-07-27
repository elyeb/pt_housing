# Script to produce graphics on ADU data for HSN
# Author: Elye Bliss
# Jul 26, 2023

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(sf)
library(mapview)
library(mapshot)


setwd('~/Desktop/Housing/HSN/')

data <- read.csv('Combined_ADU_Permits.csv',header = T,stringsAsFactors = F)

########################## Clean data ##########################

colnames(data)
names(data)[names(data)=="Permit.." ] <- "Permit_no"
names(data)[names(data)=="Project.Description" ] <- "Project_Description"
names(data)[names(data)=="Site.Address" ] <- "Site_Address"
data$Submitted <- as.Date(data$Submitted, format = "%Y-%m-%d")
data$Approved <- as.Date(data$Approved, format = "%Y-%m-%d")
data$Closed <- as.Date(data$Closed, format = "%Y-%m-%d")
data$year_submitted <- format(data$Submitted,"%Y")

############### Make bar chart comparing submitted vs completed #############
submit_vs_complete <- data %>% 
  group_by(year_submitted) %>%
  summarize(total_submitted = length(Submitted),
            total_complete = length(Status[Status=='COMPLETED'|Status=="FINALED"])) %>%
  ungroup() %>%
  gather(key = type,
         value = total,
         -year_submitted)

submit_vs_complete$type[submit_vs_complete$type=='total_submitted'] <- 'Submitted'
submit_vs_complete$type[submit_vs_complete$type=='total_complete'] <- 'Completed'

submit_v_approve <- ggplot(submit_vs_complete,aes(x=year_submitted,y=total,fill=type))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)+
  theme_minimal()+
  theme(axis.text.x= element_text(angle=45,hjust=1))+
  scale_x_discrete(breaks=seq(1990,2025,by=3))+
  labs(title="Total Number of ADU Applications Submitted and Approved per Year",
       x=NULL,
       y=NULL)+
  theme(legend.title= element_blank())
ggsave('submitted_vs_approved.png',submit_v_approve,bg="white")

# Find average time to approval, group by year
approved <- data %>%
  filter(!is.na(Approved)) %>%
  mutate(time_to_approval = Approved-Submitted) %>%
  filter(time_to_approval>=0) %>%
  group_by(year_submitted) %>%
  summarize(avg_time_to_approval = mean(time_to_approval)) %>%
  ungroup()

avg_time <- ggplot(approved, aes(x=year_submitted,y=avg_time_to_approval))+
  geom_col(aes(group = 1,fill='red'), alpha = 0.75)+
  theme_minimal()+
  theme(axis.text.x= element_text(angle=45,hjust=1))+
  scale_x_discrete(breaks=seq(1990,2025,by=3))+
  labs(title="Average No. Days for Permits to be Approved",
       x=NULL,
       y="No. days")+
  theme(legend.position="none")
ggsave('avg_time_approved.png',avg_time,bg="white")

########################## map data for ADUs ##########################


#Examples of messy addresses:
# 411, 415, 423, 429 MONROE ST
# 1235/1239 S STREET
# 251 47TH ST & 4673 MCN..
# 511 ROOT ST (SFR) & 515 
# 3001(SFR) & 3023(ADU) AL

# Derived rules: 
# cut off anything after a "ST", "STREET","AVE"
# filter with any of the follow
# Split by "ACCESSORY TO", "&", "-"


clean_addresses <- function(x){
  if(grepl("&|\\(|ACCESSORY TO|\\-|\\,",x)){
    return(NA)
  }else{
    return(x)
  }
}

data$Address_Clean <- sapply(data$Site_Address,FUN=clean_addresses)

update_manually <- data %>% 
  filter(is.na(Address_Clean))

write.csv(update_manually,"update_manually.csv",row.names = F)
updated_manually <- read.csv("updated_manually.csv",header = T,stringsAsFactors = F)

data$Address_Clean[is.na(data$Address_Clean)] <- updated_manually$Address_Clean[match(data$Site_Address[is.na(data$Address_Clean)],updated_manually$Site_Address)]

data$location <- paste0(data$Address_Clean,
                        ", ", "Port Townsend",
                        ", WA ", 98368)



# geocode the addresses
lat_longs <- data %>%
  geocode(location, method = 'osm', lat = latitude , long = longitude)

# 20 missing values
lat_longs <- lat_longs%>%
  filter(!is.na(latitude))

# Save file with coordinate data
write.csv(lat_longs,'data_with_coords.csv',row.names = F)

# test mapview
all_submitted_coord <- lat_longs %>%
  select(-c(Site_Address,Address_Clean))

all_submitted <- mapview(all_submitted_coord, xcol = "longitude", ycol = "latitude", crs = 4269, grid = F,
        layer.name = c("Sites submitted")) #works!

all_completed_coord <- lat_longs %>%
  filter(Status=='COMPLETED'|Status=='FINALED')%>%
  select(-c(Site_Address,Address_Clean))

all_completed <- mapview(all_completed_coord, xcol = "longitude", ycol = "latitude", crs = 4269, grid = F,
                         layer.name = c("Sites Completed")) 

# More exploring just to see differences between decades
all_90s <- all_completed_coord %>%
  filter(year_submitted<2000)
mapview(all_90s, xcol = "longitude", ycol = "latitude", crs = 4269, grid = F,
        layer.name = c("Sites Completed"))
all_00s <- all_completed_coord %>%
  filter(year_submitted>=2000&year_submitted<2010)
mapview(all_00s, xcol = "longitude", ycol = "latitude", crs = 4269, grid = F,
        layer.name = c("Sites Completed"))
all_10s<- all_completed_coord %>%
  filter(year_submitted>=2010&year_submitted<2020)
mapview(all_10s, xcol = "longitude", ycol = "latitude", crs = 4269, grid = F,
        layer.name = c("Sites Completed"))

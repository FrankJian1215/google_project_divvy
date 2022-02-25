## Scenario
###You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

## SIX PHASE OF DATA ANALYSIS

- Ask
- Prepare
- Process
- Analyze
- Share
- Act


## ASK

# - What is the problem I am trying to solve?
#         - How can your insights drive business decisions?
#         - What steps have you taken to ensure that your data is clean?
#         - What trends or relationships did you find in the data?


## PREPARE

library("dplyr")
library("stringr")
library("data.table")
library("hablar")
library("lubridate")
library("ggplot2")
library("assertive")
library("visdat")
library("geosphere")
library("naniar")
library("EnvStats")
library("rstatix")
library("ggrepel")
library("stringr")
library("ggmap")
library("skimr")
library("knitr")


getwd()

#Set working directory

setwd("/Users/jianfrank/google_analytics_capstone/raw data/csv_file")

#Import files
# Find related files ended with "data.*csv", saved it to `myFiles`.
# Inside `grand_data`, we first use lapply() & fread() to read every list of `myFiles` and then use do.call() to bind
# all the rows together and save it to `grand_data`.

myFiles <- list.files(pattern="data.*csv")
grand_data <- do.call(rbind, lapply(myFiles, fread, na.strings = c("", "NA")))
station_info <- read.csv("Divvy_Bicycle_Stations.csv")


#Check data structures, variable, variable definition, records and datatype, 

str(grand_data)
str(station_info)


#Transform the data adding multiple columns for easier analysis
level <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
level_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
divvy_data <- grand_data %>%
        mutate(
                rideable_type = as.factor(rideable_type),
                started_at = as_datetime(started_at),
                ended_at =as_datetime(ended_at),
                member_casual = as.factor(member_casual),
                year = format(started_at, "%Y"),
                month = format(started_at, "%b"), 
                week = format(started_at, "%U"), 
                weekday = format(started_at, "%A"),
                hour = format(started_at, "%H"),
                year = factor(year),
                month = factor(month, levels = level),
                week = as.character(week), 
                weekday = factor(weekday, level = level_week),
                hour_end = format(ended_at, "%H"),
                used_time = round(as.numeric(started_at %--% ended_at, "minutes"),2), 
                distance = distHaversine(cbind(start_lng, start_lat),
                                         cbind(end_lng, end_lat))
        )

## PROCESS


#Filter out used time is below zero

divvy_data_clean <- divvy_data %>% 
        filter(used_time > 0)  


#Check if there's any missing values

divvy_data_clean %>% 
  bind_shadow() %>% 
  group_by(rideable_type, start_station_name_NA, start_station_id_NA, end_station_name_NA, end_station_id_NA, end_lat_NA, end_lng_NA) %>% 
  summarise(n())


#Check if there's any missing values given on different scenarios. We can break it down to 8 scenarios then can apply coping strategy to each of them.

# rideable_type: electric_bike, start_station_name: NA, start_station_id: NA(254320)-add "E_station" to start_station_name

divvy_data_clean <- divvy_data_clean %>% 
        mutate(start_station_name = ifelse(rideable_type == "electric_bike" &
                                                   !is.na(end_station_name) &
                                                   !is.na(end_station_id) &
                                                   is.na(start_station_id) &
                                                   is.na(start_station_name) &
                                                   !is.na(end_lat) &
                                                   !is.na(end_lng), "E_station", start_station_name))


# rideable_type: electric_bike, end_station_name: NA, end_station_id: NA(292335)- add "E_station" to end_station_name
divvy_data_clean <- divvy_data_clean %>% 
        mutate(end_station_name = ifelse(
                rideable_type == "electric_bike" &
                        is.na(end_station_name) &
                        is.na(end_station_id) &
                        !is.na(start_station_name) &
                        !is.na(start_station_id) &
                        !is.na(end_lat) &
                        !is.na(end_lng) &
                        !is.na(start_lat) &
                        !is.na(start_lng), "E_station", end_station_name))

# rideable_type: electric_bike, start_station_name: NA, start_station_id: NA, end_station_name: NA, end_station_id: NA(397056)-add "E_station" to start_station_name and end_station_name
divvy_data_clean <- divvy_data_clean %>% 
        mutate(end_station_name = ifelse(rideable_type == "electric_bike" &
                                                 is.na(end_station_name) &
                                                 is.na(start_station_name) &
                                                 is.na(start_station_id) &
                                                 !is.na(end_lat) &
                                                 !is.na(end_lng), "E_station", end_station_name),
               start_station_name = ifelse(rideable_type == "electric_bike" &
                                                   is.na(start_station_id) &
                                                   is.na(start_station_name) &
                                                   is.na(end_station_id) &
                                                   !is.na(end_lat) &
                                                   !is.na(end_lng), "E_station", start_station_name))



#After going through above processes, missing values from electric bikes have been cleaned to a point. Only thing you need to do is impute start_station_name column with value from `station_select` full joining with `divvy_data_clean` by longitude and latitude. If it still contains NA then filter out from the dataset.

# rideable_type: electric_bike, start_station_name: NA(3)-add "E_station" to start_station_name and end_station_name

station_select <- station_info %>% 
        select(ID, Station.Name, Latitude, Longitude) %>% 
        mutate(ID = as.factor(ID))
glimpse(station_select)

divvy_data_clean <- divvy_data_clean %>% 
        full_join(station_select, by = c("start_lat" = "Latitude", "start_lng" = "Longitude")) %>% 
        mutate(start_station_name = coalesce(start_station_name, Station.Name)) %>% 
        select(-ID, -Station.Name) %>% 
        filter(!is.na(ride_id))

divvy_data_clean <- divvy_data_clean %>% 
        filter(!is.na(start_station_name), !is.na(start_station_id))


# rideable_type: classic_bike, end_station_name: NA, end_station_id: NA(4299)-add "E_station" to end_station_name

divvy_data_clean <- divvy_data_clean %>% 
        mutate(end_station_name = ifelse(rideable_type == "classic_bike" &
                                                 is.na(end_station_name) &
                                                 !is.na(start_station_name) &
                                                 !is.na(start_station_id) &
                                                 !is.na(end_lat) &
                                                 !is.na(end_lng) &
                                                 !is.na(start_lat) &
                                                 !is.na(start_lng), "E_station", end_station_name))


# rideable_type: classic_bike, end_station_name: NA, end_station_id: NA, end_lat: NA, end_lng:NA(4460)-filter out
# rideable_type: docked_bike, end_station_name: NA, end_station_id: NA, end_lat: NA, end_lng:NA(4460)-filter out

divvy_data_clean <- divvy_data_clean %>% 
        filter(!is.na(end_lat),
               !is.na(end_lng))


# add "E01" to start_station_id & end_station_id
divvy_data_clean <- divvy_data_clean %>% 
        mutate(start_station_id = ifelse(is.na(start_station_id), "E01", start_station_id),
               end_station_id = ifelse(is.na(end_station_id), "E01", end_station_id))


# add "E_station" to start_station_name if `is.na(start_station_name)` is true.
# add "E_station" to end_station_name if `is.na(end_station_name)` is true.

divvy_data_clean <- divvy_data_clean %>% 
        mutate(start_station_name = ifelse(is.na(start_station_name), "E_station", start_station_name),
               end_station_name = ifelse(is.na(end_station_name), "E_station", end_station_name))

#Check if there is any missing values
gg_miss_var(divvy_data_clean)

## ANALYZE/SHARE
#Once the dataset is all set for analyzing, we first look at what does the dataset look like.


str(divvy_data_clean)
head(divvy_data_clean)
dim(divvy_data_clean)

#Then we can look at how used time and number of usage differ in group of types of bike and membership.
#As we see the result, customers who are casual user and uses the docked bike have the most spread_out time on the bike. 
#Standard deviation of casual users riding docked bikes is more than 30 times larger than those who are member users riding docked bikes. 
#We can deep-dive later. But let's see the median used time cross membership and the median used time cross type of bikes and membership first.
divvy_data_clean %>% 
        group_by(rideable_type, member_casual) %>% 
        summarise(sd_used_time = sd(used_time),
                  mean_used_time = mean(used_time),
                  median_used_time = median(used_time),
                  maximum_used_time = max(used_time), 
                  minimum_used_time = min(used_time), 
                  interquartile_used_time = iqr(used_time),
                  count = n()) %>% 
        ungroup() %>% 



#Median for member riders and casual riders
used_time_1 <- divvy_data_clean %>% 
  group_by(member_casual) %>% 
  summarise(median = median(used_time))
used_time_mb <- used_time_1 %>% 
  filter(member_casual == "member") %>% 
  pull(median)
used_time_ca <- used_time_1 %>% 
  filter(member_casual == "casual") %>% 
  pull(median)
ggplot(divvy_data_clean, aes(x=used_time, fill= member_casual))+
  geom_histogram(alpha=0.3, size=1.5, bins = 40)+
  scale_x_continuous(limits = c(0,160))+
  coord_cartesian(xlim = c(0,50))+
  geom_vline(data=used_time_1, aes(xintercept = median, color = member_casual),size=1.2, linetype = "dashed")+
  annotate(x=used_time_mb, y=1000000, label = paste("Member Median = ", used_time_mb),geom="label",size=3, fontface="bold")+
  annotate(x=used_time_ca, y=800000, label = paste("Casual Median = ", used_time_ca),geom="label",size=3, fontface="bold")





# Median used time cross type of bikes and membership
# We can see for member users no matter what type of bike they have use they have pretty much the same median around 9. 
# However, for casual users they have larger median used time especially in docked_bike category which has been used mostly by casual riders. 

used_time_1 <- divvy_data_clean %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(median = median(used_time))

ggplot(divvy_data_clean, aes(x=used_time, fill= member_casual))+
  geom_histogram(alpha=0.3, size=1.5, bins = 40)+
  scale_x_continuous(limits = c(0,160))+
  coord_cartesian(xlim = c(0,50))+
  geom_vline(data=used_time_1, aes(xintercept = median, color = member_casual),size=1.2, linetype = "dashed")+
  geom_text_repel(
          data = used_time_1,
          aes(x = median, 
              y = 750000,
              label = paste(member_casual, "median \n", median)), 
            size = 3, 
            fontface = "bold"
  )+
  facet_grid(rideable_type~.)


#Use graph to see the median of different group
divvy_data_clean %>% 
        group_by(rideable_type, member_casual) %>% 
        summarise(sd_used_time = sd(used_time),
                  mean_used_time = mean(used_time),
                  median_used_time = median(used_time),
                  maximum_used_time = max(used_time), 
                  minimum_used_time = min(used_time), 
                  interquartile_used_time = iqr(used_time),
                  count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%"))

third_quartile <- quantile(divvy_data_clean$used_time, 0.75) %>% unname()
first_quartile <- quantile(divvy_data_clean$used_time, 0.25) %>% unname()
iqr <- IQR(divvy_data_clean$used_time)
lower <- first_quartile - 1.5*iqr
higher <- first_quartile + 1.5*iqr

divvy_outlier <- divvy_data_clean %>% 
        filter(used_time < lower | used_time > higher)        

divvy_data_clean %>% 
        ggplot(aes(x = member_casual, y = used_time))+
        geom_boxplot()+
        geom_point(divvy_outlier, mapping = aes(x = member_casual, y = used_time), color = "red", alpha = 0.5)+
        geom_jitter(aes(color = rideable_type), alpha = 0.5)




#We just found that time casual users on a docked bike is the most spread out among all categories. 
#What I mean by spread out is a few days away from time since they first used it. 
#It's highly unlikely that users are on a bike all day long. We probably can assume there might be something wrong 
#with casual users returning their bikes. 
#(follow-up: used time outlier of end time frequency in month & weekday)

divvy_doc_cas <- divvy_data_clean %>% 
        filter(rideable_type == "docked_bike", member_casual == "casual")
out <- boxplot.stats(divvy_doc_cas$used_time)$out

divvy_doc_cas %>% 
        filter(used_time %in% out) %>%
        count()

divvy_doc_cas %>% 
        filter(used_time %in% out) %>% 
        group_by(start_station_name, end_station_name) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count))

divvy_doc_cas %>% 
        filter(used_time %in% out) %>% 
        distinct(end_station_name)




#We move our focus to the number of rides between casual and member users. 
#For number of rides itself, member riders use divvy more than casual user by 10%.

divvy_data_clean %>% 
        group_by(member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = round(count/sum(count),2)) %>% 
        ggplot(aes(x = member_casual, y = percent))+
        geom_bar(stat = "identity", position = "stack")+
        geom_text(aes(y = percent/2, label = paste0(percent*100, "%")), color = "yellow", fontface = "bold",size = 4.5)+
        scale_y_continuous(labels = scales::percent)




# Next, let's see what the numbers look like if we delve it in on monthly basis.
divvy_data_clean %>% 
        filter(member_casual == "member") %>% 
        group_by(month) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(count))
divvy_data_clean %>% 
        filter(member_casual == "casual") %>% 
        group_by(month) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(count))


#Let's plot it on the graph to see differences in number of rides between member riders and casual riders. 
#Overall, there's a noticeable trends that it climbed from January, peaked in July/August and declined ever since to the end of year. 
#But you can tell from the plot that `r params$casual` riders surpass `r params$member` riders in numbers of rides from June to August.
divvy_data_clean %>% 
        group_by(month, member_casual) %>% 
        summarise(count = n())  %>% 
        ggplot(aes(x = month, y = count, group = member_casual, color = member_casual))+
        geom_line()


#Table of number of rides cross type of bikes and membership over month

count_m <- divvy_data_clean %>% 
        group_by(month, member_casual, rideable_type) %>% 
        summarise(count = n()) %>% 
        spread(month, count)
kable(count_m)



# Table of median used time cross type of bikes and membership over month

median <- divvy_data_clean %>% 
        group_by(month, member_casual, rideable_type) %>% 
        summarise(median = median(used_time)) %>% 
        spread(month, median)
kable(median)




#Plot it on the graph. But if you dive deeper, you will notice that most of the gap is actually 
#coming from docked bike category.


divvy_data_clean %>% 
        group_by(month, member_casual, rideable_type) %>% 
        summarise(count = n()) %>% 
        mutate(label = ifelse(month == "Dec", as.character(member_casual), NA_character_)) %>% 
        ggplot(aes(x = month, y = count, group = member_casual, color = member_casual))+
        geom_line()+
        geom_text_repel(aes(label = label, x = "Dec", y = count), na.rm = T)+
        facet_grid(rideable_type~., scales = "free")+
        theme(legend.position = "none")
        


#Given on median used time for different category, it's likely who used docked bike buy day pass service 
#which provides unlimited 3-hour ride a day. For those we use docked bike, it might be ahrd to tell what 
#they are using the bike for, but one thing can be for sure is they are coming from different group of people.

divvy_data_clean %>% 
        group_by(member_casual, month, rideable_type) %>% 
        summarise(median_used_time = median(used_time)) %>% 
        mutate(label = ifelse(month == "Dec", as.character(member_casual), NA_character_)) %>% 
        ggplot(aes(month, median_used_time, group = member_casual, color = member_casual))+
        geom_line()+
        geom_text(aes(label = label, x = "Dec", y = median_used_time), na.rm = TRUE) +
        facet_grid(rideable_type~., scales = "free")+
        theme(legend.position = "none")



# We dive in to know how have the bikes been used by member and casual riders in a day order by number of times they have been used from highest to lowest.

divvy_data_clean %>% 
        filter(member_casual == "member") %>% 
        group_by(hour, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(percent))
divvy_data_clean %>% 
        filter(member_casual == "casual") %>% 
        group_by(hour, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(percent))

# We dive in to know how have the bikes been used by member and casual riders in a week ordered by number 
# of times they have been used from highest to lowest.
divvy_data_clean %>% 
        filter(member_casual == "member") %>% 
        group_by(weekday, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(percent))
divvy_data_clean %>% 
        filter(member_casual == "casual") %>% 
        group_by(weekday, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(percent))

#Let's plot it.
#From the result below, you can tell `r params$member`user have consistent number of usages each day in a week. 
#However, `r params$casual`user have remarkable number of usages on weekends(Saturday & Sunday).
divvy_data_clean %>%
        group_by(weekday, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        ggplot(aes(weekday, count, group = member_casual, color = member_casual))+
        geom_line()

divvy_data_clean %>%
  group_by(weekday, member_casual, rideable_type) %>% 
  summarise(count = n()) %>% 
  mutate(label = ifelse(weekday == "Sunday", as.character(member_casual), NA_character_)) %>% 
  ggplot(aes(weekday, count, group = member_casual, color = member_casual))+
        geom_line()+
        geom_text(aes(label = label, x = "Sunday", y = count), na.rm = TRUE) +
        facet_grid(rideable_type~., scales = "free")+
        theme(legend.position = "none")

divvy_data_clean %>%
  group_by(weekday, member_casual, rideable_type) %>% 
  summarise(median = median(used_time)) %>% 
  mutate(label = ifelse(weekday == "Sunday", as.character(member_casual), NA_character_)) %>% 
  ggplot(aes(weekday, median, group = member_casual, color = member_casual))+
        geom_line()+
        geom_text(aes(label = label, x = "Sunday", y = median), na.rm = TRUE) +
        facet_grid(rideable_type~., scales = "free")+
        theme(legend.position = "none")



#We dive in to know how have the bikes been used by member and casual riders in a day order by number of times 
#they have been used from highest to lowest.

divvy_data_clean %>% 
        filter(member_casual == "member") %>% 
        group_by(hour, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(percent))
divvy_data_clean %>% 
        filter(member_casual == "casual") %>% 
        group_by(hour, member_casual) %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate(percent = paste0(round(count/sum(count),2)*100,"%")) %>% 
        arrange(desc(percent))

divvy_data_clean %>%
  group_by(hour, member_casual, rideable_type) %>% 
  summarise(count = n()) %>% 
  mutate(label = ifelse(hour == 23, as.character(member_casual), NA_character_)) %>% 
  ggplot(aes(hour, count, group = member_casual, color = member_casual))+
        geom_line()+
        geom_text(aes(label = label, x = 23, y = count), na.rm = TRUE) +
        facet_grid(rideable_type~., scales = "free")+
        theme(legend.position = "none")

divvy_data_clean %>%
  group_by(hour, member_casual, rideable_type) %>% 
  summarise(median = median(used_time)) %>% 
  mutate(label = ifelse(hour == 23, as.character(member_casual), NA_character_)) %>% 
  ggplot(aes(hour, median, group = member_casual, color = member_casual))+
        geom_line()+
        geom_text(aes(label = label, x = 23, y = median), na.rm = TRUE) +
        facet_grid(rideable_type~., scales = "free")+
        theme(legend.position = "none")





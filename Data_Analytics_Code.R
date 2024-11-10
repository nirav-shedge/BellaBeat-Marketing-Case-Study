library(tidyverse)
library(hexbin)
library(dplyr)
library(cowplot)
library(ggplot2)
library(readxl)
library(purrr)
library(lubridate)


#First Step is to set up working directory so that importing the dataset becomes 
#easier. 


#STEP 1: ################# Importing the dataset ############################### 
daily_activity<-read.csv("dailyActivity_merged.csv")
attach(daily_activity)
hourly_intensities<-read.csv("hourlyIntensities_merged.csv")
attach(hourly_intensities)
sleep_day<-read.csv("sleepDay_merged.csv")
attach(sleep_day)
weightinfo<-read.csv("weightLogInfo_merged.csv")
attach(weightinfo)
view(weightinfo)
hourly_steps<-read.csv("hourlySteps_merged.csv")
view(hourly_steps)
daily_calories<-read.csv("dailyCalories_merged.csv")
view(daily_calories)
daily_intensities<-read.csv("dailyIntensities_merged.csv")

view(sleep_day)

#STEP 2: ################## Exploring the data #################################

#a) Exploring the daily_activity dataset
count(daily_activity) #457 observations
glimpse(daily_activity) #Date variable is a character and need to be transformed to "DATE" format
sum(is.na(daily_activity)) #No missing values
head(daily_activity, 10) #exploring the first 10 observations
n_distinct(daily_activity$Id) #Gives the distinct ID's, essentially number of unique individuals = 33

#b) Exploring the sleep_data dataset
count(sleep_day) #413 observations
glimpse(sleep_day) #date needs to be transformed to "DATE" format
sum(is.na(sleep_day)) # No missing values
head(sleep_day, 10) #Exploring the first 10 observations
n_distinct(sleep_day)#Gives the distinct ID's, essentially number of unique individuals = 410

#STEP 3: #################### Performing Analysis ##############################
summary(daily_activity)

#a) Relationship between Total Steps Vs. Calories
ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, col = "red")+
  labs(x = "Total Steps", y = "Calories", title = "Total Steps Vs. Calories")

#This relationship suggests that as the total number of steps increase, the calories burnt also increases
#indicating that people are interested in walking and the usage of smart devices can really add value. 

#b) Relationship between Time on the clock and Average Intensity
hourly_intensities <- hourly_intensities %>%
  mutate(time = as.POSIXct(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
         time = format(time, "%I:%M:%S %p"))  

#Aggregating the average intensity variable
hourly_intensity_avg<- hourly_intensities%>%
  group_by(time)%>%
  summarize(AverageIntensity = mean(AverageIntensity, na.rm = TRUE))

#Bar Chart
ggplot(hourly_intensity_avg, aes(x = time, y = AverageIntensity)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Time of Day", y = "Average Intensity", title = "Average Intensity by Time of Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#This relationship shows how the average intensity varies by the 'Time of Day' and indicates 
#the average intensity as the highest between 6PM and 7PM. This could help in advertisment / marketing. 
#c) Relationship between Time and Hourly Steps Total
hourly_steps<- hourly_steps %>%
  mutate(date_time = as.POSIXct(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
         hour = format(date_time, "%I:%M:%S %p"))  

#Aggregating the 'Calories' and 'Average Steps' variable
hourly_average<- hourly_steps %>%
  group_by(hour) %>%
  summarize(AverageCalories = mean(Calories, na.rm = TRUE),
            AverageSteps = mean(StepTotal, na.rm = TRUE)) %>%
  arrange(hour)

#Converting the variable to factor
hourly_average <- hourly_average %>%
  mutate(hour = factor(hour, levels = unique(hour)))


#Bar Chart
ggplot(hourly_steps, aes(x = hour, y = StepTotal)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Time of Day", y = "Total Steps", title = "Total Steps by Time of Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#This chart shows the relationship between Time of Date and Total Steps, and describes when 
#people usually walk the most and is in line with common sense. This will help the marketing
#team to understand not only the fact that consumers use smart devices to track data, but can 
#help in understanding what people use the smart devices for. 


#d) Relationship between Time in Bed and Total Minutes Asleep
ggplot(data = sleep_day, aes(x = TotalMinutesAsleep, y = TotalTimeInBed))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#Distance Tracker
ggplot(data = daily_activity, aes(x = TrackerDistance))+
  geom_histogram(fill = "lightblue", col = "black")+
  theme_minimal()+
  labs(x = "Tracker Distance", y = "Count", title = "Tracker Distance of Device")

#Through this histogram, we can analyze how much should the tracker on average measure and last 
#for, and based on the chart, most of the tracker track the activities somewhere between 
#5-6 miles. 

#e) Pie chart of the number of manual or automatic entries
percentage_TF<- weightinfo %>%
  group_by(IsManualReport) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(percentage_TF, aes(x = "", y = percentage, fill = IsManualReport)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  labs(title = "Manual Report Entries") + 
  theme_void() + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))

#This chart shows the number of people who reported their BMI and weight manually, versus the 
#ones who did not. In this case, 61.2% people reported their entries manually, instead of a metric. 
#The new smart device can contain a tool that can be used to automatically download the reports. 


#STEP 4: ################ Performing Advanced Analysis #########################

#First, conducting inner_joins of the datasets that are of interest
innerjoin_1 <- inner_join(daily_activity, daily_calories, by = c('Id', 'Calories'))
view(innerjoin_1)
innerjoin_2<-inner_join(daily_activity, daily_intensities)
view(innerjoin_2)
innerjoin_3<-inner_join(daily_activity, daily_steps)
view(innerjoin_3)
innerjoin_4<-inner_join(daily_activity, sleep_day)

#Second, merging the inner joins 
merge_1<-merge(innerjoin_1, innerjoin_2)
attach(merge_1)

#Step 1: Checking missing values
sum(is.na(merge_1))

#Step 2: Glimpse & Summary of the data
summary(merge_1)
glimpse(merge_1)

#Step 3: Visualization 1 - Relationship between Very Active Distance and Calories burned
ggplot(data = merge_1, aes(x = VeryActiveDistance, y = Calories))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  labs(x = "Very Active Distance", y = "Calories", title = "Very Activity Level and Calories Burned")

#Visualization 2 - Relationship between Light Activity Level and Calories burned
ggplot(data = merge_1, aes(x = LightActiveDistance, y = Calories))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  labs(x = "Light Active Distance", y = "Calories", title = "Light Activity Level and Calories Burned")

#Visualization 3 - analyzing the number of people spending time being active at varying levels 

#a) Calculating the mean 
mean(VeryActiveMinutes)
mean(FairlyActiveMinutes)
mean(SedentaryMinutes)
mean(LightlyActiveMinutes)

#b) Creating a data frame with average values and activity status
Average_activity<-data.frame(activity_levels = c("Very Active", "Fairly Active", "Lightly Active", "Sedimentary"),
                             average_minutes = c(24.10, 17.29, 199.59, 799.29))

ggplot(data = Average_activity, aes(x = activity_levels, y = average_minutes))+
  geom_col(fill = "skyblue")+
  labs(x = "Activity Levels", y = "Average Minutes Spent", title = "Average Minutes spent by Activity Level")+
  theme_minimal()+
  geom_text(aes(label = round(average_minutes, 1)),
            vjust = -0.5,                          
            color = "black",                         
            size = 4) 

#Most number of people spent time in sedimentary which is not good, and i recommend the new 
#software to encourage users to perform activities. 


#c) Working with Sleep Dataset:
mean(sleep_day$TotalMinutesAsleep[sleep_day$TotalSleepRecords == 2])
mean(sleep_day$TotalMinutesAsleep[sleep_day$TotalSleepRecords == 1])
mean(sleep_day$TotalTimeInBed[sleep_day$TotalSleepRecords == 2])
mean(sleep_day$TotalTimeInBed[sleep_day$TotalSleepRecords == 1])

#As expected, people who sleep twice spend more time in bed (because of collective sleep routines), than 
#people who sleep only once during the day. 

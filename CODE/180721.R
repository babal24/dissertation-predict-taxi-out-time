################################################
#DISSERTATION
#MSc Business Analytics
#Queen's University Belfast
################################################
#Vasileios Gounaris-Bampaletsos - 40314803

#TITLE: Improving management and finances in the aviation industry, 
#by forecasting the taxi-out time of aircraft
################################################

# setting the working directory
setwd("/Users/basilisgounarismpampaletsos/Desktop/DISSERTATION/CODE")
#set the num digits
options(scipen = 9)
# check working directory if it is ok
getwd()


#LOADING LIBRARIES
#import all the important libraries
#for this project
library(readxl)
library(psych)
library(ggplot2)
library(caTools)
library(statsr)
library(dplyr)
library(BAS)
library(car)
library(tidyr)
library(purrr)
library(gridExtra)
library(forcats)
library(corrplot)
library(magrittr)
library(caret)
library(Hmisc)
library(tidyverse)
library(ggpubr)
library(ROCR)
library(broom)
library(lubridate)
library(GGally)
library(ISLR)
library(hrbrthemes)
library(viridis)
library(e1071)
library(plyr)
library(readr)
library(repr)
library(glmnet)
library(ggthemes)
library(scales)
library(wesanderson)
library(styler)
library(xgboost)
library(randomForest)
library(rsample)      
library(gbm)          
library(h2o)          
library(pdp)          
library(lime)
library(naniar)
library(leaps)
library(tree)
library(MASS)
library(class)
library(data.table)
library(sandwich)
library(rpart.plot)
library(lmtest)
library(ranger)
library(nnet)
library(pROC)
library(kernlab)


#-----------------------------
# loading data
#name the data -> JFK
JFK <- read.csv("M1_final.csv") 


###########################################################
#DATA UNDERSTANDING
###########################################################
#summarize the raw data
#checking distribution and descriptive statistics
summary(JFK)
#check the structure
str(JFK)

#check for missing values
vis_miss(JFK)
sum(is.na(JFK))


#check some basic statistics 
#minimum, median and maximum values
#outliers, mean, median, quantiles
#understand data's distribution
#summary statistics function 
summary(JFK$MONTH) 
summary(JFK$DAY_OF_MONTH) 
summary(JFK$DAY_OF_WEEK)
summary(JFK$OP_UNIQUE_CARRIER) 
summary(JFK$TAIL_NUM) 
summary(JFK$DEP_DELAY) 
summary(JFK$DEST)
summary(JFK$CRS_ELAPSED_TIME)
summary(JFK$DISTANCE)
summary(JFK$CRS_DEP_M)
summary(JFK$DEP_TIME_M)
summary(JFK$CRS_ARR_M)
summary(JFK$Temperature)
summary(JFK$Dew.Point)
summary(JFK$Humidity)
summary(JFK$Wind)
summary(JFK$Wind.Speed)
summary(JFK$Wind.Gust)
summary(JFK$Pressure)
summary(JFK$Condition)
summary(JFK$sch_dep)
summary(JFK$sch_arr)


#check some statistics for target variable
summary(JFK$TAXI_OUT) #summary statistics
t.test(JFK$TAXI_OUT, conf.level=0.95) #Confidence Interval 
shapiro.test(JFK$TAXI_OUT[1:5000]) #normality for the first 5.000 obs.
skewness(JFK$TAXI_OUT) #Skewed towards the right


aggregate(TAXI_OUT ~ Temperature , JFK = JFK, FUN = mean)
aggregate(TAXI_OUT ~ sch_dep , JFK = JFK, FUN = mean)
aggregate(TAXI_OUT ~ Wind.Speed , JFK = JFK, FUN = mean)

describeBy(JFK$TAXI_OUT,JFK$MONTH)
describeBy(JFK$Temperature, JFK$TAXI_OUT)

describe(JFK)

#basic visualisations 
#histograms for numerical variables
#histograms help to check the data quality
hist(JFK$MONTH)
hist(JFK$DAY_OF_MONTH)
hist(JFK$DAY_OF_WEEK)
hist(JFK$DEP_DELAY)
hist(JFK$CRS_ELAPSED_TIME)
hist(JFK$DISTANCE)
hist(JFK$CRS_DEP_M)
hist(JFK$DEP_TIME_M)
hist(JFK$CRS_ARR_M)
hist(JFK$Temperature)
hist(JFK$Dew.Point)
hist(JFK$Humidity)
hist(JFK$Wind.Speed)
hist(JFK$Wind.Gust)
hist(JFK$Pressure)
hist(JFK$sch_dep)
hist(JFK$sch_arr)
hist(JFK$TAXI_OUT)


#visualisations for categorical variables
#barplot with all different carriers
#see the distribution of each carrier in JFK airport
ggplot(JFK, aes(OP_UNIQUE_CARRIER)) +
  geom_bar(colour="black", mapping = aes(fill = TAXI_OUT)) +
  labs(fill="Taxi Out", x="carriers", y= "count", 
       title="Carriers' Distribution in JFK")

#barplot with aircraft information
#tail numbers
#code for every aircraft
ggplot(JFK, aes(TAIL_NUM)) +
  geom_bar(colour="black", mapping = aes(fill = TAXI_OUT)) +
  labs(fill="TAXI_OUT", x="Tail Numbers", y= "count", 
       title="Tail Numbers Distribution")

#barplot with destination count
#check the most favourite destinations from JFK airport
ggplot(JFK, aes(DEST)) +
  geom_bar(colour="black", mapping = aes(fill = TAXI_OUT)) +
  labs(fill="taxi out", x="destination", y= "count", 
       title="Destinations' Distribution from JFK")

#barplot with wind(direction) count
#see the wind(direction) distribution in JFK airport
ggplot(JFK, aes(Wind)) +
  geom_bar(colour="black", mapping = aes(fill = TAXI_OUT)) +
  labs(fill="Taxi Out", x="Wind(direction)", y= "count", 
       title="Distribution of the wind's direction")

#barplot with condition(weather) count
#see the weather's condition distribution in JFK airport
ggplot(JFK, aes(Condition)) +
  geom_bar(colour="black", mapping = aes(fill = TAXI_OUT)) +
  labs(fill="Taxi Out", x="Weather Condition", y= "count", 
       title="Distribution of the weather's condition")


#################################################################################
#DATA PREPARATION
#################################################################################

#################
#FIX THE DATA
#The dataset has not any outliers to fix
#every variable is ok with its distribution and max-low values
#convert the month, day of month, day of week as factor
#because they represent days and months so it is not numeric
JFK$OP_UNIQUE_CARRIER <-as.factor(JFK$OP_UNIQUE_CARRIER)
JFK$TAIL_NUM <- as.factor(JFK$TAIL_NUM)
JFK$DEST <- as.factor(JFK$DEST)
JFK$Wind <- as.factor(JFK$Wind)
JFK$Condition <- as.factor(JFK$Condition)
JFK$MONTH <- as.factor(JFK$MONTH)
JFK$DAY_OF_WEEK <- as.factor(JFK$DAY_OF_WEEK)
JFK$DAY_OF_MONTH <- as.factor(JFK$DAY_OF_MONTH)

#############################################
#FINAL VISUALISATIONS
#VISUALISE THE CLEAN DATA
#made visualisations for every variables
#and also a compination between target variable and others

#----------------------
#TAXI_OUT
#Taxi out distribution
ggplot(JFK, aes(x=TAXI_OUT)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(TAXI_OUT)),
             linetype = "dashed", size = 0.6, color = "#FC4E07") +
  labs(x="Taxi Out Time(minutes)", y= "Count", 
     title="Distribution of Taxi-Out Time", 
     caption="With the mean line in red")

#barchart
ggplot(JFK, aes(TAXI_OUT)) +
  geom_bar(colour="black", mapping = aes(fill = TAXI_OUT)) +
  labs(fill="Taxi Out", x="Taxi Out Time(minutes)", y= "count", 
       title="Distribution of Taxi Out Time")

#plot the variable alone 
plot(JFK$TAXI_OUT)

#cumulative distribution
#this graph help us to understand
#better the distribution of the target variable
ggplot(JFK, aes(x=TAXI_OUT)) +
  stat_ecdf(geom = "step", color = 'red', lwd = 1) +
  labs(x="Taxi Out Time (minutes)", y= "Proportion", 
       title="Cumulative Distribution of Taxi Out")

#----------------------
#MONTH
ggplot(JFK,aes(MONTH, fill= MONTH))+ 
  geom_bar(stat="count", alpha = 1) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Months' Distribution", 
       x = "Months", y = "Count")

#----------------------
#DAY_OF_MONTH
ggplot(JFK, aes(x=DAY_OF_MONTH)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Days of Month", y = "Count", 
       title = "Count of the Days of Month")

#----------------------
#DAY_OF_WEEK
ggplot(JFK,aes(DAY_OF_WEEK, fill= DAY_OF_WEEK))+ 
  geom_bar(stat="count", alpha = 1) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Day of Week's Distribution", 
       x = "Day of Week", y = "Count")

#----------------------
#OP_UNIQUE_CARRIER
#B6 = JetBlue Airways
#DL = Delta Airlines
#9E = Endeavor Air
#AA = American Airlines
ggplot(JFK, mapping = aes(OP_UNIQUE_CARRIER)) + 
  geom_bar() +
  labs(title = "Aircraft carrier frequency", 
       x = "Unique Carriers", y = "Frequency")

#----------------------
#DEST = destination
#LAX = Los Angeles
#SFO = San Francisco
#BOS = Logan International Airport Boston
#FLL = Fort Lauderdale-Hollywood InternationL Airport
ggplot(JFK,aes(DEST, fill= DEST))+ 
  geom_bar(stat="count", alpha = 1) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Most famous destinations from JFK", 
       x = "Destinations", y = "Count")

#----------------------
#DEP_DELAY
#189 observations > 200 min. of delay
#690 observations > 100 min. of delay
#no further investigation
ggplot(JFK, aes(x=DEP_DELAY)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(DEP_DELAY)), 
             linetype = "dashed", size = 1, color = "#FC4E07") +
  labs(x="Delay (minutes)", y= "Count", 
       title="Distribution of Delay Time", 
       caption="With the mean line in red")

#----------------------
#CRS_ELAPSED_TIME
#scheduled arrival time
ggplot(JFK, aes(CRS_ELAPSED_TIME)) +
  geom_bar(colour="black", mapping = aes(fill = CRS_ELAPSED_TIME)) +
  labs(fill="CRS_ELAPSED_TIME", 
       x="Scheduled Flight Time(minutes)", y= "count", 
       title="Distribution of Scheduled Flight Time")

#density plot
ggplot(JFK, aes(x=CRS_ELAPSED_TIME)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(CRS_ELAPSED_TIME)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Scheduled Flight Time(minutes)", y= "Count", 
       title="Distribution of Scheduled Flight Time", 
       caption="With the mean line in red")

#----------------------
#DISTANCE
ggplot(JFK, aes(x=DISTANCE)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(DISTANCE)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Distance (miles)", y= "Count", 
       title="Distribution of Destinations' Airports Distances", 
       caption="With the mean line in red")

#histogram
ggplot(JFK, mapping = aes(DISTANCE)) +
  geom_histogram(bins=50) +
  labs(title = "Distance Distribution", x = "Distance", y = "Frequency")

#----------------------
#CRS_DEP_M
#in minutes after 24:00 at night
#500 = morning(08:00)
#1000-1250 = afternoon(17:00-20:00) 
ggplot(JFK, aes(x=CRS_DEP_M)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(CRS_DEP_M)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Scheduled Departure Time(Minutes after 24:00)", y= "Count", 
       title="Distribution of Scheduled Departures in JFK", 
       caption="With the mean line in red")

#----------------------
#DEP_TIME_M
#in minutes after 24:00 at night
#500 = morning(08:00)
#1000-1250 = afternoon(17:00-20:00) 
ggplot(JFK, aes(x=DEP_TIME_M)) +
  geom_density(aes(y = ..count..), 
               fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(DEP_TIME_M)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Real Departure Time(Minutes after 24:00)", y= "Count", 
       title="Distribution of Real Departures in JFK", 
       caption="With the mean line in red")

#----------------------
#CRS_ARR_M
#in minutes after 24:00 at night
#750 = morning(12:30)
#1000-1250 = afternoon(17:00-20:00) 
ggplot(JFK, aes(x=CRS_ARR_M)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(CRS_ARR_M)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Scheduled Arrival Time(Minutes after 24:00)", y= "Count", 
       title="Distribution of Scheduled Arrivals in JFK", 
       caption="With the mean line in red")

###################
#Weather Conditions
#----------------------
#Temperature
ggplot(JFK, aes(x=Temperature)) +
  geom_histogram(binwidth=1, fill="blue", color="#e9ecef", alpha=1) +
  labs(x="Temperature(Fahrenheit)", y= "Count", 
       title="Temperature Destribution in JFK")

#density plot
ggplot(JFK, aes(x=Temperature)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(Temperature)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Temperature(Fahrenheit)", y= "Count", 
       title="Distribution of Temperature in JFK", 
       caption="With the mean line in red")

#----------------------
#Humidity
ggplot(JFK, aes(x=Humidity)) +
  geom_histogram(binwidth=1, fill="blue", color="#e9ecef", alpha=1) +
  labs(x="Humidity(%)", y= "Count", 
       title="Humidity Destribution in JFK")

#density plot
ggplot(JFK, aes(x=Humidity)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(Humidity)), 
             linetype = "dashed", size = 0.6, color = "#FC4E07") +
  labs(x="Humidity(%)", y= "Count", 
       title="Distribution of Humidity in JFK", 
       caption="With the mean line in red")

#----------------------
#Wind
ggplot(JFK, aes(Wind, fill= Wind))+ 
  geom_bar(stat="count", alpha = 1) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Wind's Types Distribution in JFK", 
       x = "Wind Type", y = "Count")

#----------------------
#Wind Speed
ggplot(JFK, aes(x=Wind.Speed)) +
  geom_histogram(binwidth=1, fill="blue", color="#e9ecef", alpha=1) +
  labs(x="Wind Speed(knot)", y= "Count", 
       title="Wind Speed Destribution in JFK")

#density plot
ggplot(JFK, aes(x=Wind.Speed)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(Wind.Speed)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Wind Speed(knot)", y= "Count", 
       title="Distribution of Wind Speed in JFK", 
       caption="With the mean line in red")

#----------------------
#Pressure
ggplot(JFK, aes(x=Pressure)) +
  geom_histogram(binwidth=1, fill="blue", color="#e9ecef", alpha=1) +
  labs(x="Air Pressure(psi)", y= "Count", 
       title="Air Pressure Distribution in JFK")

#histogram
ggplot(JFK, aes(x=Pressure)) +
  geom_histogram(binwidth=1, fill=c(rep("red",1), rep("blue",1), rep("orange",1)), 
                 color="#e9ecef", alpha=1) +
  labs(x="Air Pressure(psi)", y= "Rented Bike Count", 
       title="Air Pressure Distribution in JFK")

#----------------------
#Condition
#weather condition types
ggplot(JFK,aes(Condition, fill= Condition))+ 
  geom_bar(stat="count", alpha = 1) + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Most Frequent Weather Conditions in JFK", 
       x = "Weather Condition Type", y = "Count")

#----------------------
#sch_dep
#scheduled departures (number of aircrafts)
ggplot(JFK, aes(x=sch_dep)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(sch_dep)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Departures(aircaft)", y= "Count", 
       title="Distribution of Simultaneous Departures in JFK", 
       caption="With the mean line in red")

#barplot
ggplot(JFK, aes(sch_dep)) +
  geom_bar(colour="black", mapping = aes(fill = sch_dep)) +
  labs(fill="sch_dep", x="Departures(aircaft)", y= "count", 
       title="Distribution of Simultaneous Departures in JFK")

#----------------------
#sch_arr
#scheduled simultaneous arrivals (number of aircrafts)
ggplot(JFK, aes(x=sch_arr)) +
  geom_density(aes(y = ..count..), fill="#69b3a2", color="#e9ecef", alpha=1) +
  geom_vline(aes(xintercept = mean(sch_arr)), 
             linetype = "dashed", size = 0.8, color = "#FC4E07") +
  labs(x="Arrivals(aircaft)", y= "Count", 
       title="Distribution of Simultaneous Arrivals in JFK", 
       caption="With the mean line in red")

#barplot
ggplot(JFK, aes(sch_arr)) +
  geom_bar(colour="black", mapping = aes(fill = sch_arr)) +
  labs(fill="sch_arr", x="Arrivals(aircaft)", y= "count", 
       title="Distribution of Simultaneous Arrivals in JFK")


#################################################
#VISUALISATIONS
#VISUALISATIONS BETWEEN TAXI-OUT AND OTHERS
#checking the correlations between 
#target variable and other variables

#TAXI OUT -- Month
ggplot(JFK) +
  geom_boxplot(aes(x=MONTH, y=TAXI_OUT)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) + 
  theme(legend.position = "none") +
  labs(title = "Taxi Out Distribution by Month",
       x = "Month",
       y = "Taxi Out(Minutes)")

#TAXI OUT -- Month
ggplot(JFK) +
  geom_density(aes(x = TAXI_OUT,
                   fill = MONTH), 
               alpha = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(title = "Taxi Out Density By Month",
       fill = "Month",
       x = "Taxi Out (minutes)",
       y = "Density")

#Taxi Out -- Day of Week
ggplot(JFK) +
  geom_boxplot(aes(x=DAY_OF_WEEK, y=TAXI_OUT)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(legend.position = "none") +
  labs(title = "Taxi Out Distribution by Days of Week",
       x = "Days of Week",
       y = "Taxi Out(Minutes)")

#Taxi Out -- Day of Week
ggplot(JFK, aes(x = TAXI_OUT)) +
  geom_histogram(aes(color = DAY_OF_WEEK, fill= DAY_OF_WEEK), 
                 alpha =0.5, position = "identity", bins = 20) +
  facet_grid(DAY_OF_WEEK ~.) + 
  theme(legend.position = "none") +
  labs(title = "Taxi Out Distribution by Days of Week",
       x = "Taxi Out (minutes)",
       y = "Count")

#TAXI OUT -- Day of Week per Month
ggplot(JFK, aes(x=DAY_OF_WEEK, y=TAXI_OUT, fill=MONTH)) + 
  geom_boxplot(alpha=0.3) +
  theme(axis.title = element_text()) + 
  labs(title = "Taxi Out Density By Week's Days",
       fill = "DAY_OF_WEEK",
       x = "Day Of Week",
       y = "Taxi Out (minutes)")

#taxi out -- airline carriers
ggplot(JFK, aes(x = TAXI_OUT)) +
  geom_histogram(aes(color = OP_UNIQUE_CARRIER, fill= OP_UNIQUE_CARRIER), 
                 alpha =0.3, position = "identity", bins = 20) +
  facet_grid(OP_UNIQUE_CARRIER ~.) + 
  theme(legend.position = "none") +
  labs(title = "Taxi Out Distribution by Airline Carrier",
       x = "Taxi Out (minutes)",
       y = "Count")

#taxi out -- airline carriers
ggplot(JFK) +
  geom_boxplot(aes(x=OP_UNIQUE_CARRIER, y=TAXI_OUT)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(legend.position = "none") +
  labs(title = "Taxi Out Distribution by Airline Carrier",
       x = "Airline Carriers",
       y = "Taxi Out(Minutes)")

#Taxi Out - Condition
ggplot(JFK, aes(TAXI_OUT)) +
  geom_bar(aes(fill= Condition), width = 0.8) + 
  theme(axis.text.x = element_text(angle=80, vjust=1)) +
  labs(title="Taxi Out Time distribution by Weather's Conditions",
       x = "Taxi Out (minutes)",y = "Count",
       caption="Better Understanding for JFK's condition")

#Taxi Out -- Wind Direction Type
ggplot(JFK, aes(TAXI_OUT)) +
  geom_bar(aes(fill= Wind), width = 0.8) + 
  theme(axis.text.x = element_text(angle=80, vjust=1)) +
  labs(title="Taxi Out Time distribution by Wind Direction Type",
       x = "Taxi Out (minutes)",y = "Count",
       caption="Better Understanding for JFK's Winds")

#Temperature+Humidity per Month
ggplot(JFK, aes(x=Temperature,y=Humidity))+ 
  geom_point(alpha = 0.5, aes(color=MONTH)) + 
  labs(fill="Month", x="Temperature(Farhenheit)", y= "Humidity(%)", 
       title="Distribution of Weather Conditions in JFK")

#taxi out -- departure delay
ggplot(data = JFK) +
  geom_point(mapping = aes(x = DEP_DELAY, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = DEP_DELAY, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Departure Delay and Taxi Out Time", 
       x = "Delay(minutes)", y = "Taxi Out(minutes)")

#taxi out -- temperature
ggplot(data = JFK) +
  geom_point(mapping = aes(x = Temperature, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = Temperature, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Temperature and Taxi Out Time", 
       x = "Temperature(Fahrenheit)", y = "Taxi Out(minutes)")

#taxi out -- Pressure
ggplot(data = JFK) +
  geom_point(mapping = aes(x = Pressure, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = Pressure, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Pressure and Taxi Out Time", 
       x = "Pressure(psi)", y = "Taxi Out(minutes)")

#taxi out -- humidity
ggplot(data = JFK) +
  geom_point(mapping = aes(x = Humidity, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = Humidity, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Humidity and Taxi Out Time", 
       x = "Humidity(%)", y = "Taxi Out(minutes)")

#taxi out -- Wind Speed
ggplot(data = JFK) +
  geom_point(mapping = aes(x = Wind.Speed, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = Wind.Speed, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Wind Speed and Taxi Out Time", 
       x = "Wind Speed (knots)", y = "Taxi Out(minutes)")

#taxi out -- Scheduled Simultaneous Departures
ggplot(data = JFK) +
  geom_point(mapping = aes(x = sch_dep, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = sch_dep, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Scheduled Simultaneous Departures and Taxi Out Time", 
       x = "Departures(aircrafts)", y = "Taxi Out(minutes)")

#taxi out -- Scheduled Simultaneous Arrivals 
ggplot(data = JFK) +
  geom_point(mapping = aes(x = sch_arr, y = TAXI_OUT)) +
  geom_smooth(mapping = aes(x = sch_arr, y = TAXI_OUT), method = "lm") + 
  labs(title = "Relationship between Scheduled Simultaneous Arrivals and Taxi Out Time", 
       x = "Arrivals(aircrafts)", y = "Taxi Out(minutes)")


#######################
#CORRELATIONS
#check the correlation between variables
#use 3 different type of cor matrix for numeric variables

#install.packages("dplyr")
#library(dplyr)
continuous_var <- select(JFK,TAXI_OUT,DEP_DELAY,CRS_ELAPSED_TIME,DISTANCE,CRS_DEP_M,
                         DEP_TIME_M,CRS_ARR_M,Temperature, Dew.Point,Humidity,
                         Wind.Speed,Wind.Gust,Pressure,sch_dep,sch_arr)


#correlation matrix
#pearson method 
#FULL SIZE
continuous_var.cor = cor(na.omit(continuous_var), method = "pearson")
corrplot(continuous_var.cor)

#FULL SIZE with numbers(correlations)
corrplot(continuous_var.cor, method = 'number')

#half cor matrix
ggcorr(na.omit(continuous_var), method = c("everything", "pearson"))

#pair plots 
#using pearson method 
pairs.panels(JFK, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#categorical variables
#using pearson's chi-squared test
chisq.test(JFK$MONTH, JFK$TAXI_OUT)
chisq.test(JFK$DAY_OF_MONTH, JFK$TAXI_OUT)
chisq.test(JFK$DAY_OF_WEEK, JFK$TAXI_OUT)
chisq.test(JFK$OP_UNIQUE_CARRIER, JFK$TAXI_OUT)
chisq.test(JFK$TAIL_NUM, JFK$TAXI_OUT)
chisq.test(JFK$DEST, JFK$TAXI_OUT)
chisq.test(JFK$Wind, JFK$TAXI_OUT)
chisq.test(JFK$Condition, JFK$TAXI_OUT)

#####################################
#split the data
#train_set 80%
#test_set 20%
set.seed(123)
split = sample.split(JFK$TAXI_OUT, SplitRatio = 0.8)
train_set = subset(JFK, split == TRUE)
test_set = subset(JFK, split == FALSE)


#########################################################################
#DATA MODELING
#########################################################################
#select variables for the models
#Bayesian Information Criterion (BIC)
#for forward selection
best_sel = regsubsets(TAXI_OUT ~.-TAIL_NUM -Wind.Gust 
                      -Dew.Point -CRS_DEP_M -DISTANCE, 
                      data = train_set, method = "forward",
                      nvmax = length(data)-1)
best_sel_sum = summary(best_sel)

plot(best_sel_sum$bic, type = 'b', col = "blue", pch = 19, 
     xlab = "Number of Variables",
     ylab = "Cross-Validated Prediction Error",
     main = "Forward Stepwise Selection using BIC")
points(which.min(best_sel_sum$bic), 
       best_sel_sum$bic[which.min(best_sel_sum$bic)],
       col = "red", pch = 19)

#list all variable which are important to use
final = t(best_sel_sum$which)[,which.min(best_sel_sum$bic)]
final_names = names(data)[-23]
final_names[final[23:length(data)]]
final_names

#########################################
#create the model
#testing the variables and choose the best model
RegTest = lm(formula = TAXI_OUT ~.-TAIL_NUM -Wind.Gust
           -Dew.Point -CRS_DEP_M -DISTANCE,
           data = train_set)
summary(RegTest)

#linear regression
#making some examples before final selection
LinReg = lm(formula = RegTest,
             data = train_set)
summary(LinReg)


##############################
#assumptions checking

#assumption 1
#mean of residuals
mean(RegTest$residuals)
#residuals fit
x <- rstandard(RegTest)
sum(x > 1.96)

#assumption 2
#Homoscedasticity
#residuals normality
par(mfrow=c(2,2))
plot(RegTest)

#infuential values
#cook's distance graph
plot(RegTest, which = 4, id.n = 3)
#no. of varibles which violate cook's distance
cook <- cooks.distance(RegTest)
sum(cook > 1)

#assumption 3
#multicollinearity
vif(RegTest)

#assumption 4
#influential cases
influence.measures(RegTest)

#assumption 5
#independent residuals
durbinWatsonTest(RegTest)


###############################
#linear regression
#cross validation (k=10)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
LinReg <- train(TAXI_OUT ~.-TAIL_NUM -Wind.Gust
                 -Dew.Point -CRS_DEP_M -DISTANCE, 
                 data = train_set, method = "lm",
                 trControl = train.control)
summary(LinReg)

#print the results
print(LinReg)

#predictions on test set
LinPred <- predict(LinReg, newdata = test_set)
LinPred

#Compute RMSE, MAE and R^2
postResample(LinReg, test_set$TAXI_OUT)

########################################################
#RIDGE
########################################################
data = na.omit(JFK)

#cbuild a matrix
#use ridge regression
x = model.matrix(TAXI_OUT ~.-TAIL_NUM -Wind.Gust
                 -Dew.Point -CRS_DEP_M -DISTANCE, 
                 JFK)
#select the indepented variable
y = JFK$TAXI_OUT

#using grid search
#for best lambda
grid = 10^seq(10, -2, length = 100)
RidgeReg = glmnet(x, y, alpha = 0, lambda = grid)
RidgeReg

#plot coef of the ridge model
dim(coef(RidgeReg))
plot(RidgeReg)

#find the lambda
RidgeReg$lambda[50]
coef(RidgeReg)[,50]
sqrt(sum(coef(RidgeReg)[-1,50]^2))

predict(RidgeReg, s=50, type="coefficients")[1:20,]

x_train = model.matrix(TAXI_OUT ~.-TAIL_NUM -Wind.Gust 
                       -Dew.Point -CRS_DEP_M -DISTANCE, 
                       train_set)
x_test = model.matrix(TAXI_OUT ~.-TAIL_NUM -Wind.Gust
                      -Dew.Point -CRS_DEP_M -DISTANCE, 
                      test_set)

y_train = train_set$TAXI_OUT

y_test = test_set$TAXI_OUT

#boost the ridge model
RidgeReg = glmnet(x_train, y_train, alpha=0, lambda = grid, 
                   thresh = 1e-12)
#plot the trace of ridge
plot(RidgeReg, xvar = "lambda")

#predictions on test set
RidgePred = predict(RidgeReg, s = 4, newx = x_test)

#Compute RMSE, MAE and R^2
postResample(RidgePred, test_set$TAXI_OUT)

##########################################################
#LASSO
#lasso regression
#set the alpha=1
LassoReg = glmnet(x_train,
                   y_train,
                   alpha = 1,
                   lambda = grid)
LassoReg
plot(LassoReg)

set.seed(1)
# Fit lasso model on training data
cv_output = cv.glmnet(x_train, y_train, alpha = 1) 
# Draw plot of training MSE as a function of lambda
plot(cv_output) 

# choose the lambda which minimizes training MSE
best_lambda = cv_output$lambda.min 

#predictions on the test set
# Use best lambda to predict test set
LassoPred = predict(LassoReg, s = best_lambda , newx = x_test) 

#Compute RMSE, MAE and R^2
postResample(LassoPred, test_set$TAXI_OUT)

# Fit lasso model
out = glmnet(x, y, alpha = 1, lambda = grid) 
#check coefficients using lambda
lasso_coef = predict(out, type = "coefficients", s = best_lambda)
lasso_coef

#check only non-zero coefficients
lasso_coef[lasso_coef != 0]

#############################################################
#KNN regression

KNNmodel = knnreg(x_train, y_train)

str(KNNmodel)

KNNpred = predict(KNNmodel, data.frame(x_test))

#Compute RMSE, MAE and R^2
postResample(KNNpred, test_set$TAXI_OUT)

#plot the results from KNN 
x = 1:length(y_test)
plot(x, y_test, col = "red", type = "l", lwd=2,
     main = "Test data prediction")
lines(x, pred_y, col = "blue", lwd=2)
legend("topright",  legend = c("original", "predicted"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()


#############################################################
#SVM regression
#create the svm regression model
#use radial kernel in auto mode
SVMreg = svm(TAXI_OUT ~.-TAIL_NUM -Wind.Gust
              -Dew.Point -CRS_DEP_M -DISTANCE, 
              data=train_set)
print(SVMreg)

#predictions on the test set
SVMpred = predict(SVMreg, test_set)

#compare predictions vs real
x=1:length(test_set$TAXI_OUT)
plot(x, test_set$TAXI_OUT, pch=13, col="red")
lines(x, svm_pred, lwd="1", col="blue")

#Compute RMSE, MAE and R^2
postResample(SVMpred, test_set$TAXI_OUT)

#plot the stats and results from svm regression model
plot(JFK)
points(JFK$TAXI_OUT, SVMpred, col="red", pch=13)

############################################################
#Gradient Boosting Machines
#GBM1
#cross validation with k=5
gbm1 <- gbm(TAXI_OUT ~.-TAIL_NUM -Wind.Gust
            -Dew.Point -CRS_DEP_M -DISTANCE,
  distribution = "gaussian",
  data = train_set,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

#results
print(gbm1)

#loss function as a result of n trees added to the ensemble
gbm.perf(gbm1, method = "cv")

#influence bar chart
par(mar = c(5, 8, 1, 1))
summary(
  gbm1, 
  cBars = 10,
  method = relative.influence,
  las = 2
)

#predictions on the test set
GBM1pred <- predict(gbm1, n.trees = gbm1$n.trees, test_set)
#Compute RMSE, MAE and R^2
postResample(GBM1pred, test_set$TAXI_OUT)

##############
#GBM2
#change the values in gbm2
#target = tune the gbm1

# train model
gbm2 <- gbm(
  formula = TAXI_OUT ~.-TAIL_NUM -Wind.Gust
            -Dew.Point -CRS_DEP_M -DISTANCE,
  distribution = "gaussian",
  data = train_set,
  n.trees = 10000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

#results
print(gbm2)

#find index for n trees with minimum CV error
min_MSE <- which.min(gbm2$cv.error)
summary(gbm2$cv.error)

#loss function as a result of n trees added to the ensemble
gbm.perf(gbm2, method = "cv")

#influence bar chart
par(mar = c(5, 8, 1, 1))
summary(
  gbm2, 
  cBars = 10,
  method = relative.influence,
  las = 2
)

#predictions on the test set
GBM2pred <- predict(gbm2, n.trees = gbm2$n.trees, test_set)
#Compute RMSE, MAE and R^2
postResample(GBM2pred, test_set$TAXI_OUT)

##########################################################
#extreme gradient boosting(XGBoost)
#using cross validation with k=10
set.seed(123)
XGBreg <- train(TAXI_OUT ~.-TAIL_NUM -Wind.Gust
                 -Dew.Point -CRS_DEP_M -DISTANCE, 
                 data = train_set, method = "xgbTree",
          trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
XGBreg$bestTune

#predictions on the test set
XGBpred = predict(XGBreg, test_set)
head(predictions)

#Compute RMSE, MAE and R^2
postResample(XGBpred, test_set$TAXI_OUT)

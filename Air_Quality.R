#Air quality project

#install and include the packages that will be used

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(caret)
library(tidyverse)
library(rpart)
library(dplyr)
library(ggplot2)
library(lubridate)


# Air Quality Data Set:
# https://archive.ics.uci.edu/ml/datasets/Air+quality
# https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip

#download the zip file and extract the data

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip", dl)

AirQuality <- read_csv2(unzip(dl, "AirQualityUCI.csv"))

#missing columns found. Total 17 columns instead of 15 due to incorrect amount of semicolons
#we exclude the columns 16 and 17 that are empty and not mentioned in description

AirQuality <- AirQuality %>% select(!X16) %>% select(!X17)

AirQuality
str(AirQuality)

#We noted that the data is presented in "wide" format, with every column representing date, time, 
#sensor value, actual value of pollutant etc. 

#Here is the description of data from the web page of dataset https://archive.ics.uci.edu/ml/datasets/Air+quality

#Date - date (DD/MM/YYYY)
#Time - time (HH.MM.SS)

#CO(GT) - true hourly averaged concentration CO in mg/m3 (reference analyzer)
#PT08.S1(CO) - hourly averaged sensor response (tin oxide, nominally CO targeted)

#NMHC(GT) - true hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer)
#PT08.S2(NMHC) hourly averaged sensor response (titania, nominally NMHC targeted)

#C6H6(GT) - true hourly averaged Benzene concentration in microg/m3 (reference analyzer)
#we noted that we do not have any special sensor targeted to Benzene


#NOx(GT) - true hourly averaged NOx concentration in ppb (reference analyzer)
#ppb is an abbreviation for Parts per billion
#NOx is an abbreviation for NO and NO2
#PT08.S3(NOx) - hourly averaged sensor response (tungsten oxide, nominally NOx targeted)

#NO2(GT) - true hourly averaged NO2 concentration in microg/m3 (reference analyzer)
#PT08.S4(NO2) - hourly averaged sensor response (tungsten oxide, nominally NO2 targeted)

#PT08.S5(O3) -  hourly averaged sensor response (indium oxide, nominally O3 targeted)
#We noted that we do not have true hourly averaged 03 level data

#T - temperature in °C
#RH - relative humidity (%)
#AH - absolute humidity

#While checking the limits of values we note that the data has some `NA` values
max(AirQuality$`CO(GT)`)
AirQuality %>% filter(is.na(`CO(GT)`))

#We exclude the missing lines (containing only 'NA's) and update the dataframe
AirQuality <- AirQuality %>% filter(!is.na(`CO(GT)`))

AirQuality

max(AirQuality$`CO(GT)`)
max(AirQuality$`PT08.S1(CO)`)
max(AirQuality$`NMHC(GT)`)
max(AirQuality$`PT08.S2(NMHC)`)
max(AirQuality$`C6H6(GT)`)

max(AirQuality$`NOx(GT)`)
max(AirQuality$`PT08.S3(NOx)`)
max(AirQuality$`NO2(GT)`)
max(AirQuality$`PT08.S4(NO2)`)

max(AirQuality$`PT08.S5(O3)`)

#We note that the maximum sensor response among all the values associated with sensors (PT08.S*) is 2683.
#We know that any digital sensor has certain resolution in bits (1-bit sensor has 2 available values - "0" and "1"). 
#We suppose that 12-bit sensors are used (could measure 4096 values from 0 to 4095)


#We have 9357 lines with the values 

#We also note in the description of the dataframe that the value `-200` is assigned to missing values. 
#every column with sensor value and actual level of pollutant contains missing data.

min(AirQuality$`CO(GT)`)
min(AirQuality$`PT08.S1(CO)`)
min(AirQuality$`NMHC(GT)`)
min(AirQuality$`PT08.S2(NMHC)`)
min(AirQuality$`C6H6(GT)`)

min(AirQuality$`NOx(GT)`)
min(AirQuality$`PT08.S3(NOx)`)
min(AirQuality$`NO2(GT)`)
min(AirQuality$`PT08.S4(NO2)`)

min(AirQuality$`PT08.S5(O3)`)


#if we exclude all the lines containing missing values (we understand that the actual values of pollutants,   
#or sensor values could be only positive), we will lose most of the data (only 827 lines from 9357 will be left)
AirQuality %>% 
  filter(`CO(GT)` > 0) %>%
  filter(`PT08.S1(CO)` > 0) %>%
  filter(`NMHC(GT)` > 0) %>%
  filter(`PT08.S2(NMHC)` > 0) %>%  
  filter(`C6H6(GT)` > 0) %>%  
  filter(`NOx(GT)` > 0) %>%
  filter(`PT08.S3(NOx)` > 0) %>%
  filter(`NO2(GT)` > 0) %>%
  filter(`PT08.S4(NO2)` > 0) %>%
  filter(`PT08.S5(O3)` > 0)   

#To build the diagrams using both date and time we convert these to column to the "Timestamp" column

#First, we set the vector containing the  number of hour from each line (the data is collected hourly)
time <- as.factor(AirQuality$Time)
hour = as.numeric(format(as.POSIXct(time,format="%H.%M.%S"),"%H"))

#That we extract the date as a timestamp (number of days since January 1, 1970 used as the epoch)
date <- as.numeric(as.Date(AirQuality$Date, "%d/%m/%Y"))

#We add the vector "timestamp", which shows the number of hours from the epoch
timestamp = date * 24 + hour

#We create the empty column "Timestamp", than assign the values from the vector "timestamp" to this column
AirQuality <- AirQuality %>% mutate(Timestamp = 0)
AirQuality$Timestamp <- timestamp


#The description of data says that the certain censors are nominally targeted to certain pollutants. 


#We will make the separate dataframes for each pollutant, clean the data from missing values and 
#check the correlation between the level of pollutant and the response of correspondent sensor


#CO pollution

CO_pollution <- AirQuality %>% 
  select(Timestamp, `CO(GT)`, `PT08.S1(CO)`) %>%
  filter(`CO(GT)` > 0) %>% 
  filter(`PT08.S1(CO)` >0)

cor(CO_pollution$`CO(GT)`, CO_pollution$`PT08.S1(CO)`)
#0.8792883

CO_pollution %>%
  ggplot(aes(`CO(GT)`, `PT08.S1(CO)`)) +
  geom_point(size = 1) + 
  xlab("True hourly averaged concentration CO in mg/m3") +
  ylab("Hourly averaged CO targeted sensor response") +
  ggtitle("Actual CO pollution and response of sensor S1 targeted to CO")



#NMHC pollution
NMHC_pollution <- AirQuality %>% 
  select(Timestamp, `NMHC(GT)`, `PT08.S2(NMHC)`) %>%
  filter(`NMHC(GT)` > 0) %>% 
  filter(`PT08.S2(NMHC)` >0)

cor(NMHC_pollution$`NMHC(GT)`, NMHC_pollution$`PT08.S2(NMHC)`)
#0.8776957


NMHC_pollution %>%
  ggplot(aes(`NMHC(GT)`, `PT08.S2(NMHC)`)) +
  geom_point(size = 1) + 
  xlab("Hourly averaged overall NMHC concentration in microg/m3") +
  ylab("Hourly averaged NMHC targeted sensor response") +
  ggtitle("Actual Non Metanic HydroCarbons concentration (NMHC) pollution and response of sensor S2 targeted to NMHC")





#NOx pollution
NOx_pollution <- AirQuality %>% 
  select(Timestamp, `NOx(GT)`, `PT08.S3(NOx)`) %>%
  filter(`NOx(GT)` > 0) %>% 
  filter(`PT08.S3(NOx)` >0)

cor(NOx_pollution$`NOx(GT)`, NOx_pollution$`PT08.S3(NOx)`)
#-0.6557071

NOx_pollution %>%
  ggplot(aes(`NOx(GT)`, `PT08.S3(NOx)`)) +
  geom_point(size = 1) + 
  xlab("Hourly averaged NOx concentration in ppb") +
  ylab("Hourly averaged NOx targeted sensor response") +
  ggtitle("Actual NOx pollution and response of sensor S3 targeted to NOx")



#NO2 pollution
NO2_pollution <- AirQuality %>% 
  select(Timestamp, `NO2(GT)`, `PT08.S4(NO2)`, T) %>%
  filter(`NO2(GT)` > 0) %>% 
  filter(`PT08.S4(NO2)` >0)

cor(NO2_pollution$`NO2(GT)`, NO2_pollution$`PT08.S4(NO2)`)
#0.1573603 - weak correlation 


NO2_pollution %>%
  ggplot(aes(`NO2(GT)`, `PT08.S4(NO2)`)) +
  geom_point(size = 1) + 
  xlab("Hourly averaged NO2 concentration in microg/m3") +
  ylab("Hourly averaged NO2 targeted sensor response") +
  ggtitle("Actual NO2 pollution and response of sensor S4 targeted to NO2")


#Now we check if the temperature also influences the response of the sensor
NO2_pollution %>%
  ggplot(aes(`NO2(GT)`, `PT08.S4(NO2)`)) +
  geom_tile(aes(fill = T)) +
  theme_classic() +
  scale_fill_gradient(low="green", high="red", limits=c(-10, 50)) +
  xlab("Hourly averaged NO2 concentration in microg/m3") +
  ylab("Hourly averaged NO2 targeted sensor response") +
  ggtitle("Actual NO2 pollution and response of sensor S4 targeted to NO2")

cor(NO2_pollution$T, NO2_pollution$`PT08.S4(NO2)`)
#0.5755162

#We could see that higher temperature shifts the range of response of the sensor up with the same level of pollution




#Let's check if other sensors could help to predict the level of NO2
NO2_pollution_other <- AirQuality %>% 
  select(Timestamp, `NO2(GT)`, `PT08.S1(CO)`, `PT08.S2(NMHC)`, `PT08.S3(NOx)`, `PT08.S4(NO2)`,  `PT08.S5(O3)`) %>%
  filter(`NO2(GT)` > 0) %>%
  filter(`PT08.S1(CO)` >0) %>%
  filter(`PT08.S2(NMHC)` >0) %>%
  filter(`PT08.S3(NOx)` >0) %>%
  filter(`PT08.S4(NO2)` >0) %>%
  filter(`PT08.S5(O3)` >0)

cor(NO2_pollution_other$`NO2(GT)`, NO2_pollution_other$`PT08.S1(CO)`)
#0.642083 

cor(NO2_pollution_other$`NO2(GT)`, NO2_pollution_other$`PT08.S2(NMHC)`)
#0.6462453
  
cor(NO2_pollution_other$`NO2(GT)`, NO2_pollution_other$`PT08.S3(NOx)`)
#-0.652083 

cor(NO2_pollution_other$`NO2(GT)`, NO2_pollution_other$`PT08.S5(O3)`)
#0.7081276

#Insights 
#1. The responses of sensors nominally targeted to CO and NHMC have strong correlation with actual levels 
#of CO and NMHC correspondingly. 
#The response of sensor nominally targeted to NOx has medium negative correlation with actual level of NOx.
#2. The response of sensor nominally targeted to NO2 has weak correlation with actual level of NO2.
#3. The responses of sensors nominally targeted to 03, CO, NHMC and NOx have medium correlation 
#with the actual level of NO2 (cross-reference).
#4. We could try to use the cross-reference of the sensors (with high correlation with the levels of pollutants) 
#to predict the levels of pollutants to which the sensors are not nominally targeted. 

#We could try to make the model for prediction of NO2 level using 
#the sensors nominally targeted to other pollutants (O3, CO, NHMC and  NOx).

#Since we do not have the actual values of pollutant predicted by sensors (we only have the responses of sensors 
#and we do not have corresponding predicted level of pollutant), we noted that the correlation coefficient 
#could be used to measure how our algorithm works. We will use a threshold 0.75 as a minimal required correlation. 

#We will also use RMSE as a secondary measure for evaluation of the algorithm.

#set the function to calculate RMSE
RMSE <- function(true_concentration, predicted_concentration){
  sqrt(mean((true_concentration - predicted_concentration)^2))
}

#First, we try to build the algorithm using 10% of NO2_pollution_other dataframe for validation and, 
#90% for training (train_set)
#We will try linear Model (LM) using the sensors nominally targeted to other pollutants (O3, CO, NHMC and  NOx)

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = NO2_pollution_other$`NO2(GT)`, times = 1, p = 0.1, list = FALSE)
train_set_NO2 <- NO2_pollution_other[-test_index,]
test_set_NO2 <- NO2_pollution_other[test_index,]

train_lm <- train(`NO2(GT)` ~ `PT08.S1(CO)` + `PT08.S2(NMHC)` + `PT08.S3(NOx)` + `PT08.S5(O3)`, 
                  method = "lm", data = train_set_NO2)

train_set_NO2 <- train_set_NO2 %>% 
  mutate(predicted_NO2_lm = predict(train_lm, train_set_NO2))

RMSE(train_set_NO2$`NO2(GT)`, train_set_NO2$predicted_NO2_lm)
#32.61922
cor(train_set_NO2$`NO2(GT)`, train_set_NO2$predicted_NO2_lm)
#0.7269481

#prediction on test set
test_set_NO2 <- test_set_NO2 %>% 
  mutate(predicted_NO2_lm = predict(train_lm, test_set_NO2))

RMSE_NO2_10p <- RMSE(test_set_NO2$`NO2(GT)`, test_set_NO2$predicted_NO2_lm)
RMSE_NO2_10p
#34.8656
cor_NO2_10p <- cor(test_set_NO2$`NO2(GT)`, test_set_NO2$predicted_NO2_lm)
cor_NO2_10p
#0.6988193

cor_results <- tibble(Method = "Linear model, sensors S1, S2, S3, S5, test set 10%", Correlation = cor_NO2_10p,
                          RMSE = RMSE_NO2_10p)


cor_results

#Correlation coefficient on test_set is much lower than on train_set (and RMSE is higher).
#Looks like the algorithm was a bit overtrained and showed much better results on train set 
#than on unused for training data

#Let's use 70% of data for training (instead of 90%) and 30% of data for validation 

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = NO2_pollution_other$`NO2(GT)`, times = 1, p = 0.3, list = FALSE)
train_set_NO2_30p <- NO2_pollution_other[-test_index,]
test_set_NO2_30p <- NO2_pollution_other[test_index,]

#Linear Model (LM) using the sensors nominally targeted to other pollutants (O3, CO, NHMC and  NOx)
train_lm_NO2_30p <- train(`NO2(GT)` ~ `PT08.S1(CO)` + `PT08.S2(NMHC)` + `PT08.S3(NOx)` + `PT08.S5(O3)`, 
                  method = "lm", data = train_set_NO2_30p)

train_set_NO2_30p <- train_set_NO2_30p %>% 
  mutate(predicted_NO2_lm = predict(train_lm_NO2_30p, train_set_NO2_30p))

RMSE(train_set_NO2_30p$`NO2(GT)`, train_set_NO2_30p$predicted_NO2_lm)
#33.1204
cor(train_set_NO2_30p$`NO2(GT)`, train_set_NO2_30p$predicted_NO2_lm)
#0.7154569


#prediction on test set
test_set_NO2_30p <- test_set_NO2_30p %>% 
  mutate(predicted_NO2_lm = predict(train_lm_NO2_30p, test_set_NO2_30p))

RMSE_NO2_30p <- RMSE(test_set_NO2_30p$`NO2(GT)`, test_set_NO2_30p$predicted_NO2_lm)
RMSE_NO2_30p
#32.24374
cor_NO2_30p <- cor(test_set_NO2_30p$`NO2(GT)`, test_set_NO2_30p$predicted_NO2_lm)
cor_NO2_30p
#0.743355

cor_results <- bind_rows(cor_results,
                          tibble(Method = "Linear model, sensors S1, S2, S3, S5, test set 30%", 
                                     Correlation = cor_NO2_30p,
                                     RMSE = RMSE_NO2_30p))


cor_results


#Now our algorithm has better RMSE and correlation coefficient on test_set than when we use 90% of data for training
#Finally, we will try to use only 50% data for training, leaving 50% for testing.

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = NO2_pollution_other$`NO2(GT)`, times = 1, p = 0.5, list = FALSE)
train_set_NO2_50p <- NO2_pollution_other[-test_index,]
test_set_NO2_50p <- NO2_pollution_other[test_index,]

#Linear Model (LM) using the sensors nominally targeted to other pollutants (O3, CO, NHMC and  NOx)
train_lm_NO2_50p <- train(`NO2(GT)` ~ `PT08.S1(CO)` + `PT08.S2(NMHC)` + `PT08.S3(NOx)` + `PT08.S5(O3)`, 
                          method = "lm", data = train_set_NO2_50p)

train_set_NO2_50p <- train_set_NO2_50p %>% 
  mutate(predicted_NO2_lm = predict(train_lm_NO2_50p, train_set_NO2_50p))

RMSE(train_set_NO2_50p$`NO2(GT)`, train_set_NO2_50p$predicted_NO2_lm)
#33.11278
cor(train_set_NO2_50p$`NO2(GT)`, train_set_NO2_50p$predicted_NO2_lm)
#0.7189593


#prediction on test set
test_set_NO2_50p <- test_set_NO2_50p %>% 
  mutate(predicted_NO2_lm = predict(train_lm_NO2_50p, test_set_NO2_50p))

RMSE_NO2_50p <- RMSE(test_set_NO2_50p$`NO2(GT)`, test_set_NO2_50p$predicted_NO2_lm)
RMSE_NO2_50p
#32.5973
cor_NO2_50p <- cor(test_set_NO2_50p$`NO2(GT)`, test_set_NO2_50p$predicted_NO2_lm)
cor_NO2_50p
#0.7290529

cor_results <- bind_rows(cor_results,
                         tibble(Method = "Linear model, sensors S1, S2, S3, S5, test set 50%", 
                                    Correlation = cor_NO2_50p,
                                    RMSE = RMSE_NO2_50p))


cor_results



#We diminished the amount of data for training and this worsened the resulting RMSE and correlation coefficient

#We find the split of data as 70% for training and 30% for testing optimal and we will implement 
#a couple of other models.  



#Generalized Linear Model (GLM) using the sensors nominally targeted to two pollutants 
#with the strongest positive and negative correlation with the level of NO2 (NOx and O3)

train_glm <- train(`NO2(GT)` ~ `PT08.S3(NOx)` + `PT08.S5(O3)`, 
                  method = "glm", data = train_set_NO2_30p)

train_set_NO2_30p <- train_set_NO2_30p %>% 
  mutate(predicted_NO2_glm = predict(train_glm, train_set_NO2_30p))

RMSE(train_set_NO2_30p$`NO2(GT)`, train_set_NO2_30p$predicted_NO2_glm)
#33.15343
cor(train_set_NO2_30p$`NO2(GT)`, train_set_NO2_30p$predicted_NO2_glm)
#0.71477578


#prediction on test set
test_set_NO2_30p <- test_set_NO2_30p %>% 
  mutate(predicted_NO2_glm = predict(train_glm, test_set_NO2_30p))

RMSE_NO2_glm_30p <- RMSE(test_set_NO2_30p$`NO2(GT)`, test_set_NO2_30p$predicted_NO2_glm)
RMSE_NO2_glm_30p
#32.20375
cor_NO2_glm_30p <- cor(test_set_NO2_30p$`NO2(GT)`, test_set_NO2_30p$predicted_NO2_glm)
cor_NO2_glm_30p
#0.7441665

cor_results <- bind_rows(cor_results,
                         tibble(Method = "Generalized Linear Model, sensors S3, S5, test set 30%", 
                                    Correlation = cor_NO2_glm_30p,
                                    RMSE = RMSE_NO2_glm_30p))


cor_results




#The result is a bit better than in linear model with all the sensors except nominally targeted to NO2, 
#but correlation coefficient is still lower than our threshold (0.75)




#Random Forest (RF) using the sensors nominally targeted to other pollutants (O3, CO, NHMC and  NOx)
train_rf <- train(`NO2(GT)` ~ `PT08.S1(CO)` + `PT08.S2(NMHC)` + `PT08.S3(NOx)` + `PT08.S5(O3)`, 
                  method = "rf", data = train_set_NO2_30p)


train_set_NO2_30p <- train_set_NO2_30p %>% 
  mutate(predicted_NO2_rf = predict(train_rf, train_set_NO2_30p))

RMSE(train_set_NO2_30p$`NO2(GT)`, train_set_NO2_30p$predicted_NO2_rf)
#13.3 (actual value differs every time the code is executed)
cor(train_set_NO2_30p$`NO2(GT)`, train_set_NO2_30p$predicted_NO2_rf)
#0.966 (actual value differs every time the code is executed)

#prediction on test set
test_set_NO2_30p <- test_set_NO2_30p %>% 
  mutate(predicted_NO2_rf = predict(train_rf, test_set_NO2_30p))

RMSE_NO2_rf_30p <- RMSE(test_set_NO2_30p$`NO2(GT)`, test_set_NO2_30p$predicted_NO2_rf)
RMSE_NO2_rf_30p
#29.09 (actual value differs every time the code is executed)
cor_NO2_rf_30p <- cor(test_set_NO2_30p$`NO2(GT)`, test_set_NO2_30p$predicted_NO2_rf)
cor_NO2_rf_30p
#0.80 (actual value differs every time the code is executed)

cor_results <- bind_rows(cor_results,
                         tibble(Method = "Random Forest, sensors S1, S2, S3, S5, test set 30%", 
                                    Correlation = cor_NO2_rf_30p,
                                    RMSE = RMSE_NO2_rf_30p))

cor_results

#Using the Random Forest method and the values of sensors nominally targeted to CO. NHMC, NOx and O3, 
#we predicted the level of NO2 with the highest correlation and the lowest RMSE than using linear models. 



#But can we predict the level of NO2 using the sensor nominally targeted to NO2, and the temperature?
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = NO2_pollution$`NO2(GT)`, times = 1, p = 0.3, list = FALSE)
train_set_NO2_T <- NO2_pollution[-test_index,]
test_set_NO2_T <- NO2_pollution[test_index,]

#Linear Model (LM) using the sensor nominally targeted to NO2 and the values of temperature
train_lm_NO2_T <- train(`NO2(GT)` ~ `PT08.S4(NO2)` + T, 
                          method = "lm", data = train_set_NO2_T)

train_set_NO2_T <- train_set_NO2_T %>% 
  mutate(predicted_NO2_lm = predict(train_lm_NO2_T, train_set_NO2_T))

RMSE(train_set_NO2_T$`NO2(GT)`, train_set_NO2_T$predicted_NO2_lm)
#44.04742
cor(train_set_NO2_T$`NO2(GT)`, train_set_NO2_T$predicted_NO2_lm)
#0.3696866


#prediction on test set
test_set_NO2_T <- test_set_NO2_T %>% 
  mutate(predicted_NO2_lm = predict(train_lm_NO2_T, test_set_NO2_T))

RMSE_NO2_T <- RMSE(test_set_NO2_T$`NO2(GT)`, test_set_NO2_T$predicted_NO2_lm)
RMSE_NO2_T
#44.50329
cor_NO2_T <- cor(test_set_NO2_T$`NO2(GT)`, test_set_NO2_T$predicted_NO2_lm)
cor_NO2_T
#0.3815589

cor_results <- bind_rows(cor_results,
                         tibble(Method = "Linear model, sensor S4 (targeted to NO2) and Temperature, test set 30%", 
                                    Correlation = cor_NO2_T,
                                    RMSE = RMSE_NO2_T))


cor_results

#The predictions made by this model have the lowest correlation with actual values in spite of use of 
#the values of the sensor nominally targeted to NO2 



#Random Forest model provided the best results. We could try to predict the level of benzene 
#using the available sensors (we do not have the sensor nominally targeted to benzene in the dataset)

#We make the dataframe containing the actual levels of benzene and also the responses of all the sensors
Benzene_pollution <- AirQuality %>% 
  select(Timestamp, `C6H6(GT)`, `PT08.S1(CO)`, `PT08.S2(NMHC)`, `PT08.S3(NOx)`, `PT08.S4(NO2)`,  `PT08.S5(O3)`) %>%
  filter(`C6H6(GT)` > 0) %>%
  filter(`PT08.S1(CO)` >0) %>%
  filter(`PT08.S2(NMHC)` >0) %>%
  filter(`PT08.S3(NOx)` >0) %>%
  filter(`PT08.S4(NO2)` >0) %>%
  filter(`PT08.S5(O3)` >0)

#We check the correlation between the responses of each sensor and actual level of pollutant benzene

cor(Benzene_pollution$`C6H6(GT)`, Benzene_pollution$`PT08.S1(CO)`)
#0.8837951 

cor(Benzene_pollution$`C6H6(GT)`, Benzene_pollution$`PT08.S2(NMHC)`)
#0.9819503

cor(Benzene_pollution$`C6H6(GT)`, Benzene_pollution$`PT08.S3(NOx)`)
#-0.7357441 

cor(Benzene_pollution$`C6H6(GT)`, Benzene_pollution$`PT08.S4(NO2)`)
#0.7657314 

cor(Benzene_pollution$`C6H6(GT)`, Benzene_pollution$`PT08.S5(O3)`)
#0.8656885


#the responses of all 5 sensors show high correlation with the level of benzene (positive or negative)
#We will use Random Forest Model which showed relatively good performance in predicting of NO2 level. 

#Test set will be 30% of Benzene_pollution dataframe, the other data (70%) will be used for training (train_set)
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = Benzene_pollution$`C6H6(GT)`, times = 1, p = 0.3, list = FALSE)
train_set_benzene <- Benzene_pollution[-test_index,]
test_set_benzene <- Benzene_pollution[test_index,]



#Random Forest (RF) using the sensors nominally targeted to other pollutants (O3, CO, NHMC and  NOx)
train_benzene_rf <- train(`C6H6(GT)` ~ `PT08.S1(CO)` + `PT08.S2(NMHC)` + `PT08.S3(NOx)` + `PT08.S4(NO2)` + `PT08.S5(O3)`, 
                  method = "rf", data = train_set_benzene)


train_set_benzene <- train_set_benzene %>% 
  mutate(predicted_benzene_rf = predict(train_benzene_rf, train_set_benzene))

RMSE(train_set_benzene$`C6H6(GT)`, train_set_benzene$predicted_benzene_rf)
#0.03218301
cor(train_set_benzene$`C6H6(GT)`, train_set_benzene$predicted_benzene_rf)
#0.9999908

#prediction on test set
test_set_benzene <- test_set_benzene %>% 
  mutate(predicted_benzene_rf = predict(train_benzene_rf, test_set_benzene))


RMSE(test_set_benzene$`C6H6(GT)`, test_set_benzene$predicted_benzene_rf)
#0.302306
cor(test_set_benzene$`C6H6(GT)`, test_set_benzene$predicted_benzene_rf)
#0.9991759

#Thus we built the algorithm to predict the level of pollutant benzene with the correlation over 0.999 without 
#use of any sensor nominally targeted to benzene.    


test_set_benzene %>%
  ggplot(aes(predicted_benzene_rf, `C6H6(GT)`)) +
  geom_point(size = 1) + 
  xlab("Predicted benzene concentration, microg/m3") +
  ylab("Hourly benzene concentration, microg/m3") +
  ggtitle("Prediction of benzene concentration, test set 30%, Random Forest Model")


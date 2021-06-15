#Load the Data#
library(tidyverse)
library(caret)
total_accidents_2005_to_2014 <- rbind(accidents_2005_to_2007,accidents_2009_to_2011, accidents_2012_to_2014)

#split the total_accidents_2005_to_2014 data set into a 90% CYO dataset and 10% VALIDATION dataset#
set.seed(1)
test_index <- createDataPartition(y = total_accidents_2005_to_2014$Accident_Severity, times = 1, p = 0.1, list = FALSE)
CYO <- total_accidents_2005_to_2014[-test_index,]
temp <- total_accidents_2005_to_2014[test_index,]
Validation <- temp

#Split the CYO dataset into an 80% training set and 20% testing set#
set.seed(1)
test_index <- createDataPartition(y = CYO$Accident_Severity , times = 1, p = 0.2, list = FALSE)
testing_set <- total_accidents_2005_to_2014[-test_index,]
training_set <- total_accidents_2005_to_2014[test_index,]

##Data Exploration

#Data set summary
summary(CYO)

#Light Conditions
CYO %>% group_by(Light_Conditions) %>% summarise(n=n())
CYO %>% ggplot(aes(Light_Conditions)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Light Conditions Accident Count Distribution")

#Day of the Week
CYO %>% group_by(Day_of_Week) %>% summarise(n=n())
CYO %>% ggplot(aes(Day_of_Week)) + geom_bar() + ggtitle("Accident Count per Day Distribution")

#Road Surface Conditions
CYO %>% group_by(Road_Surface_Conditions) %>% summarise(n=n())
CYO %>% ggplot(aes(Road_Surface_Conditions)) + geom_bar() + ggtitle("Road Surface Accident Count Distribution") + scale_x_discrete(labels=c("Flood (Over 3cm of water)" = "Flood"))

#Speed Limit
CYO %>% group_by(Speed_limit) %>% summarise(n=n())
CYO %>% ggplot(aes(Speed_limit)) + geom_bar() + ggtitle("Speed Limit Accident Count Distribution") 

#Road Type
CYO %>% group_by(Road_Type) %>% summarise(n=n())
CYO %>% ggplot(aes(Road_Type)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Road Type Accident Count Distribution")

#Weather Conditions
CYO %>% group_by(Weather_Conditions) %>% summarise(n=n())
CYO %>% ggplot(aes(Weather_Conditions)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Weather Conditions Accident Count Distribution")

#Urban or Rural
CYO %>% group_by(Urban_or_Rural_Area) %>% summarise(n=n())
CYO %>% ggplot(aes(Urban_or_Rural_Area)) + geom_bar() + ggtitle("Urban or Rural Accident Count Distribution")


#Initial Model
mu1 <- mean(training_set$Accident_Severity)
mu1

#Light Condition Model
light_conditions <- training_set %>%
  group_by(Light_Conditions) %>%
  summarize(lc = mean(Accident_Severity - mu1))

predictions_lighting_conditions <- mu1 + testing_set %>% 
  left_join(light_conditions, by = "Light_Conditions") %>% 
  pull(lc)

rmse_lighting_conditions <- RMSE(testing_set$Accident_Severity, predictions_lighting_conditions)
rmse_lighting_conditions

#Light Condition + Day of the Week model
day_of_week <- training_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  group_by(Day_of_Week) %>% 
  summarize(dw = mean(Accident_Severity - mu1 - lc))

predictions_day_of_week <- testing_set %>% 
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  mutate(predict = mu1 + lc + dw) %>%
  pull(predict)

rmse_day_of_week <- RMSE(testing_set$Accident_Severity, predictions_day_of_week)
rmse_day_of_week

#Light Condition + Day of the Week + Road Surface Conditions model
road_surface_conditions <- training_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  group_by(Road_Surface_Conditions) %>% 
  summarize(rc = mean(Accident_Severity - mu1 - lc - dw))
  
predictions_road_surface_conditions <- testing_set %>% 
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  mutate(predict = mu1 + lc + dw + rc) %>%
  pull(predict)

rmse_road_surface_conditions <- RMSE(testing_set$Accident_Severity, predictions_road_surface_conditions)
rmse_road_surface_conditions

#Light Condition + Day of the Week + Road Surface Conditions + Speed Limit model
speed_limit <- training_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  group_by(Speed_limit) %>% 
  summarize(sl = mean(Accident_Severity - mu1 - lc - dw - rc))

predictions_speed_limit <- testing_set %>% 
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by ="Speed_limit") %>%
  mutate(predict = mu1 + lc + dw + rc +sl) %>%
  pull(predict)

rmse_speed_limit <- RMSE(testing_set$Accident_Severity, predictions_speed_limit)
rmse_speed_limit


#Light Condition + Day of the Week + Road Surface Conditions + Speed Limit + Road Type model
road_type <- training_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by = "Speed_limit") %>%
  group_by(Road_Type) %>% 
  summarize(rt = mean(Accident_Severity - mu1 - lc - dw - rc - sl))
  
predictions_road_type <- testing_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by ="Speed_limit") %>%
  left_join(road_type, by = "Road_Type") %>%
  mutate(predict = mu1 + lc + dw + rc +sl + rt) %>%
  pull(predict)

rmse_road_type <- RMSE(testing_set$Accident_Severity, predictions_road_type)
rmse_road_type

#Light Condition + Day of the Week + Road Surface Conditions + Speed Limit + Road Type + Weather Conditions model
weather_conditions <- training_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by = "Speed_limit") %>%
  left_join(road_type, by = "Road_Type") %>%
  group_by(Weather_Conditions) %>% 
  summarize(wc = mean(Accident_Severity - mu1 - lc - dw - rc - sl + rt))

predictions_weather_conditions <- testing_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by ="Speed_limit") %>%
  left_join(road_type, by = "Road_Type") %>%
  left_join(weather_conditions, by = "Weather_Conditions") %>%
  mutate(predict = mu1 + lc + dw + rc +sl + rt + wc) %>%
  pull(predict)

rmse_weather_conditions <- RMSE(testing_set$Accident_Severity, predictions_weather_conditions)
rmse_weather_conditions

#Light Condition + Day of the Week + Road Surface Conditions + Speed Limit + Road Type + Weather Conditions + Urban or Rural model
urban_or_rural <- training_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by = "Speed_limit") %>%
  left_join(road_type, by = "Road_Type") %>%
  left_join(weather_conditions, by = "Weather_Conditions") %>%
  group_by(Urban_or_Rural_Area) %>% 
  summarize(ur = mean(Accident_Severity - mu1 - lc - dw - rc - sl + rt + wc))

predictions_urban_or_rural <- testing_set %>%
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by ="Speed_limit") %>%
  left_join(road_type, by = "Road_Type") %>%
  left_join(weather_conditions, by = "Weather_Conditions") %>%
  left_join(urban_or_rural, by = "Urban_or_Rural_Area") %>%
  mutate(predict = mu1 + lc + dw + rc +sl + rt + wc + ur) %>%
  pull(predict)

rmse_urban_or_rural <- RMSE(testing_set$Accident_Severity, predictions_urban_or_rural)
rmse_urban_or_rural

#Regularization Model
mu1 <- mean(training_set$Accident_Severity)

lambda <- seq(0, 10, 0.25)

rmse <- sapply(lambda, function(lmd){
  
  light_conditions <- training_set %>%
    group_by(Light_Conditions) %>%
    summarize(lc = mean(Accident_Severity - mu1)/(n()+lmd))
  
  day_of_week <- training_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    group_by(Day_of_Week) %>% 
    summarize(dw = mean(Accident_Severity - mu1 - lc)/(n()+lmd))
  
  road_surface_conditions <- training_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    left_join(day_of_week, by = 'Day_of_Week') %>%
    group_by(Road_Surface_Conditions) %>% 
    summarize(rc = mean(Accident_Severity - mu1 - lc - dw)/(n()+lmd))
  
  speed_limit <- training_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    left_join(day_of_week, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
    group_by(Speed_limit) %>% 
    summarize(sl = mean(Accident_Severity - mu1 - lc - dw - rc)/(n()+lmd))
  
  road_type <- training_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    left_join(day_of_week, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit, by = "Speed_limit") %>%
    group_by(Road_Type) %>% 
    summarize(rt = mean(Accident_Severity - mu1 - lc - dw - rc - sl)/(n()+lmd))
 
  weather_conditions <- training_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    left_join(day_of_week, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit, by = "Speed_limit") %>%
    left_join(road_type, by = "Road_Type") %>%
    group_by(Weather_Conditions) %>% 
    summarize(wc = mean(Accident_Severity - mu1 - lc - dw - rc - sl + rt)/(n()+lmd))
  
  urban_or_rural <- training_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    left_join(day_of_week, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit, by = "Speed_limit") %>%
    left_join(road_type, by = "Road_Type") %>%
    left_join(weather_conditions, by = "Weather_Conditions") %>%
    group_by(Urban_or_Rural_Area) %>% 
    summarize(ur = mean(Accident_Severity - mu1 - lc - dw - rc - sl + rt + wc)/(n()+lmd))
  
  
  predictions_total <- testing_set %>%
    left_join(light_conditions, by = "Light_Conditions") %>% 
    left_join(day_of_week, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit, by ="Speed_limit") %>%
    left_join(road_type, by = "Road_Type") %>%
    left_join(weather_conditions, by = "Weather_Conditions") %>%
    left_join(urban_or_rural, by = "Urban_or_Rural_Area") %>%
    mutate(predict = mu1 + lc + dw + rc +sl + rt + wc + ur) %>%
    pull(predict)
  
  RMSE(predictions_total, testing_set$Accident_Severity)
})


qplot(lambda, rmse)

lowest_rmse <- rmse[which.min(rmse)]
lowest_rmse


lowest_lambda <- lambda[which.min(rmse)]
lowest_lambda

#Validation run
mu1 <- mean(CYO$Accident_Severity)

lambda_validation <- seq(0, 10, 0.25)

rmse_validation <- sapply(lambda_validation, function(lmd){
  
  light_conditions_validation <- CYO %>%
    group_by(Light_Conditions) %>%
    summarize(lc = mean(Accident_Severity - mu1)/(n()+lmd))
  
  day_of_week_validation <- CYO %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    group_by(Day_of_Week) %>% 
    summarize(dw = mean(Accident_Severity - mu1 - lc)/(n()+lmd))
  
  road_surface_conditions_validation <- CYO %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    left_join(day_of_week_validation, by = 'Day_of_Week') %>%
    group_by(Road_Surface_Conditions) %>% 
    summarize(rc = mean(Accident_Severity - mu1 - lc - dw)/(n()+lmd))
  
  speed_limit_validation <- CYO %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    left_join(day_of_week_validation, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions_validation, by = "Road_Surface_Conditions") %>%
    group_by(Speed_limit) %>% 
    summarize(sl = mean(Accident_Severity - mu1 - lc - dw - rc)/(n()+lmd))
  
  road_type_validation <- CYO %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    left_join(day_of_week_validation, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions_validation, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit_validation, by = "Speed_limit") %>%
    group_by(Road_Type) %>% 
    summarize(rt = mean(Accident_Severity - mu1 - lc - dw - rc - sl)/(n()+lmd))
  
  weather_conditions_validation <- CYO %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    left_join(day_of_week_validation, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions_validation, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit_validation, by = "Speed_limit") %>%
    left_join(road_type_validation, by = "Road_Type") %>%
    group_by(Weather_Conditions) %>% 
    summarize(wc = mean(Accident_Severity - mu1 - lc - dw - rc - sl + rt)/(n()+lmd))
  
  urban_or_rural_validation <- CYO %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    left_join(day_of_week_validation, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions_validation, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit_validation, by = "Speed_limit") %>%
    left_join(road_type_validation, by = "Road_Type") %>%
    left_join(weather_conditions_validation, by = "Weather_Conditions") %>%
    group_by(Urban_or_Rural_Area) %>% 
    summarize(ur = mean(Accident_Severity - mu1 - lc - dw - rc - sl + rt + wc)/(n()+lmd))
  
  
  predictions_total_validation <- Validation %>%
    left_join(light_conditions_validation, by = "Light_Conditions") %>% 
    left_join(day_of_week_validation, by = 'Day_of_Week') %>%
    left_join(road_surface_conditions_validation, by = "Road_Surface_Conditions") %>%
    left_join(speed_limit_validation, by ="Speed_limit") %>%
    left_join(road_type_validation, by = "Road_Type") %>%
    left_join(weather_conditions_validation, by = "Weather_Conditions") %>%
    left_join(urban_or_rural_validation, by = "Urban_or_Rural_Area") %>%
    mutate(predict_validation = mu1 + lc + dw + rc +sl + rt + wc + ur) %>%
    pull(predict_validation)
  
  RMSE(predictions_total_validation, Validation$Accident_Severity)
})


qplot(lambda_validation, rmse_validation)

lowest_rmse_validation <- rmse_validation[which.min(rmse_validation)]
lowest_rmse_validation


lowest_lambda_validation <- lambda_validation[which.min(rmse_validation)]
lowest_lambda_validation



#Prediction list of the top 15 most severe accidents
Final_List <- Validation %>% 
  left_join(light_conditions, by = "Light_Conditions") %>% 
  left_join(day_of_week, by = 'Day_of_Week') %>%
  left_join(road_surface_conditions, by = "Road_Surface_Conditions") %>%
  left_join(speed_limit, by ="Speed_limit") %>%
  left_join(road_type, by = "Road_Type") %>%
  left_join(weather_conditions, by = "Weather_Conditions") %>%
  left_join(urban_or_rural, by = "Urban_or_Rural_Area") %>%
  mutate(predict = mu1 + lc + dw + rc +sl + rt + wc + ur) %>%
  arrange(-predict) %>% 
  group_by(Accident_Index) %>% 
  select(Accident_Index) %>%
  head(15)

Final_List
View(Final_List) 





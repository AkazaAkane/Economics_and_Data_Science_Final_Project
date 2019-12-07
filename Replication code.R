#Predicting House Price with Different Models
# group 10: Alisa Sima, Andy Yu, August(Yuhao) Chen, Harrison Lian


#Library Used in Project
library(knitr)
library(pander)
library(readr)
library(magrittr)
library(car)
library(tidyverse)
library(gmodels)
library(foreign)
library(AER)
library(xgboost)
library(caret)
library(randomForest)
library(neuralnet)
library(stargazer)
library(broom)
library(corrplot)
library(knitr)
library(reshape2)

#load data
houses <- read_csv("kc_house_data.csv")

#Data Prepocessing

# as factor catagorical data
houses$zipcode <- as.factor(houses$zipcode)
houses$view <- as.factor(houses$view)
houses$condition <- as.factor(houses$condition)
houses$grade <- as.factor(houses$grade)

names(houses)
renovated <- houses[houses$yr_renovated!=0,]
notrenovated <- houses[houses$yr_renovated==0,]
nrow(renovated)
nrow(notrenovated)

## encode date into year,month and date
houses <- houses %>%
  select (-c(id)) %>%
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date)) %>%
  select (-c(date))

houses$renovated <- rep(0, n=nrow(houses))
houses$renovated <- ifelse(houses$yr_renovated!=0, 1, houses$renovated)
houses$basement <- ifelse(houses$sqft_basement!=0, 1, houses$renovated)

#EDA
## Signle variative EDA

renovated <- houses[houses$yr_renovated!=0,]
notrenovated <- houses[houses$yr_renovated==0,]

price_den <- ggplot(houses, aes(x = price)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Houses' Price (dollars)", y = "Density",
       title = "The Distribution of House Prices") +
  theme_bw()

bedroom_den <- ggplot(houses, aes(x = bedrooms)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Number of Bedrooms", y = "Density",
       title = "The Distribution of Number of Bedrooms") +
  theme_bw()

bathroom_den <- ggplot(houses, aes(x = bathrooms)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Number of Bathrooms", y = "Density",
       title = "The Distribution of Number of Bathrooms") +
  theme_bw()

living_den <- ggplot(houses, aes(x = sqft_living)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "House’s Interior (sqft)", y = "Density",
       title = "The Distribution of Living Interior Living Space") +
  theme_bw()

lot_den <- ggplot(houses, aes(x = sqft_lot)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "House’s Land(sqft)", y = "Density",
       title = "The Distribution of House's Land") +
  theme_bw()

floors_bar <- ggplot(houses, aes(x = as.factor(floors), 
                                 fill = as.factor(floors))) +
  geom_bar(stat = "count") +
  labs(x = "Number of Floors", y = "Count of Houses",
       title = "The Distribution of Number of Floors") +
  theme_bw() + theme(legend.position = "none")

waterfront_bar <- ggplot(houses, aes(x = as.factor(waterfront), 
                                     fill = as.factor(waterfront))) +
  geom_bar(stat = "count") +
  labs(x = "Overlooking the Waterfront", y = "Count of Houses",
       title = "The Distribution of Houses with Waterfront") +
  theme_bw() + theme(legend.position = "none")

view_bar <- ggplot(houses, aes(x = as.factor(view), 
                               fill = as.factor(view))) +
  geom_bar(stat = "count") +
  labs(x = "How Good the View Is", y = "Count of Houses",
       title = "The Distribution of View") +
  theme_bw()+ theme(legend.position = "none")

condition_bar <- ggplot(houses, aes(x = as.factor(condition), 
                                    fill = as.factor(condition))) +
  geom_bar(stat = "count") +
  labs(x = "Condition of the Houses", y = "Count of Houses",
       title = "The Distribution of Condition") +
  theme_bw() + theme(legend.position = "none")

grade_bar <- ggplot(houses, aes(x = as.factor(grade), 
                                fill = as.factor(grade))) +
  geom_bar(stat = "count") +
  labs(x = "Grade of the Houses", y = "Count of Houses",
       title = "The Distribution of Grade") +
  theme_bw() + theme(legend.position = "none")

above_den <- ggplot(houses, aes(x = sqft_above)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Above Ground Level (sqft)", y = "Density",
       title = "The Distribution of Living Space Above Area") +
  theme_bw()

basement_den <- ggplot(houses, aes(x = sqft_basement)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Basement (sqft)", y = "Density",
       title = "The Distribution of Basement Area") +
  theme_bw()

yrbuilt_den <- ggplot(houses, aes(x = yr_built)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Year Built", y = "Density",
       title = "The Distribution of Year Built") +
  theme_bw()

renovated_bar <- ggplot(houses, aes(x = as.factor(renovated), fill = as.factor(renovated))) +
  geom_bar(stat = "count") +
  labs(x = "Renovated or Not", y = "Count of Houses",
       title = "The Distribution of Renovated or Not") +
  theme_bw() + theme(legend.position = "none")

living15_den <- ggplot(houses, aes(x = sqft_living15)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "House’s Interior of the Nearest 15 Neighbors (sqft)", y = "Density",
       title = "The Distribution of Living Interior Living Space for Neighbors") +
  theme_bw()

lot15_den <- ggplot(houses, aes(x = sqft_lot15)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "House’s Land of the Nearest 15 Neighbors (sqft)", y = "Density",
       title = "The Distribution of Land Area for Neighbors") +
  theme_bw()

price_den
bedroom_den
bathroom_den 
living_den
lot_den
floors_bar
waterfront_bar
view_bar
condition_bar
grade_bar
above_den
basement_den
yrbuilt_den
renovated_bar
living15_den
lot15_den

# summary of quantitative variables
summary(houses$price)

summary(houses$bedrooms)

summary(houses$bathrooms)

summary(houses$sqft_living)

summary(houses$sqft_lot)

summary(houses$sqft_basement)

summary(houses$sqft_living15)

summary(houses$sqft_lot15)


#plot log(price)
logprice_den <- ggplot(houses, aes(x = log(price))) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(x = "Log Houses' Price (dollars)", y = "Density",
       title = "The Distribution of Log of House Price") +
  theme_bw()

logprice_den


## Bivariative EDA

source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
house_data <- houses[, c(1,2,3,4,5,6,11,12,18,19,20,21,22)]
rquery.cormat(house_data)

(ggplot(houses, aes(x = as.factor(renovated), y = log(price))) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    labs(x = "Renovated or Not", y = "log House Price",
         title = "Distribution of log of House Price based on Renovation") +
    theme_bw())

(ggplot(houses, aes(x = as.factor(waterfront), y = log(price))) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    labs(x = "Waterfront or Not", y = "log House Price",
         title = "Distribution of log of House Price based on Waterfront") +
    theme_bw())

(ggplot(houses, aes(x = as.factor(view), y = log(price))) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    labs(x = "View", y = "log House Price",
         title = "Distribution of log of House Price based on View") +
    theme_bw())

(ggplot(houses, aes(x = as.factor(condition), y = log(price))) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    labs(x = "Condition", y = "log House Price",
         title = "Distribution of log of House Price based on Condition") +
    theme_bw())

(ggplot(houses, aes(x = as.factor(grade), y = log(price))) +
    geom_boxplot(fill = "orange", alpha = 0.5) +
    labs(x = "Grade", y = "log House Price",
         title = "Distribution of log of House Price based on Grade") +
    theme_bw())

kingcounty <- c(-121.8400-1, 47.4700-0.5,-121.8400+0.5, 47.4700+0.5)
map_base <- get_stamenmap(kingcounty, maptype = "toner-lite", zoom = 8)

map_object <- ggmap(map_base, extent = "device")

mapPoints <- ggmap(map_base) +
  geom_point(aes(x = long, y = lat), 
             data = houses, alpha = .02) +
  labs(x = "Longitude", y = "Latitude", 
       title = "Distribution of Houses") + 
  theme(legend.position = "none")

mapPoints



# Linear Regression Modelling 

## Set Seed so that same sample can be reproduced
set.seed(101) 

## Now Selecting 75% of data as training set and Drop variables
sample <- sample.int(n = nrow(houses), size = floor(.75*nrow(houses)), replace = F)
train <- houses[sample,]
train_clean <- within(train, rm( 'date','yr_built','zipcode','lat','long','yr_renovated','year','month','day','sqft_basement'))
test <- houses[-sample,]
test_clean <- within(test, rm( 'date','yr_built','zipcode','lat','long','yr_renovated','year','month','day','sqft_basement'))

#Linear Regression without log transformation
model_1 <- lm(price ~.,data = train_clean)
summary(model_1)


#condition checking
residualPlot(model_1)
qqPlot(model_1)

#Linear Regression with log transformation
model_2 <- lm(log(price) ~.,data=train_clean)
residualPlot(model_2)
qqPlot(model_2)
summary(model_2)


vif(model_2)
#we find sqft living and sqft above may have high collinearity

#diagnostics

#F-test doesn't make sense since we don't have a reduced/expanded model

#leverage
#3(k+1)/n = 3(15+1)/16209 = .002961318

train_clean$lev <- hatvalues(model_2)

#arranges our data from highest leverage to lowest leverage
train_clean <- train_clean %>% 
  arrange(desc(lev))

#gives us number of high leverage values
train_clean %>% 
  filter(lev>(3*(15+1)/16209)) %>% 
  nrow()

#create new data frame with high leverage values removed
train_clean_sub <- train_clean %>%
  filter(lev <= 3*(15+1)/16209)

#lev should not be an explanatory variable for this model so we drop it
train_clean_sub <- train_clean_sub %>%
  select(-lev)

#Linear Regression with 
model_3 <- lm(I(log(price)) ~.,data=train_clean_sub)
summary(model_3)


#view model_3
stargazer(model_3, type="text", title = "Table 1: Linear Regression House Price Prediction")


#we consiered making another model by only dropping cooks distances
cook <- cooks.distance(model_2)

train_clean$cook <- cook

#gives us number of high cook's distance values
train_clean %>% 
  filter(cook>.5) %>% 
  nrow()

#only 2 high cook distance values so we don't drop

#Machine Learning Models

## ML model_1 gradient boosting
set.seed(123)
#fit model
ml_model <- train(
  price ~., data = train_clean, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter mtry
ml_model$bestTune
# Make predictions on the test data
predictions <- ml_model %>% predict(test_clean)
head(predictions)
# Compute the average prediction error RMSE and MAE
RMSE(predictions, test_clean$price)
MAE(predictions, test_clean$price)

## ML model2:random forest
set.seed(124)
#fit model
mlmodel_2 <- train(
  price ~., data = train_clean, method = "rf",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter mtry
mlmodel_2$bestTune
# Make predictions on the test data
predictions_2 <- mlmodel_2 %>% predict(test_clean)
head(predictions_2)
# Compute the average prediction error RMSE and MAE
RMSE(predictions_2, test_clean$price)
MAE(predictions_2, test_clean$price)

#one hot encoding for neural network
onehotencoder <- function(df_orig) {
  df<-cbind(df_orig)
  df_clmtyp<-data.frame(clmtyp=sapply(df,class))
  df_col_typ<-data.frame(clmnm=colnames(df),clmtyp=df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    if (df_col_typ[rownm,"clmtyp"]=="factor") {
      clmn_obj<-df[toString(df_col_typ[rownm,"clmnm"])] 
      dummy_matx<-data.frame(model.matrix( ~.-1, data = clmn_obj))
      dummy_matx<-dummy_matx[,c(1,3:ncol(dummy_matx))]
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
      df<-cbind(df,dummy_matx)
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
    }  }
  return(df)
}

train_nn <- onehotencoder(train_clean)
test_nn <- onehotencoder(test_clean)
str(train_nn)

#z-score standardize data for neural network
tk <- normalize(train_nn, method = "standardize", range = c(0, 1))
ttk <- normalize(test_nn, method = "standardize", range = c(0, 1))

#set seed 
set.seed(125)
#train neural network
mlmodel_3 = neuralnet(price ~.,data=tk, hidden=c(4,2), threshold = 0.5, 
                      linear.output = T, lifesign = "full", stepmax = 1000000)
#plot the network
attributes(mlmodel_3)$names
plot(mlmodel_3)

# Make predictions on the test data
predictions_3 <- predict(mlmodel_3,ttk)
RMSE(predictions_3, ttk$price) 



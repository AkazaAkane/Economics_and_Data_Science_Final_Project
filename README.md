# Economics and Data Science (73-265) Project:
##Predicting Housing Prices in King County

For the final project, we did an analysis on the King County house price data.

## Introduction
King County is the most populous county in Washington with over 2 million
residents and the 12th most populous county in the United States. It is also
home to Microsoft and Amazon, two of the largest tech companies. Given that
Seattle is a hub of innovation and attractive place to live, the population has increased over time, thus increasing demand for housing. Realtors interested in selling their houses may want to pay close attention to what factors influence
the price of a house. Thus, we are interested in analyzing what variables affect housing prices in King County and ultimately try to predict the price.

## Data
We obtained our dataset from [Kaggle](https://www.kaggle.com/harlfoxem/housesalesprediction). Our data consists of 21,613 observations
and 21 variables, where each observation represents a home sold between May 2014
and May 2015.

Due to our interest in analyzing and predicting the house price, we examine the relationship between house prices and other explanatory variables. The
descriptions of the variables are as follows:

## Explanatory Data Analysis
After looking at both the graphs and the summary statistics of our variables, we make the following observations: the distributions of most of our numeric variables are skewed to the right, the mean of these distributions being higher than their respective medians.

We notice that our response variable price is extremely skewed. Thus, a log transformation may normalize it. This transformation will be useful when we examine both price and log(price) in our linear regression models.

## Bivariate Exploration
First, we create a correlation plot between the numeric variables in the data set. In the graph below, red circles denote a positive correlation and blue circles denote a negative correlation. We can see that price and day, month, year, almost have no correlation. It has some strong correlation with sqft_living15, sqft_living, and sqft_above. It also has some weak positive correlation with the rest of the numeric variables.

We also see that there seems to be some multicollinearity between some of the explanatory variables, such as sqft_living and bathrooms, sqft_living15 and sqft_above,  sqft_above and bathrooms, sqft_living15, and sqft_living.

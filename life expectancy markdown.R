---
  title: "Life Expectancy Predictions"
output: rmarkdown::github_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

###Loading required libraries

```{r}
rm(list=ls())
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("GGally")
install.packages("patchwork")
install.packages("corrplot")
install.packages("scales")
install.packages("RColorBrewer")
install.packages("leaps")
install.packages("ggfortify")
install.packages("MASS")
install.packages("regclass")
install.packages("olsrr")
install.packages("car")

require(ggplot2)
require(ggthemes)
require(tidyverse)
require(dplyr)
require(GGally)
require(patchwork)
require(corrplot)
require(scales)
require(RColorBrewer)
require(leaps)
require(ggfortify)
require(MASS)
require(regclass)
require(olsrr)
require(car)

```

#Loading and exploring the dataset 
```{r}
setwd("C:/Users/Nebula PC/Desktop/RFiles")
lifexp <- read.csv("Life Expectancy Data.csv")
count(lifexp)
colnames(lifexp)
unique(lifexp$Status) ###To check if its categorical

#Knowing more about the data types and values
str(lifexp)
summary(lifexp)

#Contains implausible outliers, will need to be removed

```
#Data cleaning exercise 

```{r}

#removing country and year columns, irrelevant to the analysis
lifexp <- lifexp[,3:22]

#Creating another variable without the categorical variable 'Status'
lifexpnum <- lifexp %>%
  dplyr:: select(Life.expectancy:Schooling)

##Checking for and removing rows with Na values
apply(is.na(lifexp), 2, which)
lifexp <- na.omit(lifexp)
apply(is.na(lifexp), 2, which)

apply(is.na(lifexpnum), 2, which)
lifexpnum <- na.omit(lifexpnum)
apply(is.na(lifexpnum), 2, which)

```

```{r}
summary(lifexp)
```

##Identifying outliers through boxplots
```{r}
comma <- function(x) {
  format(x, big.mark = ",")
}
#Life expectancy
ggplot(lifexp, aes(Status, Life.expectancy, fill=Status))+
  geom_boxplot() +
  scale_y_continuous(labels = comma)+
  ggtitle("Life expectancy")+
  scale_fill_brewer(palette="Dark2")

#Adult mortality
ggplot(lifexp, aes(Status, Adult.Mortality, fill=Status))+
  geom_boxplot() +
  ggtitle("Rate of death in adults per 1000")+
  scale_y_continuous(labels = comma)+
  scale_fill_brewer(palette="Dark2")

#BMI
ggplot(lifexp, aes(Status, BMI, fill=Status))+
  geom_boxplot() +
  scale_y_continuous(labels = comma)+
  ggtitle("BMI")+
  scale_fill_brewer(palette="Dark2")

#Measles
ggplot(lifexp, aes(Status, Measles, fill=Status))+
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = comma)+
  ylab("Number of cases")+
  ggtitle ("Measles cases reported per 1000 population") 

#HIV/AIDS
ggplot(lifexp, aes(Status, HIV.AIDS, fill=Status))+
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = comma)+
  ylab("Number of cases")+
  ggtitle ("HIV-AIDS cases reported per 1000 population")

#Infant deaths 
ggplot(lifexp, aes(Status, infant.deaths, fill=Status))+
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = comma)+
  ylab("Number of deaths")+
  ggtitle ("Infant deaths per 10000") 

#Income composition  
ggplot(lifexp, aes(Status, Income.composition.of.resources, fill=Status))+
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") +
  ylab("Income Composition of Resources")+
  ggtitle ("Income composition of Resources") 
```

#Spread of response variable 
```{r}
ggplot(lifexp, aes(Life.expectancy, color=Status))+
  geom_histogram(bins=30, fill="white", alpha=0.4)+
  theme_bw()
#slight left skew, transformation may be beneficial (address later)

```
#histogram of populations
```{r}
ggplot(lifexp, aes(Population, color=Status))+
  geom_histogram(bins=30, fill="white", alpha=0.4)+
  theme_bw()
#Strong right skew, will use a long transformation
lifexp$Population <- log(lifexp$Population)

ggplot(lifexp, aes(Population, color=Status))+
  geom_histogram(bins=30, fill="white", alpha=0.4)+
  theme_bw()

```

#Scatterplot
```{r}
samp <- lifexp %>%
  filter(Measles<1000)
ggplot(samp) +
  geom_point(aes(Measles, Life.expectancy, color=Status))+
  ggtitle ("Relationship bw measles cases per 1000 and Life Expectancy")+
  theme_bw()
#little to no relationship between measles and Life expectancy. 

```

```{r}
#Correlations 

corr_lifexp <- cor(lifexpnum)
View(corr_lifexp)

#Visualizing correlations through a heatmap
ggcorr(lifexpnum, label = TRUE, label_size = 2,label_round = 2, hjust = 1, size = 3, layout.exp = 6, name = "Correlation b/w all variables", low="#99CCFF", mid="#FFCC99", high="#990000")

```

```{r}
#Dropping variables that have high correlations with other independant variables, to avoid multicollinearity

lifexp_analysis <- lifexp %>%
  dplyr :: select(-c(infant.deaths, percentage.expenditure, Schooling, thinness.5.9.years))

lifexpnum_analysis <- lifexp_analysis[,2:16]

##Check the effect on correlation coefficients  

corr_lifexp2 <- cor(lifexpnum_analysis)

ggcorr(lifexpnum_analysis, label = TRUE, label_size = 2,label_round = 2, hjust = 1, size = 3, layout.exp = 6, name = "Correlation b/w all variables", low="#99CCFF", mid="#FFCC99", high="#990000")


##Checking some of the relationships from the correlations 
ggplot(lifexp_analysis, aes(Income.composition.of.resources, Life.expectancy, color=Status)) +
  geom_point(size=0.5, alpha=0.7)+
  ggtitle ("Income Vs Life expectancy")+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_color_brewer(palette="Dark2")
#Can see some outlier in terms of income composition (many 0 values), need to be removed
```
##Correcting for income composition 
```{r}
lifexp_analysis <- lifexp_analysis %>%
  filter(Income.composition.of.resources>0)

lifexpnum_analysis <- lifexp_analysis %>%
  dplyr :: select(-c(Status))

ggplot(lifexp_analysis, aes(Income.composition.of.resources, Life.expectancy, color=Status)) +
  geom_point(size=0.5, alpha=0.7)+
  ggtitle ("Income Vs Life expectancy")+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_color_brewer(palette="Dark2")

```
#Variable Selection 

##Leaps and bounds
```{r}

best_subset <- leaps(x=lifexpnum_analysis[,2:15], y=lifexpnum_analysis[,1], nbest=5, method="adjr2",names=names(lifexpnum_analysis)[-1])

fit <- data.frame(Size = best_subset$size, Criterion=round(best_subset$adjr2, 3), best_subset$which, row.names=NULL)
fit

ggplot(fit) +
  geom_point(aes(Size, Criterion)) +
  ggtitle(" Criterion scores for all models in Leaps and bounds")+
  theme_bw()

highest_of_each <- fit %>%
  filter(Size==4)
highest_of_each

#Thus our final model through this method will be: 
```
###Cp table for decision 
```{r}
#Getting the models with the max Adjusted Rsquared values for each number of size 
size_leaps <- fit %>%
  group_by(Size) %>%
  slice(which.max(Criterion))

#Full model 
full_model <- lm(Life.expectancy ~ Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources, data=lifexpnum_analysis)

#Size 4 model
model_4 <- lm(Life.expectancy ~Adult.Mortality, HIV.AIDS + Income.composition.of.resources, data=lifexpnum_analysis)

#Size 5 model 
model_5 <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + Income.composition.of.resources, data=lifexpnum_analysis)

#Size 6 model 
model_6 <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + Income.composition.of.resources + Total.expenditure, data=lifexpnum_analysis)

#Size_7 model 
model_7 <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + GDP + Income.composition.of.resources + Total.expenditure, data=lifexpnum_analysis)

#Size 8 model
model_8 <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + GDP + Income.composition.of.resources + Total.expenditure + Diphtheria, data=lifexpnum_analysis)


CP_PRESS <- function(model, sigma_full){
  res <- resid(model)
  hat_mod <- hatvalues(model)
  CP <- sum(res^2)/sigma_full + 2*length(coef(model)) - length(res)
  PRESS <- sum(res^2/(1-hat_mod)^2)
  list(Cp=CP, PRESS=PRESS)
}

sigma_q <- summary(full_model)$sigma^2

size4_stat <- CP_PRESS(model_4, sigma_q)
size5_stat <- CP_PRESS(model_5, sigma_q)
size6_stat <- CP_PRESS(model_6, sigma_q)
size7_stat <- CP_PRESS(model_7, sigma_q)
size8_stat <- CP_PRESS(model_8, sigma_q)

size4_stat
size5_stat
size6_stat
size7_stat
size8_stat


#Thus our final model through this method will be Leap size 5, which is: 
model_leaps <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + Income.composition.of.resources, data=lifexpnum_analysis)
summary(model_leaps)
```

##Forward selection
```{r}
options(scipen = 999) ##Getting rid of scientific notations 

colnames(lifexpnum_analysis)

#Forward selection modelling 

intercept_mod <- lm(Life.expectancy~1, data=lifexpnum_analysis)

forward <- add1(intercept_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#all variables seem to be significant 
#Among which Income composition seems to be most significant (with highest F value and lowest p value)

#So adding Income composition into the model
test_mod <- lm(Life.expectancy~Income.composition.of.resources, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#HIV.AIDS seems to be the most significant 

#So adding HIV AIDS to the model 
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#Adult mortality seems to be the most significant now

#Adding Adult mortality to the model 
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#Alcohol seems to be the most significant 

#Adding alcohol to the model
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality + Alcohol, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#Total expenditure seems to be the most significant 

#Adding expenditure total to the model 
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality + Alcohol + Total.expenditure, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#GDP per capita seems to be the most significant 

#Adding GDP per capita 
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality + Alcohol + Total.expenditure + GDP, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#Diphtheria seems to be most significant 

#Adding Diphtheria to the model 
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality + Alcohol + Total.expenditure + GDP + Diphtheria, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward
#Under 5 deaths are most significant 

#Adding under 5 deaths to the model 
test_mod <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality + Alcohol + Total.expenditure + GDP + Diphtheria + under.five.deaths, data=lifexpnum_analysis)

forward <- add1(test_mod, test="F", scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)
forward

#No more variables are significant 
#So our final model through forward selection is :

model_forward <- lm(Life.expectancy~Income.composition.of.resources + HIV.AIDS + Adult.Mortality + Alcohol + Total.expenditure + GDP + Diphtheria + under.five.deaths, data=lifexpnum_analysis)
summary(model_forward)
```


##Backward selection
```{r}
#Backward selection 
full_mod <- lm(Life.expectancy ~ Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources, data=lifexpnum_analysis)

drop1(full_mod, test="F",
      scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources)

#I find that Measles, Hepatitis B, thinness, Polio, Population and BMI are insignificant 

#So dropping these from the model 
full_mod <- lm(Life.expectancy ~ Adult.Mortality  + Alcohol + under.five.deaths + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Income.composition.of.resources, data=lifexpnum_analysis)

drop1(full_mod, test="F",
      scope=~Adult.Mortality  + Alcohol + under.five.deaths + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Income.composition.of.resources, data=lifexpnum_analysis)
#Rest everything is significant 

#Hence the final model after backward selection is: 
model_backward <- lm(Life.expectancy ~ Adult.Mortality  + Alcohol + under.five.deaths + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Income.composition.of.resources, data=lifexpnum_analysis)

summary(model_backward)
```
##Stepwise selection
```{r}
#Stepwise selection 
intercept_mod <- lm(Life.expectancy~1, data=lifexpnum_analysis)
step(intercept_mod, scope=~Adult.Mortality + Measles + Alcohol + Hepatitis.B + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + Income.composition.of.resources, direction = "both")

#hence the final model through stepwise selection is 
model_step <- lm(Life.expectancy ~ Income.composition.of.resources + 
                   HIV.AIDS + Adult.Mortality + Alcohol + Total.expenditure + 
                   GDP + Diphtheria + under.five.deaths + Measles + BMI + Hepatitis.B, 
                 data = lifexpnum_analysis)
summary(model_step)
```
#Comparing all variable selection models 
```{r}
summary(model_leaps)
summary(model_forward)
summary(model_backward)
summary(model_step)

```

```{r}
#Considering categorical variable "Status"
model_with_cat <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + 
                       Income.composition.of.resources + Status, data = lifexp_analysis)

model_with_cat2 <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + 
                        Income.composition.of.resources + Status + Status:Adult.Mortality + Status:Alcohol + Status: HIV.AIDS + Status:Income.composition.of.resources, data = lifexp_analysis)

model_with_cat3 <- lm(Life.expectancy ~ Adult.Mortality + Alcohol + HIV.AIDS + 
                        Income.composition.of.resources + Status + Status:Income.composition.of.resources, data = lifexp_analysis)


summary(model_with_cat)
summary(model_with_cat2)
summary(model_with_cat3) #Still isn't better than the finalized model from leaps and bounds.
```


```{r}
#Checking assumptions 
#Checking for normality with a QQ plot
plot(model_leaps)


samp <- boxcox(model_leaps) #Just to see boxcox transformations
vif(model_leaps) #to check for multicollinearity

```

#Confidence intervals for predicted variable of  the final model 
```{r}
#Confidence interval for life expectancy 
confint.lm(model_leaps,level=0.9)
predict(model_leaps,lifexpnum_analysis, interval = "confidence", level = 0.95)


predictions <- predict(model_leaps,lifexpnum_analysis, interval = "confidence", level = 0.95)
predictions.data <- as.data.frame(predictions)

#results
ggplot(data = lifexpnum_analysis, aes(x = predictions.data$fit, y = Life.expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Predicted Life Expectancy",
       y = "Real Life Expectancy",
       title = "Real vs Predicted values for life expectancy") + 
  coord_fixed()

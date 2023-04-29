---
title: "Predict of white wine quality"
output:
  html_document:
    theme: readable
    toc: yes
  pdf_document:
    toc: yes
urlcolor: cyan
---

```{r setup, echo = FALSE, message = FALSE, warning = FALSE}
options(scipen = 1, digits = 4, width = 80)
library(knitr)
opts_chunk$set(cache = TRUE, autodep = TRUE)
```

# Introduction

The title of our project is Predict of White Wine Quality. White wine is a popular beverage that has been around for more than 7,000 years. In our daily life, white wine has also become our daily drink. There are many components in white wine, for example: volatile acidity, citric acid and residual sugar and so on. Some studies have shown that these components interact with each other to influence the quality and taste of white wine. Therefore, our group was interested in measuring the quality of white wine and want to know what substances affect the quality of a wine and what the concentration of each substance is in a good wine.

The `Wine Quality Data Set` we use in this project is provided by [`the University of California, Irvine.`](https://archive.ics.uci.edu/ml/datasets/Wine+Quality). This dataset includes 4899 observations and 12 variables.

Based on physicochemical tests, we have 11 input variables. The variables in the dataset are:

-   `fixed acidity`: fixed acidity

-   `volatile acidity`: volatile acidity, too high acidity will lead to poor taste of red wine

-   `citric acid`: citric acid, a small amount of citric acid can increase the freshness of red wine

-   `residual sugar`: residual sugar, that is, after the fermentation of alcohol has not been fermented and the residual sugar

-   `chlorides`: the salt in red wine

-   `free sulfur dioxide`: free sulfur dioxide, can prevent microorganisms and be oxidized

-   `total sulfur dioxide`: total sulfur dioxide, including free sulfur dioxide and bound sulfur dioxide, if the free sulfur dioxide concentration exceeds 50 ppm, people can feel the taste of sulfur dioxide

-   `density`: density of water, that is, water minus the volume of alcohol and sugar to calculate

-   `pH`: acidity, 0 (very acidic acid) to 14 (very basic base); mostly concentrated between 3-4 ph

-   `sulphates`: an additive that produces sulfur dioxide, which has antibacterial and antioxidant properties

-   `alcohol`: alcoholic strength

Based on sensory data, we have one output variable

-   `quality`: from low to high, score between 0 and 10.

In our project, we choose the variables which we think have significant influence in affecting the price of a house to somplify the data and try to reduce the collinearity between different predictors at beginning:

Through this study, we wanted to investigate which factors affect the quality of white wine and whether these factors are correlated and affect each other, and finally we wanted to find the most suitable model to predict the quality of white wine. With this model, we want to know which substances affect the quality of a white wine and we want to use this model to accurately predict the quality of a white wine.

Thus, we will complete three methods, method 1 we want to investigate which factors influence the quality of the wine. Method 2 we want to know if there is a relationship between these factors. Finally we will sift through the model to find the most appropriate model

In this project, many of the topics will be included, some of them will be:

-   Linear regression
-   GGplot
-   Model building
-   Model selection

# Methods

## Method 1: Factors Analysis

### Checking the data

**(1)** First, we need to import the data, and there are some basic information of our data.

```{r}
white_wine = read.csv("winewhite.csv")
str(white_wine)
```

**(2)** Then, we need to check the validity of our data and omit the missing data.

```{r check the validality}
# train set
sum(is.na(white_wine))

# test set
sum(is.na(white_wine))
```

According to the result, it shows that our data set is very good! There is no missing data in it.

**(3)** After that, we will to check the distribution of `quality`.

```{r distribution of quality}
hist(white_wine$quality, col = 'orange', main = 'Quality', prob = TRUE)
table(white_wine$quality)
```

Most of the white wine quality distribution is concentrated between 5 and 6, which is medium and medium-upper. Very good quality (9 and 10) and very poor quality (1 and 2) white wines are rare, it seems that the `quality` is little right skewed.

### Predictor Choosing

**(1)** First, we need to look at the distribution of each factor

```{r}
par(mfrow = c(2, 3))
hist(white_wine$fixed.acidity,
     xlab = "Fixed Acidity",
     ylab = "Frequency",
     main = "Fixed Acidity for White Wine",
     col = "orange",
     ylim = c(0, 3000),
     xlim = c(2, 12),
     pch = 20,
     cex = 2
)
hist(white_wine$volatile.acidity,
     xlab = "Volatile Acidity",
     ylab = "Frequency",
     main = "Volatile Acidity for White Wine",
     col = "orange",
     ylim = c(0, 3000),
     xlim = c(0,1),
     pch = 20,
     cex = 2
)
hist(white_wine$citric.acid,
     xlab = "Citric Acidity",
     ylab = "Frequency",
     main = "Citric Acidity for White Wine",
     col = "orange",
     ylim = c(0, 2500),
     xlim = c(0,1),
     pch = 20,
     cex = 2
)
hist(white_wine$residual.sugar,
     xlab = "Residual Sugar",
     ylab = "Frequency",
     main = "Residual Sugar for White Wine",
     col = "orange",
     ylim = c(0, 2500),
     xlim = c(0,30),
     pch = 20,
     cex = 2
)
hist(white_wine$chlorides,
     xlab = "Chlorides",
     ylab = "Frequency",
     main = "Chlorides for White Wine",
     col = "orange",
     ylim = c(0, 3000),
     xlim = c(0, 0.5),
     pch = 20,
     cex = 2
)
hist(white_wine$free.sulfur.dioxide,
     xlab = "Free Sulfur Dioxide",
     ylab = "Frequency",
     main = "Free Sulfur Dioxide for White Wine",
     col = "orange",
     ylim = c(0, 3000),
     xlim = c(0, 350),
     pch = 20,
     cex = 2
)
hist(white_wine$total.sulfur.dioxide,
     xlab = "Total Sulfur Dioxide",
     ylab = "Frequency",
     main = "Total Sulfur Dioxide for White Wine",
     col = "orange",
     ylim = c(0, 3000),
     xlim = c(0, 500),
     pch = 20,
     cex = 2
)
hist(white_wine$density,
     xlab = "Density",
     ylab = "Frequency",
     main = "Density for White Wine",
     col = "orange",
     ylim = c(0, 3000),
     xlim = c(0.95, 1.05),
     pch = 20,
     cex = 2
)
hist(white_wine$pH,
     xlab = "pH",
     ylab = "Frequency",
     main = "pH for White Wine",
     col = "orange",
     ylim = c(0, 1500),
     xlim = c(2.5,4),
     pch = 20,
     cex = 2
)
hist(white_wine$sulphates,
     xlab = "Sulphates",
     ylab = "Frequency",
     main = "Sulphates for White Wine",
     col = "orange",
     ylim = c(0, 1100),
     xlim = c(0, 1.5),
     pch = 20,
     cex = 2
)
hist(white_wine$alcohol,
     xlab = "Alcohol",
     ylab = "Frequency",
     main = "Alcohol for White Wine",
     col = "orange",
     ylim = c(0, 1100),
     xlim = c(5, 20),
     pch = 20,
     cex = 2
)
```

**(2)** Then, we will check the relationships between `quality` and each numerical predictors.

```{r}
par(mfrow = c(2, 3))
fix_model = lm(quality~ fixed.acidity, data = white_wine)
plot(quality~ fixed.acidity, 
     data = white_wine, 
     xlab = "Fixed Acidity",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(fix_model, lwd = 3, col = "darkred")

volatile_model = lm(quality~ volatile.acidity, data = white_wine)
plot(quality~ volatile.acidity, 
     data = white_wine, 
     xlab = "Volatile Acidity",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(volatile_model, lwd = 3, col = "darkred")

citric_model = lm(quality ~ citric.acid, data = white_wine)
plot(quality ~ citric.acid, 
     data = white_wine, 
     xlab = "Citric Acidity",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(citric_model, lwd = 3, col = "darkred")


suger_model = lm(quality ~ residual.sugar, data = white_wine)
plot(quality ~ residual.sugar, 
     data = white_wine, 
     xlab = "Residual Sugar",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(suger_model, lwd = 3, col = "darkred")

chlorides_model = lm(quality ~ chlorides, data = white_wine)
plot(quality ~ chlorides, 
     data = white_wine, 
     xlab = "Chlorides",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(chlorides_model, lwd = 3, col = "darkred")

free_sulfur_model = lm(quality ~ free.sulfur.dioxide, data = white_wine)
plot(quality ~ free.sulfur.dioxide, 
     data = white_wine, 
     xlab = "Free Sulfur Dioxide",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(free_sulfur_model, lwd = 3, col = "darkred")

total_sulfur_model = lm(quality ~ total.sulfur.dioxide, data = white_wine)
plot(quality ~ total.sulfur.dioxide, 
     data = white_wine, 
     xlab = "Total Sulfur Dioxide",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(total_sulfur_model, lwd = 3, col = "darkred")

density_model = lm(quality ~ density, data = white_wine)
plot(quality ~ density, 
     data = white_wine, 
     xlab = "Density",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(density_model, lwd = 3, col = "darkred")

pH_model = lm(quality ~ pH, data = white_wine)
plot(quality ~ pH, 
     data = white_wine, 
     xlab = "pH",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(pH_model, lwd = 3, col = "darkred")

sulphates_model = lm(quality ~ sulphates, data = white_wine)
plot(quality ~ sulphates, 
     data = white_wine, 
     xlab = "Sulphates",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(sulphates_model, lwd = 3, col = "darkred")

alcohol_model = lm(quality ~ alcohol, data = white_wine)
plot(quality ~ alcohol, 
     data = white_wine, 
     xlab = "Alcohol",
     ylab = "Quality of white wine",
     col = "darkgrey")
abline(alcohol_model, lwd = 3, col = "darkred")

```

The analysis of the above chart shows that there are some factors that do not significantly affect the quality of white wines. Therefore, we will continue to pay attention to estimated regression coefficients of each factors, so as to come to a better choice of factors.

**(3)** The estimated regression coefficients of each factors

```{r}
fix_model = lm(quality~ fixed.acidity, data = white_wine)
summary(fix_model)$coefficients[2, "Pr(>|t|)"]
volatile_model = lm(quality~ volatile.acidity, data = white_wine)
summary(volatile_model)$coefficients[2, "Pr(>|t|)"]
citric_model = lm(quality ~ citric.acid, data = white_wine)
summary(citric_model)$coefficients[2, "Pr(>|t|)"]
suger_model = lm(quality ~ residual.sugar, data = white_wine)
summary(suger_model)$coefficients[2, "Pr(>|t|)"]

chlorides_model = lm(quality ~ chlorides, data = white_wine)
summary(chlorides_model)$coefficients[2, "Pr(>|t|)"]
free_sulfur_model = lm(quality ~ free.sulfur.dioxide, data = white_wine)
summary(free_sulfur_model)$coefficients[2, "Pr(>|t|)"]
total_sulfur_model = lm(quality ~ total.sulfur.dioxide, data = white_wine)
summary(total_sulfur_model)$coefficients[2, "Pr(>|t|)"]
density_model = lm(quality ~ density, data = white_wine)
summary(density_model)$coefficients[2, "Pr(>|t|)"]

pH_model = lm(quality ~ pH, data = white_wine)
summary(pH_model)$coefficients[2, "Pr(>|t|)"]
sulphates_model = lm(quality ~ sulphates, data = white_wine)
summary(sulphates_model)$coefficients[2, "Pr(>|t|)"]
alcohol_model = lm(quality ~ alcohol, data = white_wine)
summary(alcohol_model)$coefficients[2, "Pr(>|t|)"]


```


We applied 11 simple linear models for each variables and the quality of white wine. And we found that 9 variables are significant(p < 0.05). They are `fix acidity`, `volatile acidity`, `suger`,`chlorides`, `total sulfur`, `density`, `pH`, `sulphates` and `alcohol`. These factors were significantly related to the quality of the white wines and we will later use them to construct the model.

## Method 2: Find the most proper predict model

### Full model estabilishment


```{r}
Full_model = lm (quality~ ., data= white_wine)
summary(Full_model)
```

First, we created a full_model that included all variables. According to the results, we found that the adjusted R-squared for this model is 0.28. three variables: Citric acid,chlorides, total sulfur dioxide, are not significant. We will remove the insignificant variables and see if that improves R-squared of the model.

### interaction model estabilishment

Next ,we are going to consider the interaction model. 
```{r, eval=FALSE}
Inter_model = lm (quality ~ fixed.acidity * volatile.acidity * citric.acid *residual.sugar * chlorides * total.sulfur.dioxide * free.sulfur.dioxide * density * pH * sulphates * alcohol * quality, data = white_wine)
```

After we run the model, we find that this model is too large to run. And it is not practical for us to apply it into the real condition. Therefore, this interaction model is not considerable for us in this case.  


### additive model estabilishment

```{r}
add_model = lm(quality ~ . - citric.acid - chlorides - total.sulfur.dioxide, data = white_wine)
summary(add_model)
```

In this part, we create an additive model based on the summary of each variable's p_value of the full_model.We found that the variables: 'citric.acid', 'chlorides', and 'total.sulfur.dioxide' are not significant. Therefore, in the additive model, we removed these three variables. According to the results, Adjusted R-squared for this model is 0.281. This model didn't improve on the previous one significantly even if we removed three non-significant variables. One more observation is that now all variables in the model are significant and same level of significance. We would prefer the additive model than the full model as it is more concise.





### AIC and BIC applied on full model

```{r}
back_aic_full = step(Full_model,direction ='backward',trace = 0)
back_aic_full
```


```{r}
back_bic_full = step(Full_model,direction ='backward',k = log(nrow(white_wine)), trace = 0)
back_bic_full
```

We run the AIC and BIC for the full model and get the AIC and BIC model. However, we can see that additive model, AIC model and BIC model are the same. Therefore, up to now, additive model is still the mosrt proper model that we found.


### logistic regreesion 

```{r}
log_full_model = lm (log(quality) ~ ., data = white_wine)
log_additive_model = lm (log(quality) ~ . - citric.acid - chlorides - total.sulfur.dioxide, data = white_wine)
anova(log_full_model, log_additive_model)
```
Null hypothesis: There is no difference between these two logistic models based on full model and additive model.   
Alternative hypothesis: There is difference between these two logistic models based on full model and additive model.    
Statistical conclusion: Since p = 0.91 > 0.05, we can not reject the null hypothesis.   
Since there is not significant difference between these two models, we selected the log_additive model which is a more concise model.   

Then we are going to compare the log_additive model with the additive model to see which one is better fit.
```{r}
library(faraway)
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
summary(add_model)$adj.r.squared
summary(log_additive_model)$adj.r.squared
calc_loocv_rmse(add_model)
calc_loocv_rmse(log_additive_model)
```
Since the R squared of additive model and logistic additive model are close, I tried to compared their RMSE. However their RMSE are very close to each other as well. Therefore, I prefer the additive model as it's more concise.  

###AIC and BIC applied on logistic models
```{r}
back_aic_log = step(log_additive_model,direction ='backward',trace = 0)
back_bic_log = step(log_additive_model,direction ='backward',k = log(nrow(white_wine)), trace = 0)
back_aic_log
back_bic_log
```



```{r}
summary(add_model)$adj.r.squared
summary(back_aic_log)$adj.r.squared
summary(back_bic_log)$adj.r.squared
calc_loocv_rmse(add_model)
calc_loocv_rmse(back_aic_log)
calc_loocv_rmse(back_bic_log)
```

```{r}
par(mfrow = c(1,2))
qqnorm(resid(back_bic_log))
qqline(resid(back_bic_log))
qqnorm(resid(add_model))
qqline(resid(add_model))
```
By observing the R square, we can see little difference between the add_model, back_aic_log model and back_bic_log model. And we can see little difference in RMSE of these three models as well. Therefore, we choose the most concise model of these three models. Among these three models, the back_bic_log model is the most concise with 7 variables, fewer than additive model and back_aic_log model which have 8 variables.  
Moreover, according to the normal Q-Q plot, we can see that more points of back_bic_log model do do closely follow a straight line than the additive model, which indicated that residuals come from a normal distribution for back_bic_model.


```{r}
plot(fitted(back_bic_log),resid(back_bic_log))
abline(h=0)
```

The constant variance assumption is violated because for any fitted value the residuals seem not roughly centered at 0 and the spread of the residuals is not about the same. But, however, we also notice that since the quality is integer, therefore this plot is not useful in proving if thie model is fit or not.

Finally we compared the most proper model we found so far, the back_bic_log model to the null models we found in the Method 1. 
```{r}
summary(fix_model)$adj.r.squared
summary(volatile_model)$adj.r.squared
summary(citric_model )$adj.r.squared
summary(suger_model)$adj.r.squared
summary(chlorides_model )$adj.r.squared
summary(free_sulfur_model)$adj.r.squared
summary(total_sulfur_model )$adj.r.squared
summary(density_model)$adj.r.squared
summary(pH_model )$adj.r.squared
summary(sulphates_model)$adj.r.squared
summary(alcohol_model )$adj.r.squared
summary(back_bic_log)$adj.r.squared

```

Compared all the models we have using adjusted r.squared, the model of back_bic_log shows the highest adjusted r.squared, indicating that this model is the most prefer model we found.  

## Method 3: Collinearity Analysis

a) **General relationship on collinearity on every vaiables in the dataset**

<br />
In this section, we are trying find the collinearity issue within the data set. Using the `pair` function.
<br />

```{r echo=TRUE}
pairs(white_wine,                     # Data frame of variables
      labels = colnames(white_wine),  # Variable names
      pch = 21,                 # Pch symbol
      col = 'dodgerblue',       #col symbol
      main = "White wine dataset",    # Title of the plot
      row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
      gap = 1,                  # Distance between subplots
      cex.labels = NULL,        # Size of the diagonal text
      font.labels = 1)          # Font style of the diagonal text
```
<br />
Now we will use vif to check the collinearity of each predictor, using the model we found.
<br />
```{r,results=FALSE,echo=TRUE,results=FALSE}
#use vif to check the collinearity of each predictor, using the model we found.
library(car)
car::vif(back_bic_log)

```
<br />
Based on the result of the function and the plot, we can tell huge collinearity issue with variable `residual.sugar`, `density`, and `alcohol`.
<br />

```{r echo=TRUE}
#calculate the standard error
sqrt(6.397)
sqrt(12.579)
sqrt(4.588)
```
<br />
Standard error of the estimated predictor $ \hat{\beta}_{r.sugar}\ $ is approximately 3 times larger than it would have been without collinearity.
<br />
Standard error of the estimated predictor $ \hat{\beta}_{density}\ $ is approximately 4 times larger than it would have been without collinearity.
<br />
Standard error of the estimated predictor $ \hat{\beta}_{alcohol}\ $ is approximately 2 times larger than it would have been without collinearity.

b) **Create different models based on the variable selection**
<br />
Thus, we try to build an interaction model without selecting these variables with high collinearity.
<br />

First we create a new model without `density`, which has the highest variance inflation factor.
```{r}
#The interaction model without the variable that has the biggest vif - density
Inter_model_den = lm (quality ~ fixed.acidity * volatile.acidity * citric.acid  * chlorides * total.sulfur.dioxide * free.sulfur.dioxide * pH * sulphates * residual.sugar * alcohol, data = white_wine)
```
<br />
Then we build two smaller model, one without `residual sugar` and `alcohol`, one without `density`, ` residual sugar` and `alcohol`
<br />
```{r}
#model without residual sugar and alcohol
Inter_model_sugar = lm(quality ~ fixed.acidity * volatile.acidity * citric.acid  * chlorides * total.sulfur.dioxide * free.sulfur.dioxide * pH * sulphates * density, data = white_wine)
```

```{r}
#model without all three elements
Inter_model_small = lm(quality ~ fixed.acidity * volatile.acidity * citric.acid  * chlorides * total.sulfur.dioxide * free.sulfur.dioxide * pH * sulphates, data = white_wine)
```

c) **Observations on models from QQ-plot**

<br />
Checking the distribution of these three model, we can use QQ-PLOT.
<br />

```{r}
#plots for model of Inter_model_den
par(mfrow = c(1,2))
qqnorm(resid(Inter_model_den))
qqline(resid(Inter_model_den))
plot(fitted(Inter_model_den),resid(Inter_model_den))
abline(h=0)
```
```{r}
par(mfrow = c(1,2))
qqnorm(resid(Inter_model_sugar))
qqline(resid(Inter_model_sugar))
plot(fitted(Inter_model_sugar),resid(Inter_model_sugar))
abline(h=0)
```

```{r}
par(mfrow = c(1,2))
qqnorm(resid(Inter_model_small))
qqline(resid(Inter_model_small))
plot(fitted(Inter_model_small),resid(Inter_model_small))
abline(h=0)
```



d) **Model selection by adjusted r squared and LOOCV-RMSE**

<br />
Now we use adjusted r squared to compare these three interactive model
<br />

```{r echo=TRUE}
#compare the adjusted r squared of these three interactive model
summary(Inter_model_den)$adj.r.squared
summary(Inter_model_sugar)$adj.r.squared
summary(Inter_model_small)$adj.r.squared
```
<br />
Comparing the adjusted r squared, we found that the model without `density` is the best model.
<br />


<br />
We can also compare the model by their LOOCV-RMSE
<br />

```{r echo=TRUE}
#compare the model by their LOOCV-RMSE
calc_loocv_rmse(Inter_model_den)
calc_loocv_rmse(Inter_model_sugar)
calc_loocv_rmse(Inter_model_small)
```
<br />
Comparing the LOOCV-RMSE, we found that the model without `density`, ` residual sugar` and `alcohol` is the best model.
<br />

<br />
Compared all the interaction models we created in this section, using adjusted r.squared, the model of `Inter_model_den` shows the highest adjusted r.squared, and the model of  `Inter_model_den` has the lowest LOOSV-RMSE.
<br />





# Results

# Discussion

# Appendix

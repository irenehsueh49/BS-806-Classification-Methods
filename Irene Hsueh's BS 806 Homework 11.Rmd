---
title: "Irene Hsueh's BS 806 Homework 11"
author: "Irene Hsueh"
date: "11/19/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
set.seed(1234)
```

### Reading in CSV File 
```{r}
happy <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 806 - Multivariate Analysis for Biostatisticians/Class 11 - Classification Methods/Homework 11/Somerville_Happiness.csv")
head(happy, 10)
```



### Splitting Dataset into Test and Training Sets 
```{r}
training_size <- sample(1:nrow(happy), 100)
training <- happy[training_size,]
test <- happy[-training_size,]
```



### Logistic Regression
```{r}
#Logistic Regression Model
logistic_regression_model <- glm(D ~ X1 + X2 + X3 + X4 + X5 + X6, family=binomial, data=happy)
summary(logistic_regression_model)

#Forward Selection Using BIC 
forward_bic <- step(logistic_regression_model, k=log(nrow(training)))
summary(forward_bic)

#Prediction Using Test Dataset
logistic_prediction <- predict(forward_bic, newdata=test, type="response")

#Calculating Area Under Curve
library(pROC)
logistic_roc <- roc(test$D, logistic_prediction)
logistic_roc
plot(logistic_roc, col="hotpink")
```



### Classification Tree
```{r}
#Creating Regression Tree
library(tree)
set.seed(2)
Classification_tree <- tree(D ~ X1 + X2 + X3 + X4 + X5 + X6, data=training, control=tree.control(nobs=nrow(training), mindev=0.001))
Classification_tree
summary(Classification_tree)


#Pruning Tree by Cross Validation
tree_prune <- cv.tree(Classification_tree, FUN=prune.tree, K=3)
tree_prune

tree_best_prune <- prune.tree(Classification_tree, best=2)
summary(tree_best_prune)

#Prediction Using Test Dataset
tree_prediction <- predict(tree_best_prune, newdata=test)

#Calculating Area Under Curve
tree_roc <- roc(test$D, tree_prediction)
tree_roc
plot(tree_roc, col="hotpink")
```


### Linear Disciminant Analysis
```{r}
#Discriminant Analysis
library(MASS)
linear_discriminant_analysis <- lda(D ~ X1 + X2 + X3 + X4 + X5 + X6, data=training)
linear_discriminant_analysis
plot(linear_discriminant_analysis, col="hotpink")

#Prediction Using Test Dataset
lda_prediction <- predict(linear_discriminant_analysis, newdata=test)

#Calculating Area Under Curve
lda_roc <- roc(test$D, lda_prediction[[2]][,2])
lda_roc
plot(lda_roc, col="hotpink")
```



### Quadratic Disciminant Analysis
```{r}
#Discriminant Analysis
quadratic_discriminant_analysis <- qda(D ~ X1 + X2 + X3 + X4 + X5 + X6, data=training)
quadratic_discriminant_analysis

#Prediction Using Test Dataset
qda_prediction <- predict(quadratic_discriminant_analysis, newdata=test)

#Calculating Area Under Curve
qda_roc <- roc(test$D, qda_prediction[[2]][,2])
qda_roc
plot(qda_roc, col="hotpink")
```



### Area Under Curve
```{r}
data.frame(classification_method=c("Logistic Regression","Classification Tree","LDA","QDA"), AUC=c(logistic_roc$auc, tree_roc$auc, lda_roc$auc, qda_roc$auc))
```



### Logistic Regression on Whole Dataset
```{r}
#Logistic Regression Model
logistic_regression_model <- glm(D ~ X1 + X2 + X3 + X4 + X5 + X6, family=binomial, data=happy)
summary(logistic_regression_model)

#Forward Selection Using BIC 
forward_bic_whole <- step(logistic_regression_model, k=log(nrow(happy)))
summary(forward_bic_whole)

#Prediction Using Whole Dataset
logistic_prediction_whole <- predict(forward_bic_whole, newdata=happy, type="response")

#Confusion Matrix
table(happy$D, logistic_prediction_whole>0.5)
```







---
title: "Predicting Exercise Manner from fitness device"
author: "KI"
date: "4/11/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Synopsis

Using data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants who were asked to perform barbell lifts correctly and incorrectly in 5 different ways, we will predict the manner in which they did the exercise.

## Getting and Cleaning Data
```{r}
train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")
```
Data is loaded in the environment and it can be seen that both training and test data have 160 variables. To see if thay are the same following code is used. 
```{r}
change <- match(FALSE,(names(test) == names(train)))
names(train)[change]
```
```{r}
names(test)[change]
```
We see that both the test and training data sets have the same column dimensions, with only the last column differing in name. For our training data set the last column is the "classe" variable, which is the variable that predicts the manner in which the participants do excercise.

```{r}
sum(is.na(train))
sum(is.na(test))
```
Removing the columns that contain NA

```{r}
keepCol <- (colSums(is.na(train)) < nrow(train)) & (colSums(is.na(test)) < nrow(test))
train <- train[,keepCol]
test <- test[,keepCol]
```
The variables have reduced from 160 to 60 in both test and training sets.
```{r}
dim(train)
```
```{r}
dim(test)
```
## Exploratory Analysis
```{r}
ggplot(data=train, aes(classe)) + geom_bar() + labs(title="Distribution of Exercise Method", x="Activity Type")
```
From the dataset documentation, we get that five different fashions of activity are: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). In this plot, we can see that most activities are classified in class "A", which is performing the activity exactly as specified.

## Cross Validation
One of the reasons we use cross validation is for picking what type of prediction function to use by comparing different predictors. In this case we are going to compare for the following two algorithms:

Random Forests
Naive Bayes

```{r}
set.seed(123)
# prediction algorithms
algorithms <- c("rf","nb")
k <- length(algorithms)
folds <- createFolds(y=train$classe, k=k, list = TRUE)
sapply(folds,length)
```

## Choosing a Prediction Model
```{r}
# empty vector to enter models
models <- vector(length = k)
# empty vector for accuracy
accuracy <- vector(length = k)
for(i in 1:k){
    trainSub <- train[-folds[[i]],]
    testSub <- train[folds[[i]],]
    algorithms[i]
    modFit <- train(as.factor(classe) ~. , data=trainSub, method=algorithms[i], preProcess = "pca", na.action = na.omit)
    models[i] <- modFit
    pred <- predict(modFit,testSub)
    accuracy[i] <- confusionMatrix(pred,testSub$classe)$overall['Accuracy']
}
```
## Choosing a Prediction Model

Now that we have predicted with both our models, we will take a look at the accuracy and see which has the largest one.
Maximum accuracy comes out to be 99.3% (0.9931193) which corresponds to "rf" random forests algiorithm. Hence, random forest model is used on test data.
```{r}
# select the model with highest accuracy
predictionFunction <- models[match(max(accuracy),accuracy)]
# predict using model
results <- predict(predictionFunction,test)
```

## Sample and out of sample Error rate
The out of sample rate is always higher than in sample error. After applying the random forests model on test data it can be observed that it gave correct predictions in 18/20 cases which gives an accuracy of 90%. 

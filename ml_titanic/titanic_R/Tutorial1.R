# Trevor Stephens - 9 Jan 2014
# Titanic: Getting Started With R - Part 1: Booting up in R
# Full guide available at http://trevorstephens.com/

# Set working directory and import datafiles
setwd("~/Kaggle/Titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Examine structure of dataframe
str(train)

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0, 418)

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# table 函数对应的就是统计学中的列联表，是一种记录频数的方法，对于统计来说有非常重要的应用
# table() 函数demo
ct <- data.frame(X = factor(c("Yes","Yes","No","Not Sure","No"),levels = c("Yes","No","Not Sure")),
                 Y =  factor(c("Yes", "No", "No", "Yes", "No"), levels = c("Yes", "No")))

cttab <- table(ct)                 
mode(cttab)
str(cttab)
summary(cttab)
attributes(cttab)
dimnames(cttab)
as.data.frame(cttab)
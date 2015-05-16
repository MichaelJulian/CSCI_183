# In Class HW - Data Science April 18, 2015

library(rpart);library(rpart.plot); library(rattle); library(RColorBrewer)
library(caret); library(gbm); library(plyr)


# Load in Titanic Data
train <- read.csv('~/Downloads/train.csv')
test <- read.csv('~/Downloads/test.csv')
test$Survived = 0
combi <- rbind(train, test)




# Use regression tree to predict Ages, replace NAs.
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
combi$Embarked[is.na(combi$Embarked)] <- "S"
combi$Fare[is.na(combi$Fare) | combi$Fare == 0] <- mean(combi$Fare, na.rm=T)


# Create and Visualize a Tree
Survivalfit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                     data=train, method='class')
fancyRpartPlot(Survivalfit)
plotcp(Survivalfit)


# Split them back
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(69)
fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)

set.seed(69)
colnames(train)
gbmFit1 <- train(Survived ~ Pclass + Sex + Age + SibSp +
                     Parch + Fare + Embarked, data = train,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

# ⁃	Use 10-fold CV
# ⁃	gbm model
# ⁃	grid search w/ interaction depth 1,…,4 (this is number of levels in tree)
# ⁃	number of trees 200 to 10000 by 200
# ⁃	shrinkage .1,.05,.01,.005,.001

gbmGrid <-  expand.grid(interaction.depth = c(1,2,3,4),
                        n.trees = (1:50)*200,
                        shrinkage = c(0.1,.05,.01,.005,.001))


nrow(gbmGrid)

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 1)
set.seed(69)
gbmFit2 <- train(Survived ~ Pclass + Sex + Age + SibSp +
                     Parch + Fare, data = train,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaludate:
                 tuneGrid = gbmGrid)
# Summarize and Plot
ggplot(gbmFit2)
summary(gbmFit2)

preds <- predict(gbmFit2, test, type='raw')


test[,1]

write.csv(data.frame(PassengerID=test[,1], Survived = round(preds)), 
          '/Users/mijulian/Desktop/CSCI_183/GBM_Titanic_Submission.csv',
          row.names=FALSE)

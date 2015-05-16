train <- read.csv('/Users/mijulian/Desktop/CSCI_183/expedia/train.csv')
test <- read.csv('/Users/mijulian/Desktop/CSCI_183/expedia/test.csv')
sampleSub <- read.csv('/Users/mijulian/Desktop/CSCI_183/expedia/sample_submission.csv')
library(caret); library(data.table); library(pROC); library(corrplot)
set.seed(6969)
inTrain = createDataPartition(p=.70, y=train$booking_bool, list=FALSE)
tr = train[inTrain,]
ts = train[-inTrain,]

# Mean Probability Prediction
Prediction = rep(mean(tr$booking_bool), dim(ts)[1])
Prediction0 = rep(0, dim(ts)[1])
Labels = factor(ts$booking_bool)
plot(roc(Labels, Prediction0))




# Creating Search Positions
unique_srch_id = unique(td[,srch_id])
positionID = sapply(unique_srch_id, FUN=function(x){
    seq(1:td[srch_id==x,.N])   
})
positionID = unlist(positionID)
td[,position := positionID]
hist(td[booking_bool == 1, position], breaks=32)
td

# SUBMISSION 1 : Shipping Mean Prediction, Baseline Submission
predictions = train$
Submission = data.frame('srch-prop_id' = paste(test$srch_id, test$prop_id, sep='-'),
                        booking_bool = mean(train$booking_bool))
write.csv(Submission, '/Users/mijulian/Desktop/CSCI_183/expedia/submission1_means.csv', row.names=FALSE)


# SUBMISSION 2 : 
# Assume first prop_id shown is booked.
test.dt = as.data.table(test)
unique_srch_id = unique(test.dt[,srch_id])
Prediction_2 = sapply(unique_srch_id, FUN=function(x){
    c( 1, rep(0,(test.dt[srch_id==x,.N]-1)) )   
})
Prediction_2 = unlist(Prediction_2)

Submission_2 = data.frame('srch-prop_id' = paste(test$srch_id, test$prop_id, sep='-'),
                          booking_bool = Prediction_2)
write.csv(Submission_2, '/Users/mijulian/Desktop/CSCI_183/expedia/submission2_position.csv', row.names=FALSE)

#
colnames(tr)
featurePlot(x=tr[,c('price_usd','prop_starrating')], # predictor variables
            y=tr$booking_bool, # target prediction
            plot='pairs') # plot type
        
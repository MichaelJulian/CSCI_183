train <- read.csv('/Users/mijulian/Desktop/CSCI_183/expedia/train.csv')
test <- read.csv('/Users/mijulian/Desktop/CSCI_183/expedia/test.csv')
#sampleSub <- read.csv('/Users/mijulian/Desktop/CSCI_183/expedia/sample_submission.csv')
library(caret); #library(data.table); #library(pROC); library(randomForest); 
library(Hmisc)
library(gbm); library(plyr)
set.seed(415)
test$booking_bool = 0
combi <- rbind(train, test)

sapply(combi,class)

cols = sapply(names(combi), FUN=function(x){length(levels(factor(combi[,x])))})
continuous = names(cols[cols>=33])
categorical = names(cols[cols<33])
combi <- rbind(train, test)

combi[,categorical] <- lapply(combi[,categorical], factor)
sapply(train,class)


combi$cutPrice = cut2(combi$price_usd, g=10)
combi$cutDist = cut2(combi$orig_destination_distance, g=10)
combi$cutWindow = cut2(combi$srch_booking_window, g=10)
combi$cutLocScore1 = cut2(combi$prop_location_score1, g=10)
combi$cutLocScore2 = cut2(combi$prop_location_score2, g=10)
combi$cutVisStar = cut2(combi$visitor_hist_starrating, g=10)
combi$cutVisAdrUsd = cut2(combi$visitor_hist_adr_usd, g=10)
combi$cutSrchScore = cut2(combi$srch_query_affinity_score*-1, g=8)

train <- combi[1:300621,]
test <- combi[300622:596244,]

# fit <- randomForest(as.factor(booking_bool) ~ prop_starrating+
#                         prop_review_score + prop_brand_bool + promotion_flag +
#                         srch_length_of_stay + srch_adults_count + srch_booking_window +
#                         srch_children_count + srch_room_count + srch_saturday_night_bool +
#                         random_bool + comp1_rate + comp1_inv + comp2_rate + comp2_inv +
#                         cutDist + cutSrchScore + cutWindow +
#                         cutLocScore1 + cutLocScore2 + cutVisStar + cutVisAdrUsd,
#                     data=train, importance=TRUE, ntree=300)
# varImpPlot(fit)

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 1)

gbmGrid <-  expand.grid(interaction.depth = c(1,2,3,4),
                        n.trees = 2000,
                        shrinkage = c(.1,.05,.01))

set.seed(6969)
summary(train$cutLocScore1)
summary(combi$srch_adults_count)


# t1 <- train[1:10000,]
# t2 <- train[10001:20000,]
gbmFit2 <- train(as.factor(booking_bool) ~ prop_starrating +
                     prop_review_score + prop_brand_bool + promotion_flag # +# +
#                      cutDist + cutSrchScore +
#                       cutLocScore1 + cutLocScore2,
    ,
                 data=t1,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = T,
                 tuneGrid = gbmGrid)
ggplot(gbmFit2)
summary(gbmFit2)
booking_bool
booking_bool = predict(gbmFit2, test)
summary(booking_bool)

Submission = data.frame('srch-prop_id' = paste(test$srch_id, test$prop_id, sep='-'),
                        booking_bool = booking_bool)
colnames(Submission) = c('srch-prop_id','booking_bool')
write.csv(Submission, '/Users/mijulian/Desktop/CSCI_183/expedia/submission3_gbm.csv',
          row.names=FALSE)


# Creating Search Positions
# td = as.data.table(train)
# unique_srch_id = unique(td[,srch_id])
# positionID = sapply(unique_srch_id, FUN=function(x){
#     seq(1:td[srch_id==x,.N])   
# })
# positionID = unlist(positionID)
# td[,position := positionID]

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
        
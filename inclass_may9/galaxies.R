#Install R packages ripa, jpeg, and EBImage
install.packages(c('ripa','jpeg','EBImage'))
install.packages('zoom')
install.packages('BiocInstaller', repos='http://www.bioconductor.org/packages/2.14/bioc')
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")# 
library(ripa); library(jpeg); library(EBImage)

library(randomForest)
# a. Reads in each image (and the training labels)
# b.  Converts each image to gray scale and resizes each image to 50x50
# c.  Computes the mean, variance, 10th, 25th, 75th, 90th quantiles of each image
# 4)  Split the training data into 2 CV folds, fit a linear regression to the labels using the variables 
#   you found in c on one fold and compute the root mean square error on the other.
# 5)  Time permitting, what other features can you compute?  Does your score improve?
# 
# use func:  list.files, readJPEG, rgb2grey, resize
# Also, given the size of the data, when you loop over the images to 
# create features print out progress updates the screen
path = "/Users/mijulian/Desktop/Kaggle/images_training_rev1"
files = list.files(path = path)

GalaxyIDs = sapply(files, FUN=function(x){
    as.numeric(substr(x,start=1,stop=6))})

key <- read.csv(paste0('/Users/mijulian/Dropbox/image_classification_competition/galaxy_train.csv'))
train <- features[1:30789,]
test <- features[20001:30789,]
train$prob = key$Prob_Smooth[1:30789]

features$GalaxyID = GalaxyIDs


df = merge(key,features)

fit = lm(Prob_Smooth ~., data=df[,-1])

test = merge(key,features, by='GalaxyID', all.y=T)
test <- test[is.na(test$Prob_Smooth),]
test$Prob_Smooth = predict(fit, test)

sub_1 = test[,(1:2)]

write.csv(sub_1, '/Users/mijulian/Desktop/sub_1.csv', row.names=F)



for(i in 1:61578){
    
    as.numeric(substr(files[1],start=1,stop=6))
    x = readJPEG(paste0(path,'/',files[2]))
    
    x_g <- rgb2grey(x);
    
    xgm <- matrix(x_g, nrow=424, ncol=424)
    xgmr <- round(xgm)
    
    plot(imagematrix(xgm[(157:267),(157:267)], type='grey',
                     ncol=111, nrow=111))
    plot(imagematrix(xgmr,type='grey',ncol=424,nrow=424))
    plot(imagematrix(xgm,type='grey',ncol=424,nrow=424))
    
    plot(x_g)
    key$mean <- rep(0, length(key[,1]))
    key$var <- rep(0, length(key[,1]))
    key$var <- rep(0, length(key[,1]))
    
    
    
    
    # FEATURES
    mean(x)
    xg <- rgb2grey(x)
    xgr <- resize(xg,w=50, h=50);
    xgm <- matrix(xg, nrow=424, ncol=424)
    xgm <- xgm[(157:267),(157:267)] # Middle 111x111 matrix
    xgmr <- round(xgm)    
    mean(xgm)
    var(xgm)
    mean(xgmr)
    var(xgmr)
    
}









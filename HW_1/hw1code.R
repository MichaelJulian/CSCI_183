library(data.table)
library(ggplot2)
data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
dt1 <- as.data.table(data1)
dt1$Day = 1
setwd('/Users/mijulian/Desktop/CSCI_183/HW_1')
# Loop and bind data tables
for (i in 2:31) {
    dtn <- as.data.table(read.csv(url(paste0(
        "http://stat.columbia.edu/~rachel/datasets/nyt", i, ".csv"))))
    dtn$Day = i
    dtl <- rbind(dt1, dtn)
    
}
dt  <- as.data.table(dt1)
dt[,agecat := cut(Age, c(-Inf,0,18,24,34,44,54,64,Inf))]

# Some Summaries:
dt[,.(count=.N, minAge=min(Age), meanAge=mean(Age), maxAge=max(Age)),agecat][order(agecat)]

# So only signed in users have ages and genders
dt[,.(Gender.Mean=mean(Gender), Signed_In.mean = mean(Signed_In),
      Impressions.mean=mean(Impressions), Clicks.mean=mean(Clicks)),agecat][order(agecat)]

# plots
ggplot(dt[Day == 1], aes(x=Clicks, fill=agecat)) + geom_histogram(binwidth=1)
ggplot(dt[Clicks > 0], aes(x=agecat, y=Impressions, fill=agecat)) + geom_boxplot()##
ggplot(dt[Day == 1 & Clicks > 0 & Clicks/Impressions != 1], aes(x=agecat, y=Clicks/Impressions, fill=agecat)) + geom_boxplot()##
dt$scode[dt$Impressions==0] = 'NoImps'
dt$scode[dt$Impressions > 0] = 'Imps'
dt$scode[dt$Clicks > 0] = 'Clicks'
dt$scode = factor(dt$scode)

# factorize
data1$scode <- factor(data1$scode)

#look at levels
dt[,.(Count=.N, CTR=mean(Clicks/Impressions)),
   .(scode, Gender, agecat)][order(scode, Gender, agecat)]


# PLOT AND SAVE JPG
# Plot Avg Age of Viewers
print(ggplot(dt[Age != 0,.(AvgAge=mean(Age)),.(Day, Gender)], 
       aes(y=AvgAge, x=Day, colour=factor(Gender))) + geom_line() + 
    ggtitle('Average Age Between Genders'))
dev.copy(jpeg,filename='Age_Between_Genders.jpg')
dev.off()


# Plot Avg CTR between Genders over time
print(ggplot(dt[Impressions != 0,.(AvgCTR=mean(cthru, na.rm=TRUE)),.(Day, isMale=factor(Gender))], 
       aes(y=AvgCTR, x=Day, colour=isMale)) + geom_line() + 
    ggtitle('Average CTR Between Genders'))
# Save File
dev.copy(jpeg,filename='CTR_Between_Genders.jpg')
dev.off()



# Plot Avg CTR between Age Categories
print(ggplot(dt[Impressions != 0,.(AvgCTR=mean(cthru, na.rm=TRUE)),.(Day, agecat)], 
       aes(y=AvgCTR, x=Day, colour=agecat)) + geom_line() + 
    ggtitle('Average CTR Between AgeGroups') )
dev.copy(jpeg,filename='CTR_Between_Ages.jpg')
dev.off()



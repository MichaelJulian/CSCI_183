# SCU BIKE THEFT DATA
library(ggplot2)
bt <- read.csv('/Users/mijulian/Desktop/Kaggle/scubiketheft.csv')
bt$LOCATION <- as.character(bt$LOCATION)

colnames(bt)[3] = "LOC"
dormOther = sapply(bt$LOC, FUN=function(x){return(paste(x,"Other"))})
dormOther = as.vector(dormOther)

dormNames = c('Swig','Nobili','Sobrato','Villas','Flip','Casa','Walsh','Campisi',"Other")
dorm = sapply(dormOther,FUN=function(x){ 
            for(i in 1:9){
                if(grepl(dormNames[i],x)){
                    return(dormNames[i])
                }}})
dormunlisted = unlist(lapply(dorm, '[[', 1))
dorm = as.vector(dormunlisted)
bt$dorm = dorm


# VISUALIZE DORMS
c <- ggplot(btd[dorm != "Other"], aes(factor(dorm)))
c <- c + geom_bar()
c


# FORMATTING DATES
bt$DATE = as.character(bt$DATE)
bt$DATE[2] = "12/07/07"
bt$DATE[25] = "3/14/08"
bt$DATE[67] = "9/23/09"
bt$DATE[165] = "10/31/11"
bt$weekday = weekdays(bt$DATE)
bt$month = months(bt$DATE)

# PLOT WEEKDAYS
wkday <- ggplot(bt, aes(factor(weekday), fill=weekday)) + 
    geom_bar() + scale_fill_brewer(palette="Spectral")
wkday

# PLOT MONTHS
months <- ggplot(bt, aes(factor(month), fill=month)) + 
    geom_bar() + scale_fill_brewer(palette="Spectral")
months


# PLOT COSTS ( if given )
bt$BIKE.TYPE <- as.character(bt$BIKE.TYPE)
strsplit(,split='[$]')[[1]][2]
bt$COST <- sapply(bt$BIKE.TYPE, FUN=function(x){strsplit(x, split='[$]')[[1]][2]})
bt$COST2 <- sapply(bt$COST, FUN=function(x){strsplit(x, split='[- ]')[[1]][1]})
COSTS <-  as.numeric(bt$COST2)
COSTS <- COSTS[!is.na(COSTS)]
qplot(COSTS)


# Look at the Lost Info
class(bt$LOST.INFO)
bt$LOST.INFO
# Many descriptions
# Either: (1) lock cut and removed (2) unlocked bike (3) wheels/tires taken
lostLevels <- c("cut",)

wkday
dev.copy(jpeg,filename='/Users/mijulian/Desktop/CSCI_183/scu_bikethefts/EDA_weekdays.jpg')
dev.off()
months
dev.copy(jpeg,filename='/Users/mijulian/Desktop/CSCI_183/scu_bikethefts/EDA_months.jpg')
dev.off()

qplot(COSTS,main="Reported Costs of Bikes")
dev.copy(jpeg,filename='/Users/mijulian/Desktop/CSCI_183/scu_bikethefts/EDA_costs.jpg')
dev.off()

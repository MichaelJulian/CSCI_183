# D3.js
library(quantmod)
library(data.table)
getwd()
emp <- read.table('../inclass_d3_assignment/unemployment.tsv', header=T)
head(emp)
dim(emp)

# Ok, cool beans but lets get scu bike theft data

bt <- read.csv('../scu_bikethefts/scubiketheft.csv')
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


# FORMATTING DATES
bt$DATE = as.character(bt$DATE)
bt$DATE[2] = "12/07/07"
bt$DATE[25] = "3/14/08"
bt$DATE[67] = "9/23/09"
bt$DATE[165] = "10/31/11"
bt$YM = format(as.Date(bt$DATE, format='%m/%d/%y'), format='%Y-%m')

bt$weekday = weekdays(bt$DATE)
bt$month = months(bt$DATE)

bdt <- as.data.table(bt)
bdt[,.(Y.M, dorm),]


tbl = table(bdt[,.(Y.M, dorm),])
write.table(tbl, '../inclass_d3_assignment/scubiketheft_ts.tsv', sep='\t')

bt2 <- read.table('../inclass_d3_assignment/scubiketheft_ts.tsv', header=T)
head(bt2)
filter(bt2, c(1,1,1)/3)
bt2_filtered <- filter(bt2, c(1,1,1)/3)
rownames(bt2_filtered) = rownames(bt2)
colnames(bt2_filtered) = colnames(bt2)
bt2_df <- data.frame(bt2_filtered)
bt2_df[1,] <- c(0,0,0,0,0,0,0,0)
bt2_df[51,] <- c(0,0,0,0,0,0,0,0)

write.table(t(bt2_df), '../inclass_d3_assignment/scubikefiltered.tsv', 
            sep='\t', col.names=NA)

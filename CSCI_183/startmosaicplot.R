getwd()
gs <- read.csv('../mosaicplot/GSS.csv')
sapply(gs,class)

levels(gs[,3])
levels(gs[,4])
colnames(gs) <- c('yr','id','inc','gay')
#GS with no non-answers
gsa <- gs[gay != ]
summary(gs[,4])
gs[,3]

install.packages('vcd'); 
library(vcd)
mosaic(gs[,3:4])
gs[,3:4]
HairEyeColor
mosaic(HairEyeColor, shade=TRUE, legend=TRUE)

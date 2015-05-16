emp2 <- read.table('../inclass_d3_assignment/unemployment2.tsv', header=T)
library(devtools)
install_github("choroplethr", "trulia")
library(choroplethr)
install.packages('acs')
library('acs')

api.key.install(key='9820f0043ffae70cac4bd7642fe33dcce8e9abf2')

acs.fetch(endyear=2011, variable=c("B08101",'county'), span=5,
          geo.make(state="*", county="*"))
geo.make(us=1)



install.packages("maps")
library(ggplot2)
library(maps)
# Code from http://eglenn.scripts.mit.edu/citystate/2013/07/using-acs-r-to-create-chorpleth-maps/
county.df=map_data("county")
names(county.df)[5:6]=c("state","county")
state.df=map_data("state")
us.county=geo.make(state="*", county="*")

# fetch data
us.transport=acs.fetch(geography=us.county, 
                       table.number="B08101", col.names="pretty")

# divide.acs
us.pub.trans=divide.acs(numerator=us.transport[,41], 
                        denominator=us.transport[,1], method="proportion")
pub.trans.est=data.frame(county=geography(us.pub.trans)[[1]], 
                         percent.pub.trans=as.numeric(estimate(us.pub.trans)))

head(pub.trans.est)
# clean out data
pub.trans.est$county=gsub("Parish", "County", pub.trans.est$county)
pub.trans.est$state=tolower(gsub("^.*County, ", "", pub.trans.est$county))
pub.trans.est$county=tolower(gsub(" County,.*", "", pub.trans.est$county))
choropleth=merge(county.df, pub.trans.est, by=c("state","county"))
choropleth=choropleth[order(choropleth$order), ]
choropleth$pub.trans.rate.d=cut(choropleth$percent.pub.trans, 
                                breaks=c(0,.01,.02,.03,.04,.05,.1,1), include.lowest=T)

head(choropleth)

ggplot(choropleth, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = pub.trans.rate.d), colour = "white", size = 0.2) + 
    geom_polygon(data = state.df, colour = "white", fill = NA) +
    scale_fill_brewer(palette = "Blues")


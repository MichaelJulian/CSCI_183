library(ggplot2); library(data.table)
m = read.csv('../highdimensional/music-all.csv')
mt <- as.data.table(m)

# Impute attempt
lapply(m, FUN=function(x){
    x[is.na(x)]
    
    })
# Impute NAs with 0
# Idea, impute with mean of artist.
m[,4:73] <- lapply(m[,4:73], FUN=function(x){
    x[is.na(x)] = 0
    x
})

# Principal Component Analysis
pr = prcomp(m[,4:73], scale=T, retx=T)
# Plot first two PC
plot(pr$x, col=m$artist)

# Convert to df, plot artists.
pr.df <- as.data.frame(pr$x)
pr.df$artist = m$artist
ggplot(data=pr.df, aes(x=pr.df[,1], y=pr.df[,2])) + 
    geom_text(aes(label=artist, size=1, hjust=0, 
                  vjust=0,col=factor(artist))) + 
    xlab('PC 1') + ylab('PC 2') 



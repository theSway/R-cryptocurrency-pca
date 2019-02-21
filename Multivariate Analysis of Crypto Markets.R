setwd("PROJECT DIRECTORY HERE")

# Let us prepare the data

cryptos <- data.frame(crypto_markets)
included_cryptos <- cryptos[cryptos$date >= as.Date("2018-02-01"),] # First filter
crypto_labels <- unique(included_cryptos$slug)

# Second filter
semifinal_data <- cryptos[cryptos$date >= as.Date("2018-02-01") & cryptos$date <= as.Date("2018-02-28") & cryptos$slug %in% crypto_labels,]

# For prettier names
Asset <- semifinal_data$slug
Close <- semifinal_data$close
market <- semifinal_data$market
close_ratio <- semifinal_data$close_ratio
spread <- semifinal_data$spread

# First iteration of final data, to be reshaped
final_data <- data.frame(Asset, Close)

# We want to explore the month of February, let's see if we can have consistent observations
first_coin <- "bitcoin"
counter <- 0
unfit_observations <- c()
for(coin in final_data$Asset){
  if(coin == first_coin){
    counter <- counter + 1
  }
  else{
    if(counter != 28){
      unfit_observations <- c(unfit_observations, first_coin)
    }
    first_coin <- coin
    counter <- 1
  }
}
if(counter != 28){
  unfit_observations <- c(unfit_observations, first_coin)
}
unfit_observations <- c(unfit_observations)
final_data <- final_data[!(final_data$Asset %in% unfit_observations),] # Consistent final data

# We want to also include market cap into our analysis, so let's create a filter
semifinal_data <- semifinal_data[!(semifinal_data$slug %in% unfit_observations),]
alphabetic_semifinal_data <- semifinal_data[order(semifinal_data$slug), ]
counter2 <- 0
no_market <- c()
market_means <- c()
i = 1
for(iterator in 1:1246){
  market_mean <- mean(alphabetic_semifinal_data$market[i:(i+27)])
  if(market_mean == 0){
    no_market <- c(no_market, alphabetic_semifinal_data$slug[i])
  }
  else{
    market_means <- c(market_means, market_mean)
    market_mean <- 0
  }
  i <- i+28

}

# Further refining final data
final_data <- final_data[!(final_data$Asset %in% no_market),]

# Market cap into descriptive bins
groups <- c()
for(m in 1:1016){
  if(market_means[m] >= 1e+08){
    groups <- c(groups, "over 100M")
    next
  }
  if(market_means[m] >= 5e+07){
    groups <- c(groups, "over 50M")
    next
  }
  if(market_means[m] >= 5e+05){
    groups <- c(groups, "over 500k")
    next
  }
  if(market_means[m] < 5e+05){
    groups <- c(groups, "under 500k")
  }
}

# Producing final data frame
library(tidyr)
library(plyr)
final_data$Day <- 1:28

df <- spread(final_data, Day, Close)
df$group <- groups

# Univariate analysis

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Prices of big five by market cap
bigfive <- final_data[1:140,]
library(ggplot2)
ggplot(data=bigfive, aes(x=Day, y=Close, group=Asset, color=Asset)) +
  geom_line()+
  geom_point() +
  scale_fill_discrete(name = "Asset") +
  ggtitle('February price performance of Big Five')

# Binning by group
categories <- as.data.frame(table(df$group))
categories <- categories[c(4,3,1,2),]
ggplot(data=categories, aes(Var1, Freq)) +
  geom_col(stat="identity", binwidth = 2) +
  xlab("Market capitalisation, USD") +
  ggtitle("Market capitalisation distribution of assets")

# Histogram of volatility, by category
vols <- c()
k <- 1
alphabetic <- final_data[order(final_data$Asset),]
for(i in 1:1016){
  price <- alphabetic$Close[k:(k+27)]
  ret <- diff(log(price))
  coinvol <- sd(ret)*sqrt(28)*100
  vols <- c(vols, coinvol)
  k <- k+28
}
vol_df <- data.frame(Asset= df$Asset, Volatility=vols, Group=df$group)
vol_df <- vol_df[!vol_df$Volatility %in% boxplot.stats(vol_df$Volatility)$out,]
df <- df[df$Asset %in% vol_df$Asset,]
massive_coins <- vol_df[which(vol_df$Group == "over 100M"), c("Asset", "Volatility", "Group")]
large_coins <- vol_df[which(vol_df$Group == "over 50M"), c("Asset", "Volatility", "Group")]
mid_coins <- vol_df[which(vol_df$Group == "over 500k"), c("Asset", "Volatility", "Group")]
small_coins <- vol_df[which(vol_df$Group == "under 500k"), c("Asset", "Volatility", "Group")]

h1 <- ggplot(data=massive_coins, aes(Volatility)) +
  geom_histogram(bins=50) +
  ggtitle('Over 100M')

h2 <- ggplot(data=large_coins, aes(Volatility)) +
  geom_histogram(bins=50) +
  ggtitle('Over 50M')

h3 <- ggplot(data=mid_coins, aes(Volatility)) +
  geom_histogram(bins=50) +
  ggtitle('Over 500k')

h4 <- ggplot(data=small_coins, aes(Volatility)) +
  geom_histogram(bins=50) +
  ggtitle('Under 500k')

multiplot(h1, h3, h2, h4, cols=2)

# Daily log returns
# Massive model
diff_df <- data.frame("1"=rep(NA, 140), 
                      "2"=rep(NA, 140),
                      "3"=rep(NA, 140),
                      "4"=rep(NA, 140),
                      "5"=rep(NA, 140),
                      "6"=rep(NA, 140),
                      "7"=rep(NA, 140),
                      "8"=rep(NA, 140),
                      "9"=rep(NA, 140),
                      "10"=rep(NA, 140),
                      "11"=rep(NA, 140),
                      "12"=rep(NA, 140),
                      "13"=rep(NA, 140),
                      "14"=rep(NA, 140),
                      "15"=rep(NA, 140),
                      "16"=rep(NA, 140),
                      "17"=rep(NA, 140),
                      "18"=rep(NA, 140),
                      "19"=rep(NA, 140),
                      "20"=rep(NA, 140),
                      "21"=rep(NA, 140),
                      "22"=rep(NA, 140),
                      "23"=rep(NA, 140),
                      "24"=rep(NA, 140),
                      "25"=rep(NA, 140),
                      "26"=rep(NA, 140),
                      "27"=rep(NA, 140))
numeric_df <- df[which(df$group == "over 100M"), c(2:29)]
for(i in 1:140){
  diff_num <- diff(as.numeric(log(numeric_df[i,])))
  diff_df[i,] <- t(diff_num)
}

# Large model
diff_df <- data.frame("1"=rep(NA, 67), 
                      "2"=rep(NA, 67),
                      "3"=rep(NA, 67),
                      "4"=rep(NA, 67),
                      "5"=rep(NA, 67),
                      "6"=rep(NA, 67),
                      "7"=rep(NA, 67),
                      "8"=rep(NA, 67),
                      "9"=rep(NA, 67),
                      "10"=rep(NA, 67),
                      "11"=rep(NA, 67),
                      "12"=rep(NA, 67),
                      "13"=rep(NA, 67),
                      "14"=rep(NA, 67),
                      "15"=rep(NA, 67),
                      "16"=rep(NA, 67),
                      "17"=rep(NA, 67),
                      "18"=rep(NA, 67),
                      "19"=rep(NA, 67),
                      "20"=rep(NA, 67),
                      "21"=rep(NA, 67),
                      "22"=rep(NA, 67),
                      "23"=rep(NA, 67),
                      "24"=rep(NA, 67),
                      "25"=rep(NA, 67),
                      "26"=rep(NA, 67),
                      "27"=rep(NA, 67))
numeric_df <- df[which(df$group == "over 50M"), c(2:29)]
for(i in 1:67){
  diff_num <- diff(as.numeric(log(numeric_df[i,])))
  diff_df[i,] <- t(diff_num)
}

# Mid model
diff_df <- data.frame("1"=rep(NA, 582), 
                      "2"=rep(NA, 582),
                      "3"=rep(NA, 582),
                      "4"=rep(NA, 582),
                      "5"=rep(NA, 582),
                      "6"=rep(NA, 582),
                      "7"=rep(NA, 582),
                      "8"=rep(NA, 582),
                      "9"=rep(NA, 582),
                      "10"=rep(NA, 582),
                      "11"=rep(NA, 582),
                      "12"=rep(NA, 582),
                      "13"=rep(NA, 582),
                      "14"=rep(NA, 582),
                      "15"=rep(NA, 582),
                      "16"=rep(NA, 582),
                      "17"=rep(NA, 582),
                      "18"=rep(NA, 582),
                      "19"=rep(NA, 582),
                      "20"=rep(NA, 582),
                      "21"=rep(NA, 582),
                      "22"=rep(NA, 582),
                      "23"=rep(NA, 582),
                      "24"=rep(NA, 582),
                      "25"=rep(NA, 582),
                      "26"=rep(NA, 582),
                      "27"=rep(NA, 582))
numeric_df <- df[which(df$group == "over 500k"), c(2:29)]
for(i in 1:582){
  diff_num <- diff(as.numeric(log(numeric_df[i,])))
  diff_df[i,] <- t(diff_num)
}

# Small model
diff_df <- data.frame("1"=rep(NA, 141), 
                      "2"=rep(NA, 141),
                      "3"=rep(NA, 141),
                      "4"=rep(NA, 141),
                      "5"=rep(NA, 141),
                      "6"=rep(NA, 141),
                      "7"=rep(NA, 141),
                      "8"=rep(NA, 141),
                      "9"=rep(NA, 141),
                      "10"=rep(NA, 141),
                      "11"=rep(NA, 141),
                      "12"=rep(NA, 141),
                      "13"=rep(NA, 141),
                      "14"=rep(NA, 141),
                      "15"=rep(NA, 141),
                      "16"=rep(NA, 141),
                      "17"=rep(NA, 141),
                      "18"=rep(NA, 141),
                      "19"=rep(NA, 141),
                      "20"=rep(NA, 141),
                      "21"=rep(NA, 141),
                      "22"=rep(NA, 141),
                      "23"=rep(NA, 141),
                      "24"=rep(NA, 141),
                      "25"=rep(NA, 141),
                      "26"=rep(NA, 141),
                      "27"=rep(NA, 141))
numeric_df <- df[which(df$group == "under 500k"), c(2:29)]
for(i in 1:141){
  diff_num <- diff(as.numeric(log(numeric_df[i,])))
  diff_df[i,] <- t(diff_num)
}

# Entire model
diff_df <- data.frame("1"=rep(NA, 930), 
                      "2"=rep(NA, 930),
                      "3"=rep(NA, 930),
                      "4"=rep(NA, 930),
                      "5"=rep(NA, 930),
                      "6"=rep(NA, 930),
                      "7"=rep(NA, 930),
                      "8"=rep(NA, 930),
                      "9"=rep(NA, 930),
                      "10"=rep(NA, 930),
                      "11"=rep(NA, 930),
                      "12"=rep(NA, 930),
                      "13"=rep(NA, 930),
                      "14"=rep(NA, 930),
                      "15"=rep(NA, 930),
                      "16"=rep(NA, 930),
                      "17"=rep(NA, 930),
                      "18"=rep(NA, 930),
                      "19"=rep(NA, 930),
                      "20"=rep(NA, 930),
                      "21"=rep(NA, 930),
                      "22"=rep(NA, 930),
                      "23"=rep(NA, 930),
                      "24"=rep(NA, 930),
                      "25"=rep(NA, 930),
                      "26"=rep(NA, 930),
                      "27"=rep(NA, 930))
numeric_df <- subset(df, select = -c(Asset, group))
for(i in 1:930){
  diff_num <- diff(as.numeric(log(numeric_df[i,])))
  diff_df[i,] <- t(diff_num)
}

# Onto the PCA

pca <- prcomp(diff_df, scale. = T) # Running centered and scaled PCA
pca$center # Mean of variables
pca$scale # Standard deviation of variables
pca$rotation # Rotation matrix
plot(pca, type = "l")

summary(pca)

# More plotting

library(ggfortify)
marketcap <- data.frame(df, "Market Cap"=df$group)
autoplot(pca, data = marketcap , colour='Market.Cap') +
  labs(title="2D PCA of cryptocurrencies")

ggplot(pca, aes(PC1, PC2)) +
  geom_point() 

# Even more plotting

library(pca3d)
pca3d(pca, group = df$group)

# Group plots

massivepca <- autoplot(pca) + labs(title="Over 100M")
largepca <- autoplot(pca) + labs(title="Over 50M")
midpca <- autoplot(pca) + labs(title="Over 500k")
smallpca <- autoplot(pca) + labs(title="Under 500k")
multiplot(massivepca, midpca, largepca, smallpca, cols=2)

pca3d(pca)


# Some experiments

library(ggbiplot)
pca.x <- as.data.frame(pca$x)
pca.x$groups <- df$group
pca.centroids <- aggregate(pca.x[,1:13], list(Type = pca.x$groups), mean)
distances <- dist(rbind(pca.centroids[pca.centroids$Type == "over 50M",2:3],pca.centroids[pca.centroids$Type == "over 100M",2:3]), method = "euclidean")

ggbiplot(pca.x[,10:11], var.axes =FALSE)

plot(pca$x)



#####################################################################################################
# Clustering
#####################################################################################################
library(ggplot2)
library(ggcorrplot)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readr)
library(mlr)
library(dendextend)
library(reshape2)
library(ggforce)

# data cleaning
creditcard <- read.csv("Desktop/CCGENERAL.csv")
creditcardinfo = select(creditcard,-c(1))
creditcardinfo$CREDIT_LIMIT[is.na(creditcardinfo$CREDIT_LIMIT)] <- mean(creditcardinfo$CREDIT_LIMIT, na.rm=TRUE)
creditcardinfo$MINIMUM_PAYMENTS[is.na(creditcardinfo$MINIMUM_PAYMENTS)] <- mean(creditcardinfo$MINIMUM_PAYMENTS, na.rm=TRUE)
sapply(creditcardinfo, class)
summary(creditcardinfo)

# data visualization 
# Part1: Explore the distribution of data
require(reshape2)
melt.creditcardinfo <- melt(creditcardinfo)
head(melt.creditcardinfo)
ggplot(data = melt.creditcardinfo, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")
# Part2: visualize the correlations
cor(creditcardinfo)
ggcorrplot(cor(creditcardinfo))
# Part3: check the outliers
OutVals = boxplot(creditcardinfo)$out
which(creditcardinfo %in% OutVals)

#scale the values
credit_card_scaled <- scale(creditcardinfo)
summary(credit_card_scaled)

# Hierarchical Clustering 
distances = dist(credit_card_scaled,method = 'euclidean')
clusters = hclust(d = distances,method = 'ward.D2')
plot(color_branches(as.dendrogram(clusters),k = 4,groupLabels = F))
clusterGroups = cutree(clusters,k=4)
table(clusterGroups)

# plot only the upper part
plot(cut(as.dendrogram(clusters),h=20)$upper) 
# label with color
plot(clusters)
rect.hclust(clusters, k = 4, border = 2:5)

# kmeans
set.seed(617)
# firstly calculate the turning point
# within error  
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = credit_card_scaled,centers = x,iter.max = 50,nstart = 25)$tot.withinss})

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# ratio
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = credit_card_scaled,centers = x,iter.max = 10000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# silhoette
silhoette_width = sapply(2:10,FUN = function(x) pam(x = credit_card_scaled,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

#fviz_nbclust(credit_card_scaled, kmeans, method="wss")
#fviz_nbclust(credit_card_scaled, kmeans, method="silhouette")

km = kmeans(x = credit_card_scaled,centers = 4,iter.max=10000,nstart=25)
k_segments = table(km$cluster)
k_segments

# plot the final graph
# 1) The PCA plots the data in two-dimensional space
# hierarchical clustering
hc_pc = prcomp(credit_card_scaled)
fviz_pca_ind(hc_pc, habillage = clusterGroups)
#k-means
hc_pc = prcomp(credit_card_scaled)
fviz_pca_ind(hc_pc, habillage = k_segments)

# 2) silhouette plot
# hierarchical clustering
hc_sil1 = silhouette(clusterGroups, dist(credit_card_scaled, method = "euclidean"), lable = FALSE)
fviz_silhouette(hc_sil1, print.summary = FALSE) + theme_minimal()
#k-means
hc_sil = silhouette(k_segments, dist(credit_card_scaled, method = "euclidean"), lable = FALSE)
fviz_silhouette(hc_sil, print.summary = FALSE) + theme_minimal()

# find insights & analyze box plot
c = creditcardinfo
c$cluster = clusterGroups
c_plots = melt(c, id.var = "cluster")
c_plots$cluster = as.factor(c$cluster)

c_plots %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1)  +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 1) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

c_plots %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1) +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 2) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

c_plots %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = cluster), outlier.size = 1) +
  facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

## Assignment Two 
dail <- load('~/Desktop/dailvotes.Rdata')
dail
# [1] "votes"
votes [votes == 'a'] <- 'n'
votes [votes == 'y'] <- 1
votes [votes == 'n'] <- 0
votes <- as.data.frame(votes)
class(votes)
head(votes)
#                                        V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 V23
# &#211; Caol&#225;in, Caoimhgh&#237;n    0  0  1  0  0  0  1  1  1   1   1   1   1   1   0   1   0   1   1   1   0   0   0
# &#211; Cu&#237;v, &#201;amon            0  0  1  0  0  0  0  0  0   0   0   0   0   1   0   0   0   0   0   0   0   0   0
# &#211; Feargha&#237;l, Se&#225;n        0  0  1  0  0  1  0  0  0   0   0   0   1   1   1   1   1   0   0   0   0   0   1
# &#211; R&#237;ord&#225;in, Aodh&#225;n  0  0  0  1  1  1  0  0  0   0   0   0   0   0   0   0   0   0   0   0   1   1   0
# &#211; Snodaigh, Aengus                 0  0  0  0  0  0  0  1  1   1   1   1   1   1   0   1   0   0   1   1   0   0   0
# Adams, Gerry                            0  0  0  0  0  0  0  0  0   0   0   0   0   1   0   0   0   0   0   0   0   0   0

## K-means Clustering 
WGSS <- rep(0,10)
n <- nrow(votes)
WGSS[1] <- (n-1) * sum(apply(votes, 2, var))
for (k in 2:10) {WGSS[k] <- sum(kmeans(votes, centers = k)$withinss)}
plot(1:10, WGSS, type = 'b', xlab = 'k', ylab = 'Within group sum of squares')
fitkm <- kmeans(votes, centers = 8, nstart = 10)
d <- dist(votes, method = 'euclidean')^2
sil <- silhouette(fitkm$cluster,d)
plot(sil)

## Hierarchical Clustering 
d <- dist(votes, method = 'euclidean')
fit <- hclust(d, method = 'average')
plot(fit, cex = 0.5)
groups <- cutree(fit, k=8)
table(groups)
# groups
# 1  2  3  4  5  6  7  8 
# 13 56  4 76  4  9  2  2 
l <- rect.hclust(fit, k=8, border = 'blue')
install.packages("pvclust")
library(pvclust)
fit <- pvclust(votes, method.hclust="average", method.dist="euclidean")
plot(fit)

## Further Thinking 
hcl <- cutree(hclust(dist(votes)), 10)
pcl <- kmeans(votes, centers = 8)
tab <- table(hcl, pcl$cluster)
tab 
# hcl 1  2  3  4  5  6  7  8
# 1   0  0  6  0  0  0  0 12
# 2   0  0  0  0  0  0 40  0
# 3   0  0  5  0  0  0  1  0
# 4   0  4  0  0 12 19  1  0
# 5  24 13  0 15  0  0  0  0
# 6   0  0  5  0  0  0  0  0
# 7   0  0  0  0  0  0  0  2
# 8   0  0  0  2  1  0  0  0
# 9   0  0  1  0  0  0  0  1
# 10  0  0  1  0  0  0  1  0








#==============================================================================
# Problem 1
#==============================================================================
#1
df <- read.csv('wine.csv')
sds <- apply(df, 2, sd) 
means <- apply(df, 2, mean) 
x.scale <- scale(df)
#2
dist.x.scale <- dist(x.scale, method="euclidean")
hc.complete <- hclust(dist.x.scale,method="complete")
plot(hc.complete,cex=0.5)
#4
hc.single <- hclust(dist.x.scale,method="single")
hc.average <- hclust(dist.x.scale,method="average")
plot(hc.single,cex=0.5)
plot(hc.average,cex=0.5)
#7
with(df, plot(Alcohol ~ Dilution, col=memb, pch=memb))
#8
set.seed(1)
memb1 <- kmeans(x.scale, 5)$cluster
set.seed(2)
memb2 <- kmeans(x.scale, 5)$cluster
set.seed(3)
memb3 <- kmeans(x.scale, 5)$cluster
with(df, plot(Alcohol ~ Dilution, col=memb1, pch=memb1))
with(df, plot(Alcohol ~ Dilution, col=memb2, pch=memb2))
with(df, plot(Alcohol ~ Dilution, col=memb3, pch=memb3))

make_cluster_graph <- function(df, x, y, number_clusters, seeds=c(1, 2, 3)) {
    par(mfrow=c(1,3)) 
    for (seed in seeds) {
        set.seed(seed)
        members <- kmeans(df[, x], number_clusters)$cluster
        #plot(df[, y] ~ df[, x], col=members, pch=members)
        tables(member)
    } 
}
#12
set.seed(12)
memb12 <- kmeans(x.scale, 3)$cluster
hc.complete.memb <- cutree(hc.complete, k=3)
sum(diag(table(memb12, hc.complete.memb))) / sum(table(memb12, hc.complete.memb))

#==============================================================================
# Problem 2
#==============================================================================
#13
x <- read.csv('wine.csv')
pr_comps <- prcomp(x, scale=TRUE)
#15
summary(pr_comps)
#17
biplot(pr_comps, scale=0)
pr_comps$x[,1][159]
pr_comps$x[,2][159]

#==============================================================================
# Problem 3
#==============================================================================
#19
df <- read.csv('GeneExpression.csv', header=FALSE)
df <- t(df)
df <- scale(df)
means2 <- apply(df[21:40, ], 2, mean)
hist(means2)
#21
dist_df <- dist(df, method="euclidean")
hc_complete <- hclust(dist_df,method="complete")
hc_members <-cutree(hc_complete, k=2)
#23
pr_comps2 <- prcomp(df)
biplot(pr_comps2)

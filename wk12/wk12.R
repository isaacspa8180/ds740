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


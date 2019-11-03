library(cluster)
library(FactoMineR)
library(factoextra)

df <- iris[c(1, 2, 3, 4)]
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)
autoplot(prcomp(df))

res.pca <- PCA(decathlon2.active, graph = FALSE,scale.unit = TRUE)
res.hcpc <- HCPC(res.pca, graph = FALSE)

f<- prcomp(decathlon2.active,scale. = TRUE)
#### f$x is equal to res.pca$ind$cood #######

fviz_nbclust(f$x, pam, method = "silhouette")+theme_classic()
clara.res <- clara(f$x, 3, pamLike = TRUE)

fviz_cluster(clara.res,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

autoplot(cluster::clara(f$x, 3))
autoplot(stats::kmeans(f$x,3),data=f$x)

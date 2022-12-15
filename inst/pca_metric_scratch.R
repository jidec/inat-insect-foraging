all_diffs_df <- as.data.frame(do.call(rbind, all_diffs))
pca <- prcomp(all_diffs_df)
cbind(all_diffs_df,pca$x)
View(obs[[2]])

test_data <- as.data.frame(cbind(pca$x,as.character(obs[[2]]$species[1:1000]),as.numeric(d_mean_v),as.character(d_argmax_v)))

test_data$PC1 <- as.numeric(test_data$PC1)
test_data$V6 <- as.character(test_data$V6)
test_data$d_mean_v <- as.character(test_data$V6)
model <- lm(PC1~V6,data=test_data)
summary(model)
View(test_data)
temp_pca <- pca$
    plot(pca$x[,1],pca$x[,2],xlim=c(-50,50),ylim=c(-50,50))

View(test)
par(mfrow = c(1,3))
plot(pca$rotation[,3])
summary(pca)

View(all_diffs_df)
dd <- dist(all_diffs_df) # euclidean distances between the rows
d
og_fit <- fit
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
View(d)

fit$points <- fit$points[fit$points[,2] < 50 & fit$points[,1] < 50,]
fit$points <- fit$points[obs[[2]]$species[1:1000] == "Vanessa cardui (Linnaeus, 1758)",]
fit$points
obs[[2]]$species[1:1000] == "Vanessa cardui (Linnaeus, 1758)"
rm(usa_insects)
# plot solution
x <- fit$points[,1]
y <- fit$points[,2]

dev.off()

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", col = factor(obs[[2]]$sp_mod[1:1000]),xlim=c(-10,10),ylim=c(-10,10))

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", col = factor(obs[[2]]$sp_mod[1:1000]),xlim=c(-10,10),ylim=c(-10,10))

text(x, y, labels = obs[[2]]$sp_mod[1:1000], cex=1)

obs[[2]]$species
obs[[2]]$sp_mod <- t
t <- substr(obs[[2]]$species,start=1,stop=3)
t
# K-Means Cluster Analysis
fit <- kmeans(pca$x[,1:3], 3) # 5 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

#obs = obs1
#nbins = 8
#i = 1
#i = 2
#start_trim = 2
#end_trim = 1

addNewForagingMetricsFromObs <- function(obs, nbins=-1, start_trim=-1,end_trim=-1, mds=T, pca=T){
    source("src/padBinFreqHours.R")
    source("src/getHrsTupleFromObs.R")
    library(diptest)
    hrs <- getHrsTupleFromObs(obs)
    sc_hrs <- hrs[[2]]
    ssc_hrs <- hrs[[1]]

    d_argmax_v <- c()
    d_mean_v <- c()
    all_diffs <- list()
    d_dip <- c()
    d_first_half_sum <- c()
    d_second_half_sum <- c()

    for(i in 1:length(sc_hrs)){
        d <- getSpExpecDiff(padBinFreqHours(ssc_hrs[[i]],nbins,start_trim,end_trim),padBinFreqHours(sc_hrs[[i]],nbins,start_trim,end_trim))
        all_diffs <- append(all_diffs, list(d))
        d0 <- d - min(d)
        d_mean_v <- c(d_mean_v, sum(as.numeric(names(d0))*d0)/sum(d0))
        d_argmax_v <- c(d_argmax_v,as.numeric(names(which(d==max(d)))))

        d_dip <- c(d_dip, dip(d))

        d_first_half_sum <- c(d_first_half_sum, sum(d[1:4]))
        d_second_half_sum <- c(d_second_half_sum, sum(d[5:8]))
    }

    obs[[2]]$d_mean_v <- d_mean_v
    obs[[2]]$diff_argmax <- d_argmax_v
    obs[[2]]$d_dip <- d_dip
    obs[[2]]$d_half1_sum <- d_first_half_sum
    obs[[2]]$d_half2_sum <- d_second_half_sum

    all_diffs_df <- as.data.frame(do.call(rbind, all_diffs))

    if(pca){
        pca <- prcomp(all_diffs_df)

        obs[[2]]$pc1 <- pca$x[,1]
        obs[[2]]$pc2 <- pca$x[,2]
        print(pca$rotation)
        print(summary(pca))
    }

    if(mds){
        dists <- dist(all_diffs_df) # euclidean distances between the rows
        fit <- cmdscale(dists,eig=TRUE, k=2) # k is the number of dim]

        mds1 <- fit$points[,1]
        mds2 <- fit$points[,2]
        obs[[2]]$mds1 <- mds1
        obs[[2]]$mds2 <- mds2
    }


    # K-Means Cluster Analysis
    #fit <- kmeans(pca$x[,1:3], 3) # 5 cluster solution
    # get cluster means
    #aggregate(mydata,by=list(fit$cluster),FUN=mean)
    # append cluster assignment
    #mydata <- data.frame(mydata, fit$cluster)

    return(obs)
}

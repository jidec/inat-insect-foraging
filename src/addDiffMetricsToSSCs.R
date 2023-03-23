
addDiffMetricsToSSCs <- function(sscs, nbins=-1, start_trim=-1,end_trim=-1, mds=F, pca=T){
    source("src/padBinFreqHours.R")
    source("src/getSpExpecDiff.R")
    source("src/getHrsTupleFromObs.R")
    library(diptest)
    sc_hrs <- sscs$obs
    ssc_hrs <- sscs$bl_obs

    all_diffs <- list()

    # mean interval (an hour or number representing a set of hours) of the differences
    # intended to represent the peak of activity during the day by the mean of when it's higher than expected
    diff_mean_interval <- c()
    # the unimodality of the differences
    # less unimodal diffs have more peaks in when activity is higher than expected
    diff_unimod <- c()

    #
    diff_half1_sum <- c()
    diff_half2_sum <- c()
    diff_third1_sum <- c()
    diff_third2_sum <- c()
    diff_third3_sum <- c()

    diff_third1_mean <- c()
    diff_third2_mean <- c()
    diff_third3_mean <- c()

    diff_total <- c()
    diff_abs_total <- c()
    hr_freqs <- list()

    for(i in 1:length(sc_hrs)){
        hf <- padBinFreqHours(ssc_hrs[[i]],nbins,start_trim,end_trim)
        d <- getSpExpecDiff(hf,padBinFreqHours(sc_hrs[[i]],nbins,start_trim,end_trim))
        all_diffs <- append(all_diffs, list(d))
        d0 <- d - min(d)

        diff_mean_interval <- c(diff_mean_interval, sum(as.numeric(names(d0))*d0)/sum(d0))

        diff_unimod <- c(diff_unimod, dip(d))

        l <- length(d)
        diff_half1_sum <- c(diff_half1_sum, sum(d[1:(l/2)]))
        diff_half2_sum <- c(diff_half2_sum, sum(d[(l/2+1):l]))

        diff_third1_sum <- c(diff_third1_sum, sum(d[1:(l/3)]))
        diff_third2_sum <- c(diff_third2_sum, sum(d[(l/3+1):((l/3)*2)]))
        diff_third3_sum <- c(diff_third3_sum, sum(d[((l/3)*2):l]))

        diff_third1_mean <- c(diff_third1_mean, mean(d[1:(l/3)]))
        diff_third2_mean <- c(diff_third2_mean, mean(d[(l/3+1):((l/3)*2)]))
        diff_third3_mean <- c(diff_third3_mean, mean(d[((l/3)*2):l]))
        diff_total <- c(diff_total,sum(d))
        diff_abs_total <- c(diff_abs_total,sum(abs(d)))

        hr_freqs <- append(hr_freqs,list(hf)) # add hr freq curve
    }

    sscs$diff_mean_interval <- diff_mean_interval
    sscs$diff_unimod <- diff_unimod
    sscs$diff_half1_sum <- diff_half1_sum
    sscs$diff_half2_sum <- diff_half2_sum
    sscs$diff_third1_sum <- diff_third1_sum
    sscs$diff_third2_sum <- diff_third2_sum
    sscs$diff_third3_sum <- diff_third3_sum

    sscs$diff_third1_mean <- diff_third1_mean
    sscs$diff_third2_mean <- diff_third2_mean
    sscs$diff_third3_mean <- diff_third3_mean
    sscs$diff_total <- diff_total
    sscs$diff_abs_total <- diff_abs_total
    sscs$hr_freqs <- hr_freqs

    all_diffs_df <- as.data.frame(do.call(rbind, all_diffs))

    if(pca){
        pca <- prcomp(all_diffs_df)

        sscs$pc1 <- pca$x[,1]
        sscs$pc2 <- pca$x[,2]
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

    return(sscs)
}

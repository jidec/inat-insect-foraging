#start_trim=5
#end_trim=5

# add hr freqs (tabled hours/a curve of hrs)
# and pcas
addHrFreqsToSSCs <- function(sscs,start_trim=6,end_trim=6,thin=T,normalize=T,scale=F){
    library(stringr)
    hr_freqs <- list()
    for(i in 1:nrow(sscs)){
        hf <- padBinFreqHours(sscs$obs[[i]],-1,start_trim,end_trim)
        hr_freqs <- append(hr_freqs,list(hf))
    }
    sscs$hr_freqs <- hr_freqs

    bound <- data.frame(do.call(rbind,sscs$hr_freqs))
    colnames(bound) <- str_replace(colnames(bound),"X","")

    if(thin){
        for(r in 1:nrow(bound)){
            sdev_obs_per_hr <- sqrt(var(as.numeric(bound[r,])))
            med_obs_per_hr <- median(as.numeric(bound[r,]))
            bound[r,bound[r,] > med_obs_per_hr + sdev_obs_per_hr] <- med_obs_per_hr
        }
    }

    if(normalize){
        for(r in 1:nrow(bound)){
            bound[r,] <- bound[r,] / max(bound[r,])
        }
    }

    if(scale){
        for(c in 1:ncol(bound)){
            bound[,c] <- scale(bound[,c])
        }
    }

    pcdata <- prcomp(bound)
    print(summary(pcdata))
    plot(y=pcdata$rotation[,1],x=colnames(bound))
    plot(y=pcdata$rotation[,2],x=colnames(bound))
    plot(y=pcdata$rotation[,3],x=colnames(bound))
    plot(y=pcdata$rotation[,4],x=colnames(bound))
    plot(y=pcdata$rotation[,5],x=colnames(bound))
    sscs$freq_pc1 <- pcdata$x[,1]
    sscs$freq_pc2 <- pcdata$x[,2]
    sscs$freq_pc3 <- pcdata$x[,3]
    sscs$freq_pc4 <- pcdata$x[,4]
    sscs$freq_pc5 <- pcdata$x[,5]

    return(sscs)
}

addShpPCs <- function(sscs){
    library("Momocs")
    hr_freqs_shp <- sscs$hr_freqs
    for(i in 1:length(hr_freqs_shp)){
        curve_mat <- cbind(as.numeric(names(hr_freqs_shp[[i]])),hr_freqs_shp[[i]])
        #curve_mat[,2] <- curve_mat[,2] /  max(curve_mat[,2])
        #curve_mat[,2] <- scale(curve_mat[,2],center=TRUE)
        hr_freqs_shp[[i]] <- curve_mat
        rownames(hr_freqs_shp[[i]]) <- NULL
    }
    shps <- Momocs::Opn(hr_freqs_shp)
    op <- opoly(shps,degree=4)
    op.p <- PCA(op)

    sscs$shp_pc1 <- op.p$x[,1]
    sscs$shp_pc2 <- op.p$x[,2]
    plot(op.p$rotation[,1])
    plot(op.p$rotation[,2])

    plot(op.p,cex=0.1)

    return(sscs)
}

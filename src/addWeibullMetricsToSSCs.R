
# must edit iterations manually within batchWeib
addWeibullMetricsToSSCs <- function(sscs,ncores=7,iters=50, ci=FALSE, bootstraps=25){
    library(parallel)
    library(phenesse)
    library(pbapply)

    cl <- makeCluster(ncores,type="SOCK")
    list <- sscs$obs
    #out <- clusterApply(cl, list, batchWeib)#, iters=iters, ci=ci, bootstraps=bootstraps)
    out <- pblapply(cl=cl, list, batchWeib, iters=iters, ci=ci, bootstraps=bootstraps)
    #?pblapply
    stopCluster(cl)
    out <- as.data.frame(do.call(rbind, out))

    return(cbind(sscs,out))
}

batchWeib <- function(obs, iters=50, ci=FALSE, bootstraps=25){

    if(length(obs) > 80){
        obs <- sample(obs,80)
    }

    # onset
    q5 <- tryCatch(phenesse::weib_percentile(observations = obs, percentile = 0.05, iterations = iters),
                   error = function(e) NA)
    # mid
    q50 <- tryCatch(phenesse::weib_percentile(observations = obs, percentile = 0.5, iterations = iters),
                    error = function(e) NA)
    # offset
    q95 <- tryCatch(phenesse::weib_percentile(observations = obs, percentile = 0.95, iterations = iters),
                    error = function(e) NA)

    # out
    rdf <- data.frame(q5 = q5, q50 = q50, q95 = q95)

    return(rdf)
}

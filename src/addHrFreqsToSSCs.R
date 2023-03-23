
# add hr freqs (tabled hours/a curve of hrs)
# and pcas
addHrFreqsToSSCs <- function(sscs,start_trim=-1,end_trim=-1){
    hr_freqs <- list()
    for(i in 1:nrow(sscs)){
        hf <- padBinFreqHours(sscs$obs[[i]],-1,start_trim,end_trim)
        hr_freqs <- append(hr_freqs,list(hf))
    }
    sscs$hr_freqs <- hr_freqs

    bound <- data.frame(do.call(rbind,ssc$hr_freqs))
    colnames(bound) <- str_replace(colnames(bound),"X","")
    pcdata <- prcomp(bound)
    #plot(y=pcdata$rotation[,1],x=colnames(bound))
    #plot(y=pcdata$rotation[,2],x=colnames(bound))
    sscs$freq_pc1 <- pcdata$x[,1]
    sscs$freq_pc2 <- pcdata$x[,2]

    return(sscs)
}

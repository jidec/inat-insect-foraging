
getHrsTupleFromObs <- function(obs){
    # extract bf hours and baseline hours
    obs_hrs <- list()
    all_hrs <- list()

    for(i in 1:length(obs[[1]]))
    {
        hours <- obs[[1]][[i]]
        split_index <- which(hours == -1)
        bf <- hours[1:split_index-1]
        bl <- hours[split_index+1:length(hours)]
        bl <- na.omit(bl)
        obs_hrs <- c(obs_hrs,list(bf))
        all_hrs <- c(all_hrs,list(bl))
    }

    return(list(obs_hrs,all_hrs))
}

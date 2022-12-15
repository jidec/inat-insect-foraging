#hrs = ssc_hrs[[1]]
#nbins = 6
padBinFreqHours <- function(hrs,nbins=-1,start_trim=-1,end_trim=-1){

    # get freq table
    hrs <- table(hrs)

    # find hrs that need zeroes
    missing <- setdiff(1:24,as.numeric(names(hrs)))

    # append named zeroes
    for(hr in 1:24){
        if(hr %in% missing){
            named_hr <- 0
            names(named_hr) <- hr
            hrs <- c(hrs,named_hr)
        }
    }

    # sort 1-24
    hrs <- hrs[order(as.numeric(names(hrs)))]

    # set 0 at midnight
    hrs[24] <- 0
    hrs[1] <- 0

    if(nbins != -1){
        n_per_bin <- 24/nbins
        counter <- 0
        bin <- c()
        bins <- c()
        for(hr in 1:24){
            if(counter == 0){
                first_hr <- hr
            }
            counter <- counter + 1
            bin <- sum(bin,hrs[hr])
            if(counter >= n_per_bin){
                names(bin) <- mean(c(first_hr,hr))#paste0(as.character(first_hr),"_", as.character(hr))
                bins <- c(bins,bin)
                bin <- c()
                counter <- 0
            }

        }
        hrs <- bins
    }

    if(start_trim != -1){
        hrs <- hrs[(start_trim+1):length(hrs)]
    }
    if(end_trim != -1){
        hrs <- hrs[1:(length(hrs)-end_trim)]
    }

    return(hrs)
}

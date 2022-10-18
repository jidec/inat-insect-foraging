
tablePadHours <- function(hrs){
    hrs <- table(hrs)

    #start_hr <- as.numeric(names(hrs)[1])
    #end_hr <- as.numeric(names(hrs)[length(hrs)])
    #hrs <- c(rep(0,start_hr-1),hrs,rep(0,24-end_hr))

    missing <- setdiff(1:24,as.numeric(names(hrs)))
    for(m in 1:length(missing)){
        hrs <- append(hrs, 0, after=missing[m])
    }

    names(hrs) <- 1:24
    hrs[1:5] <- 0.0000001

    return(hrs)
}

untableByNames <- function(v,m=1){
    v <- v * m
    n_v <- c(0)
    #i = 7
    for(i in 1:length(v))
    {
        #obs <- round(v[i])
        #hr <- as.numeric(names(v[i]))
        #obs
        #hr
        #rep(as.numeric(names(v[i])),round(v[i]))
        n_v <- c(n_v,rep(as.numeric(names(v[i])),round(v[i])))
    }
    #n_v <- n_v / 10000
    return(n_v)
}

deconvolveSSCHours <- function(hrs,bl_hrs,method="fourier",ut_mult=1000){
    yc <- tablePadHours(hrs)
    yc_is_zero <- yc < 0.0001

    c <- tablePadHours(bl_hrs)

    ydc <- NA
    if(method == "fourier"){
        ydc <- Re(fft(fft(yc)/fft(c),inverse=TRUE))
    }
    else if(method == "RL"){
        ydc <- rPeaks::SpectrumDeconvolution(yc,c,iterations=5,repetitions=1,boost=1,method="RL")
    }
    else if(method =="gold"){
        ydc <- rPeaks::SpectrumDeconvolution(yc,c,iterations=5,repetitions=1,boost=1,method="Gold")
    }
    else if(method == "weiner")
    {
        c.fft <- fft(c)
        c.fft.mod <- abs(c.fft)^2

        # Wiener decovolution algorithm
        snr <- rep(1, length(c.fft))
        snr[c.fft.mod<0.05] <- 0.05
        ydc <- Re(fft(fft(yc)/fft(c)*abs(fft(c))^2/(abs(fft(c))^2+1/snr), inverse = TRUE)) #/nfft)
        #ydc <- scale(fft_z,center=F) + 2
    }

    # fix ydc
    ydc <- ydc + (-min(ydc))
    ydc[yc_is_zero] <- 0.0001

    par(mfrow = c(1, 3))
    plot(yc)
    plot(c)
    plot(ydc)

    yc_ut <- untableByNames(yc)
    print(paste("YC (Conv Hrs) Mean",mean(yc_ut)))

    ydc_ut <- untableByNames(ydc,ut_mult)
    ydc_ut[1] <- 1
    print(paste("YDC (Deconv Hrs) Mean",mean(ydc_ut)))

    print(paste("YC (Conv Hrs) Onset",weib_percentile(observations = hrs, percentile = 0.05, iterations = 25)))
    print(paste("YDC (Deconv Hrs) Onset",weib_percentile(observations = ydc_ut, percentile = 0.05, iterations = 25)))
    print(paste("YC (Conv Hrs) Offset",weib_percentile(observations = hrs, percentile = 0.95, iterations = 25)))
    print(paste("YDC (Deconv Hrs) Offset",weib_percentile(observations = ydc_ut, percentile = 0.95, iterations = 25)))

    return(list(yc,c,ydc,yc_ut,ydc_ut))
}

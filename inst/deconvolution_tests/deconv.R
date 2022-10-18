###
y <- c(0, 0, 0, 1, 1, 1, 0, 0, 0)
plot(y)
c <- c(3, 2, 1, 0, 0, 0, 0, 0, 0)
plot(c)


#yc <- conv(y,c)
yc <- Re(fft(fft(y) * fft(c),inverse=TRUE))
plot(yc)

ydc<- Re(fft(fft(yc)/fft(c),inverse=TRUE))
plot(ydc)

all_hours <- c()
for(i in 1:length(bf_hours)){
    all_hours <- c(all_hours,bf_hours[[i]])
}

tablePadHours <- function(hours){
    hours <- table(hours)
    start_hour <- as.numeric(names(hours)[1])
    end_hour <- as.numeric(names(hours)[length(hours)])
    hours <- c(rep(0,start_hour-1),hours,rep(0,24-end_hour))
    return(hours)
}

yc <- padHours(bf_hours[[31]])
yc_is_zero <- yc < 0.001
c <- padHours(all_hours)

#yc <- scale(table(hours),center=F)
# library(MASS)
#d <- fitdistr(hours,"weibull")
#yc <- scale(dweibull(1:22,shape=d$estimate[1],scale=d$estimate[2]),center=F)

plot(yc,xlim=c(0,24))
plot(c,xlim=c(0,24))
length(yc)
length(c)

#yc <- c(yc,rep(0,length(c) - length(yc)))
#yc <- conv(y,c)
#yc <- Re(fft(fft(y) * fft(c),inverse=TRUE))

ydc <- Re(fft(fft(yc)/fft(c),inverse=TRUE))
#ydc <- scale(ydc,center=F)
names(ydc) <- 1:24
ydc <- ydc + 0.2
ydc[yc_is_zero] <- 0.0001
plot(ydc*1000)
ydc_ut <- untableByNames(ydc)
mean(ydc_ut)

names(yc) <- 1:24
yc_ut <- untableByNames(yc)
mean(yc_ut)
mean(ydc_ut)

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

ydc <- untableByNames(ydc)
plot(ydc)


#ydc_df <- data.frame(ydc)
#colnames(ydc_df) <- c("n_obs")
#ydc_df$hour <- 1:24
#library(ggplot2)
#ggplot(ydc, aes(x=hour,y=n_obs)) +
#    geom_point() +
    #geom_smooth(method="loess",span=0.3)
#    geom_smooth()

fitdistr(ydc_df$hour,"normal")
ydc
rep(as.numeric(ydc,names(ydc)))


untableByNames <- function(v){
    v <- v * 10000
    n_v <- c(0)
    for(i in 1:length(v))
    {
        n_v <- c(n_v,rep(names(v[i]),round(v[i])))
    }
    n_v <- n_v / 10000
    return(n_v)
}

for(i in 1:length(ydc))
{
    n_ydc <- c(n_ydc,rep(names(ydc[i]),round(ydc[i])))
}

mean(as.numeric(n_ydc))


ydc
invTable = function(tb, random = TRUE){
    output = rep(names(tb), tb)
    return(output)
}

ydc_df$hour_freq <- ydc_df$n_obs * ydc_df$hour

mean(ydc_df$hour_freq)
library(smoother)
ydcs <- smth(ydc,method = "gaussian",window=1,alpha=1)
plot(ydc)
plot(ydcs)
par(mfrow =c(1,3))
# wiener
c.fft <- fft(c)
c.fft.mod <- abs(c.fft)^2

# Wiener decovolution algorithm
snr <- rep(1, length(c.fft))
snr[c.fft.mod<0.05] <- 0.05
fft_z <- Re(fft(fft(yc)/fft(c)*abs(fft(c))^2/(abs(fft(c))^2+1/snr), inverse = TRUE)) #/nfft)
fft_z <- scale(fft_z,center=F) + 2
#fft_z[0:3] <- 0
#fft_z[19:23] <- 0

plot(c)


plot(yc)
plot(c)
plot(fft_z)
par(mfrow=c(1,3))
plot(fft_z)
#edge effects


de <- rPeaks::SpectrumDeconvolution(yc,c,iterations=5,repetitions=1,boost=1)
plot(rPeaks::SpectrumDeconvolution(yc,c,iterations=1,repetitions=1,boost=1,method="RL"))
plot(de)
View(yc)
plot(yc)
c <- c * 0.5
yc[yc==0] <- 0.000000001

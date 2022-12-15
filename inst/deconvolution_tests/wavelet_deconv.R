data <- waved.example(TRUE, TRUE)  
attach(data)
names(data)

my.own.simulation <- waved.example(FALSE)  
4096

original.data.with.figures <- waved.example(TRUE, TRUE)  
original.data.without.figures <- waved.example(TRUE, FALSE)  

plot(t, lidar, type="l")  
plot(t, lidar.blur, type="l")
plot(t, lidar.noisy, type="l") 

lidar.wvd <- WaveD(lidar.blur, g, F=6, thr=0)
multires(lidar.wvd$w, lo=3, hi=6) 
lidar.noisy.wvd <- WaveD(lidar.noisy, g, F=6, thr=0) 
multires(lidar.noisy.wvd$w, lo=3, hi=6)

lidar.w <- FWaveD(lidar.blur, g, F=6)
lidar.wavelet.coef.at.level.5 <- lidar.w[dyad(5)]
inverse.waved <- IWaveD(lidar.w)

plot(t, WaveD(lidar.noisy, g, F=6, thr=0.2)$ord, type="l") 
plot(t, WaveD(lidar.noisy, g, F=6, thr=0.02)$ord, type="l") 

my.thr <- c(0.01, 0.02, 0.03, 0.04)
lidar.my.thr.wvd <- WaveD(lidar.noisy, g, L=3, F=6, thr=my.thr)
lidar.maxi.wvd <- WaveD(lidar.noisy, g)
round(maxithresh(lidar.noisy, g, L=3, F=6), 4)

unthresholded.w <- lidar.maxi.wvd$w
multires(unthresholded.w, lo=3, hi=6)  
thresholded.w <- lidar.maxi.wvd$w.thr
multires(thresholded.w, lo=3, hi=6) 

lidar.Fmax.wvd <- WaveD(lidar.noisy, g, F=10)
multires(lidar.Fmax.wvd$w.thr)
plot(t, lidar.Fmax.wvd$ord, type="l") 

print(find.j1(g, scale(lidar.noisy))) 

plotspec(g, scale(lidar.noisy))
plotspec(g.noisy, scale(lidar.noisy))  

plot(t, lidar.maxi.wvd$ord, type="l") 
plot(t, lidar.maxi.wvd$waved, type="l") 

lidar.ti.fast.waved <- WaveD(lidar.noisy, g, MC=TRUE)

plot(t, WaveD(lidar.noisy, g, SOFT=FALSE)$ord, type="l")  
plot(t, WaveD(lidar.noisy, g, SOFT=TRUE)$ord, type="l") 

doppler.wvd <- WaveD(doppler.noisy, g)  
lidarT.wvd <- WaveD(lidar.noisyT, g)
plot(doppler.wvd)
summary(doppler.wvd)

plot(lidarT.wvd)
summary(lidarT.wvd)   

plot(WaveD(lidar.noisyT, g, SOFT=TRUE)$ord, type="l")
plot(WaveD(lidar.noisyT, g, SOFT=TRUE, eta=sqrt(8))$ord, type="l")
plot(WaveD(lidar.noisyT, g, SOFT=FALSE, eta=sqrt(8))$waved, type="l")
plot(WaveD(lidar.noisyT, g, SOFT=FALSE, eta=sqrt(12))$waved, type="l")

lidar.NEV.wvd <- WaveD(lidar.noisy, g.noisy)
plot(lidar.NEV.wvd)

wL <- rep(0,2048)
wR <- rep(0,2048)
wL[dyadjk(4,3)] <- 1
wR[dyadjk(6,40)] <- 1
plot(t, IWaveD(wL,3), type="l")
plot(t, IWaveD(wR,3), type="l")


# wavelet deconv
plot(WaveD(yc, c, SOFT=TRUE)$ord, type="l")
plot(yc)
plot(c)

c <- as.numeric(c)
e <- WaveD(yc,c)

View(e)
View(yc)
View(c)
View(lidar)

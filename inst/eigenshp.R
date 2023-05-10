
library("Momocs")
sscs <- addHrFreqsToSSCs(sscs,start_trim=6,end_trim=6)
hr_freqs_shp <- sscs$hr_freqs
for(i in 1:length(hr_freqs_shp)){
    curve_mat <- cbind(as.numeric(names(hr_freqs_shp[[i]])),hr_freqs_shp[[i]])
    curve_mat[,2] <- curve_mat[,2] /  max(curve_mat[,2])
    curve_mat[,2] <- scale(curve_mat[,2],center=TRUE)
    hr_freqs_shp[[i]] <- curve_mat
    rownames(hr_freqs_shp[[i]]) <- NULL
}
shps <- Momocs::Opn(hr_freqs_shp)
op <- opoly(shps)
op.p <- PCA(op)

plot(op.p,cex=0.1)

sscs$shp_pc1 <- op.p$x[,1]
sscs$shp_pc2 <- op.p$x[,2]
plot(op.p$rotation[,1])
plot(op.p$rotation[,2])

plot(op.p,cex=0.1)
bot.f <- efourier(shps)
bot.p <- PCA(bot.f)
bot.p
plot(bot.p, morpho=FALSE)
plot(bot.p, 'type')

op <- npoly(shps, 5)
?npoly
op.p <- PCA(op)
op.p
plot(op.p, 1, morpho=TRUE)
plot(op.p,points=FALSE)

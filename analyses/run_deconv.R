# make sure usa_insects and bf_hours are loaded

for(i in 6:10){
    yc_c_ydc <- deconvolveSSCHours(hrs=bf_hours[[i]],bl_hrs=usa_insects$local_hour,method="weiner",ut_mult=10000)
}
yc_c_ydc <- deconvolveSSCHours(hrs=bf_hours[[1]],bl_hrs=usa_insects$local_hour,method="weiner",ut_mult=10000)
yc <- yc_c_ydc[[1]]
c <- yc_c_ydc[[2]]
ydc <- yc_c_ydc[[3]]
yc_ut <- yc_c_ydc[[4]]
ydc_ut <- yc_c_ydc[[5]]
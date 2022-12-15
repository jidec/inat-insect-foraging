
plot(tablePadScaleHours(bf_hours[[1]]))

plot(tablePadScaleHours(bf_bl_hours[[1]]))

plot(tablePadScaleHours(bf_hours[[1]]) %/% tablePadScaleHours(bf_bl_hours[[1]]))
plot(t)

length(bf_hours[[1]]) / length(usa_insects$local_hour)
table(usa_insects$local_hour) * 4.01946e-05

hist(bf_hours[[1]],xlim=c(0,24))
hist(usa_insects$local_hour,xlim=c(0,24))


hrs <- binPadScaleHours(bf_hours[[2]])
hrs2 <- binPadScaleHours(usa_insects$local_hour) * 4.01946e-05
plot(hrs2)
plot(hrs)
plot(hrs - hrs2)

#2
hrs <- bf_hours[[1]]
scaleBinHours <- function(hrs){

    saved <- hrs
    hrs <- table(hrs)

    # add 0s to missing hours for rescaling purposes
    missing <- setdiff(1:24,as.numeric(names(hrs)))
    for(m in 1:length(missing)){
        hrs <- append(hrs, 0, after=missing[m])
    }
    names(hrs) <- 1:24
    # set midnight to 0 (midnight obs are erroneous)
    hrs[1] <- 0

    # scale from 1 to 2
    hrs <- rescale(hrs, to=c(1,2))

    period <- hrs
    period[(hrs >= 0 & hrs <= 4) | (hrs >= 22 & hrs <= 24)] <- NA
    period[hrs >= 5 & hrs <= 8] <- "1"
    period[hrs >= 9 & hrs <= 12] <- "2"
    period[hrs >= 13  & hrs <= 16] <- "3"
    period[hrs >= 17  & hrs <= 21] <- "4"
    period
    return(hrs)
}

#3
library("scales")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

binPadScaleHours <- function(hrs,scale=F){
    hrs <- table(hrs)

    #start_hr <- as.numeric(names(hrs)[1])
    #end_hr <- as.numeric(names(hrs)[length(hrs)])
    #hrs <- c(rep(0,start_hr-1),hrs,rep(0,24-end_hr))

    missing <- setdiff(1:24,as.numeric(names(hrs)))
    for(m in 1:length(missing)){
        hrs <- append(hrs, 0, after=missing[m])
    }

    names(hrs) <- 1:24
    hrs[1] <- 0

    if(scale){
        hrs <- rescale(hrs, to=c(1,2))
    }

    return(hrs)
}

binTableScaleHours <- function(hrs){
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

    # scale from 1 to 2
    hrs <- rescale(hrs, to=c(1,2))

    bl_period <- baseline
    bl_period[(baseline >= 0 & baseline <= 4) | (baseline >= 22 & baseline <= 24)] <- NA
    bl_period[baseline >= 5 & baseline <= 8] <- "1"
    bl_period[baseline >= 9 & baseline <= 12] <- "2"
    bl_period[baseline >= 13  & baseline <= 16] <- "3"
    bl_period[baseline >= 17  & baseline <= 21] <- "4"

    return(hrs)
}

#4

obs <- bf_hours[[5]]
baseline <- usa_insects$local_hour

usa
getAdjPeriods <- function(obs,baseline){
    obs_period <- obs
    obs_period[(obs >= 0 & obs <= 4) | (obs >= 22 & obs <= 24)] <- NA
    obs_period[obs >= 5 & obs <= 8] <- "1"
    obs_period[obs >= 9 & obs <= 12] <- "2"
    obs_period[obs >= 13  & obs <= 16] <- "3"
    obs_period[obs >= 17  & obs <= 21] <- "4"

    bl_period <- baseline
    bl_period[(baseline >= 0 & baseline <= 4) | (baseline >= 22 & baseline <= 24)] <- NA
    bl_period[baseline >= 5 & baseline <= 8] <- "1"
    bl_period[baseline >= 9 & baseline <= 12] <- "2"
    bl_period[baseline >= 13  & baseline <= 16] <- "3"
    bl_period[baseline >= 17  & baseline <= 21] <- "4"

    bl_period <- as.vector(table(bl_period))
    bl_period <- scale(bl_period,center=F)

    obs_period <- as.vector(table(obs_period))
    obs_period <- scale(obs_period,center=F)

    bl_period
    obs_period
    plot(obs_period,ylim=c(0,1.5))
    plot(bl_period,ylim=c(0,1.5))
    plot(obs_period %/% bl_period,ylim=c(0,1.5))
    table(bl_period)

    table(obs_period) / table(bl_period)
}

getAdjPeriods <- function(obs,baseline){
    obs_period <- obs
    obs_period[(obs >= 0 & obs <= 4) | (obs >= 22 & obs <= 24)] <- NA
    obs_period[obs >= 5 & obs <= 8] <- "1"
    obs_period[obs >= 9 & obs <= 12] <- "2"
    obs_period[obs >= 13  & obs <= 16] <- "3"
    obs_period[obs >= 17  & obs <= 21] <- "4"

    bl_period <- baseline
    bl_period[(baseline >= 0 & baseline <= 4) | (baseline >= 22 & baseline <= 24)] <- NA
    bl_period[baseline >= 5 & baseline <= 8] <- "1"
    bl_period[baseline >= 9 & baseline <= 12] <- "2"
    bl_period[baseline >= 13  & baseline <= 16] <- "3"
    bl_period[baseline >= 17  & baseline <= 21] <- "4"

    bl_period <- as.vector(table(bl_period))
    bl_period <- scale(bl_period,center=F)

    obs_period <- as.vector(table(obs_period))
    obs_period <- scale(obs_period,center=F)

    bl_period
    obs_period
    plot(obs_period,ylim=c(0,1.5))
    plot(bl_period,ylim=c(0,1.5))
    plot(obs_period %/% bl_period,ylim=c(0,1.5))
    table(bl_period)

    table(obs_period) / table(bl_period)
}



plot(tablePadScaleHours(bf_hours[[5]]))
plot(tablePadScaleHours(usa_insects$local_hour))
plot(tablePadScaleHours(bf_hours[[5]]) / tablePadScaleHours(usa_insects$local_hour))

#5

hist(bf_hours[[5]],xlab="Hour",xlim=c(0,24),main="Activity Curve of a Species")
hist(usa_insects$local_hour,xlab="Hour",xlim=c(0,24),main="Activity Curve of all Insects (Proxy for Observer Effort)")

library(dplyr)
# perform binning with specific number of bins
df %>% mutate(new_bin = cut(Hour, breaks=7))

df <- data.frame(bf_hours[[5]])
df$Period <- df$Hour
df$Period[df$Hour >= 1 & df$Hour <= 4] <- 1
df$Period[df$Hour >= 5 & df$Hour <= 8] <- 2
df$Period[df$Hour >= 9  & df$Hour <= 12] <- 3
df$Period[df$Hour >= 13 & df$Hour <= 16] <- 4
df$Period[df$Hour >= 17 & df$Hour <= 20] <- 5
df$Period[df$Hour >= 21 & df$Hour <= 24] <- 6
colnames(df) <- "Hour"
hist(df$Period,breaks=6,xlim=c(1,6))

df$Period <- df$Hour
df$Period[df$Hour >= 1 & df$Hour <= 8] <- "1"
df$Period[df$Hour >= 1 & df$Hour <= 8] <- "1"
df$Period[df$Hour >= 9 & df$Hour <= 16] <- "2"
df$Period[df$Hour >= 17  & df$Hour <= 24] <- "3"
plot(table(df$Period))
hist(df$Period,breaks=6,xlim=c(1,3))

df$Period <- df$Hour
df$Period[(df$Hour >= 0 & df$Hour <= 4) | df$Hour >= 22 & df$Hour <= 24] <- "0"
df$Period[df$Hour >= 5 & df$Hour <= 8] <- "1"
df$Period[df$Hour >= 9 & df$Hour <= 12] <- "2"
df$Period[df$Hour >= 13  & df$Hour <= 16] <- "3"
df$Period[df$Hour >= 17  & df$Hour <= 21] <- "4"


test <- data.frame(table(df$Period))
colnames(test) <- c("period","relative_obs")
test
ggplot(test, aes(x=period,y=relative_obs)) + geom_point()
test$adj <- test$

    getAdjPeriods <- function(obs,baseline){
        obs_period <- obs
        obs_period[(obs >= 0 & obs <= 4) | (obs >= 22 & obs <= 24)] <- "0"
        obs_period[obs >= 5 & obs <= 8] <- "1"
        obs_period[obs >= 9 & obs <= 12] <- "2"
        obs_period[obs >= 13  & obs <= 16] <- "3"
        obs_period[obs >= 17  & obs <= 21] <- "4"

        bl_period <- baseline
        bl_period[(baseline >= 0 & baseline <= 4) | (baseline >= 22 & baseline <= 24)] <- "0"
        bl_period[baseline >= 5 & baseline <= 8] <- "1"
        bl_period[baseline >= 9 & baseline <= 12] <- "2"
        bl_period[baseline >= 13  & baseline <= 16] <- "3"
        bl_period[baseline >= 17  & baseline <= 21] <- "4"

        table(obs_period) * table(bl_period)
    }
table(bf_hours[[5]])
table(bf_hours[[5]])

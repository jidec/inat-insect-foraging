# plot diff curves - can modify to plot expec too
plotActivityDiff <- function(sscs,i){
    source("src/getSpExpecDiff.R")
    source("src/padBinFreqHours.R")
    getSpExpecDiff(padBinFreqHours(sscs$obs[[i]],8,1,1),padBinFreqHours(sscs$bl_obs[[i]],8,1,1),TRUE)
}

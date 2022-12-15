getSpExpecDiff <- function(ssc_freqs, sc_freqs,plot=F,get_expec_instead=F){

    prop <- sum(ssc_freqs) / sum(sc_freqs)
    expected <- sc_freqs * prop
    diff <- ssc_freqs - expected

    yl <- max(c(max(expected),max(ssc_freqs)))

    if(plot){
        plot(ssc_freqs,ylim=c(0,yl),main="species")
        plot(expected,ylim=c(0,yl),main="expected")
        plot(diff,ylim=c(-1 * yl,yl),main="diff")
    }

    if(get_expec_instead){
        return(expected)
    }

    return(diff)
}

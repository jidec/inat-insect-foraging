
#df = usa_bfs
daymet_data$
# add temp precip etc data to observations using the year, yday, and daymet_tile cols
addDaymetCellData <- function(df)
{
    # every row in usa_ants needs to get a value containing matching
    library(dplyr)
    library(tidyr)
    library(parallel)
    library(data.table)

    # convert daymet to data table for 30x faster filtering
    temp <- as.data.table(daymet_data,keep.rownames = TRUE)
    t <- 0.002 * nrow(df)
    print(paste("estimated max time:", t, "seconds"))

    #row = df[1,]
    # function to filter over rows for apply
    batchFilter <- function(row)
    {
        if(!is.null(df[1,56])){
            vals <- temp[year == as.numeric(row[33]) & yday == as.numeric(row[56]) & tile == as.numeric(row[57])]
        }
        if(is.null(df[1,56])){
            vals <- temp[year == as.numeric(row[29]) & yday == as.numeric(row[30]) & tile == as.numeric(row[31])]
        }

        if(nrow(vals) > 0){
        row$daylength <- vals$value[1]
        row$precip <- vals$value[2]
        row$solar_rad <- vals$value[3]
        row$temp_max <- vals$value[5]
        row$temp_min <- vals$value[6]
        row$vapor_pressure <- vals$value[7]
        }
        else{
            row$daylength <- -1
            row$precip <- -1
            row$solar_rad <- -1
            row$temp_max <- -1
            row$temp_min <- -1
            row$vapor_pressure <- -1
        }

        return(row)
    }

    out <- apply(df,MARGIN = 1, FUN = batchFilter)
    out <- as.data.frame(do.call(rbind, out))

    return(out)
}

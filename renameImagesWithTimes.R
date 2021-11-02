
#rename images as foraging times extracted from image ids
image_ids <- list.files("images")
image_ids <- strsplit(image_ids," ")
image_ids <- t(as.data.frame(image_ids))
image_ids <- as.vector(image_ids[,1])

library(dplyr)
#make sure this field is "character"
usa_ants$time_observed_at <- as.character(usa_ants$time_observed_at)
#new but very slow loop way, only need to do it once though
matching_times <- character()
for(s in image_ids)
{
    rowmatch <- usa_ants %>% filter(id == s)

    if(nrow(rowmatch) == 0)
    {
        matching_times <- c(matching_times, "NO-TIME-MATCH")
    }
    else
    {
        #time is EST hour currently
        matching_times<- c(matching_times, rowmatch$est_hour)
    }
}
View(matching_times)

#previous vectorized way but doesnt work because gaps
matching_obs <- usa_ants[usa_ants$id %in% image_ids,]
matching_times <- matching_obs$time_observed_at

#rename
file.rename(from=list.files("images", full.names = TRUE), to=paste0("images/",image_ids, "_", matching_times,"-EST.jpg"))
#start with notebooks on repo

#flowering, fruiting, leaves out (vegetative?) - way to get leaves out labels
#NN for fruiting should be done

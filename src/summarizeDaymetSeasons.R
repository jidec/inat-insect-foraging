
summarizeDaymetSeasons <- function(daymet_data)
{
    daymet_seasons <- daymet_data %>%
        group_by(tile,season) %>%
        summarise(mean_daylength = mean(daylength),
                  mean_precip = mean(precip),
                  mean_srad = mean(srad),
                  mean_swe = mean(swe),
                  mean_tmin = mean(tmin),
                  mean_vp = mean(vp))
}

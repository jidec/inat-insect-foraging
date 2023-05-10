library(tidyverse)
library(rvest)

# written by Vaughn Shirey
##### ButterfliesAndMoths.org Scraper #####
# Base URL
base_url <- "https://www.butterfliesandmoths.org/species/"

# Valid colors
valid_cols <- c("black", "gray", "white",
                "brown", "red", "orange",
                "yellow", "green", "blue",
                "pink", "purple")

# Valid features
valid_feat <- c("eyespot", "stripe", "checker",
                "tail", "band", "spot")
# Valid features
valid_activities <- c("morning","noon","afternoon","evening","diurnal","crepuscular","day","early","late","dawn","dusk","sunrise","sunset")

# Loop over species and put results into a data frame
my_species <- data.table::fread("../data/speciesList.csv") %>%
  dplyr::filter(!is.na(species)) %>%
  dplyr::mutate(species_lookup = gsub(" ", "-", species))

my_species <- data.frame(matrix(nrow=239))
my_species$species_lookup <- gsub(" ","-",unique(sscs$species))
my_species$species <- unique(sscs$species)

pb <- txtProgressBar(min=0, max=nrow(my_species),
                     style=3, width=50, char=".")

color_list <- list()
feature_list <- list()
activity_list <- list()
for(i in 1:length(my_species$species_lookup)){
  out <- tryCatch({
    webpage <- rvest::read_html(paste0(base_url, my_species$species_lookup[i]))
  }, error=function(e){NA})

  if(is.na(out)){
    print(paste0("Species ", my_species$species_lookup[i], " not found."))
  } else{
    id_class <- rvest::html_elements(out,
                                     '.views-field-field-identification , .views-label-field-identification')[[1]]
    id_text <- rvest::html_text(id_class) %>%
      stringr::str_to_lower()

    lh_class <- rvest::html_elements(out,
                                     '.views-field-field-lifehistory , .views-label-field-lifehistory')[[1]]
    lh_text <- rvest::html_text(lh_class) %>%
        stringr::str_to_lower()

    color_list[[i]] <- c(species=my_species$species[i],
                         sapply(valid_cols, function(x) str_detect(id_text, fixed(x))))
    feature_list[[i]] <- c(species=my_species$species[i],
                           sapply(valid_feat, function(x) str_detect(id_text, fixed(x))))
    activity_list[[i]] <- c(species=my_species$species[i],
                         sapply(valid_activities, function(x) str_detect(lh_text, fixed(x))))
  }

  Sys.sleep(0.5)
  setTxtProgressBar(pb, i)

}
close(pb)

# Create the final color data frame
color_df <- do.call(rbind, color_list) %>% as.data.frame() %>%
  dplyr::select(scientificName=species, black, gray, white, brown, red,
                orange, yellow, green, blue, pink, purple)
color_df[,2:12] <- lapply(color_df[,2:12], function(x) as.integer(x))
color_df <- color_df %>%
  dplyr::mutate(colorDiversity=rowSums(across(2:12)))

unique_colors <- apply(color_df[,2:12], 1, paste, collapse="")
unique_color_table <- table(unique_colors) %>% sort(decreasing=TRUE) %>%
  as.data.frame()

color_df <- color_df %>%
  dplyr::mutate(unique_colors=paste0(black, gray, white, brown, red, orange,
                                    yellow, green, blue, pink, purple)) %>%
  left_join(unique_color_table) %>%
  dplyr::select(-c(14)) %>%
  dplyr::mutate(uniqueColorIndex=scale(Freq)) %>%
  dplyr::select(-Freq)

write.csv(color_df, "butterflyColors_allNA.csv")

# Create the final feature data frame
feature_df <- do.call(rbind, feature_list) %>% as.data.frame() %>%
  dplyr::select(scientificName=species, eyespot, stripe, checker,
                tail, band, spot)
feature_df[,2:7] <- lapply(feature_df[,2:7], function(x) as.integer(x))
feature_df <- feature_df %>%
  dplyr::mutate(featureDiversity=rowSums(across(2:7)))

write.csv(feature_df, "butterflyFeatures_allNA.csv")

# Create the final feature data frame
feature_df <- do.call(rbind, activity_list) %>% as.data.frame() %>%
    dplyr::select(scientificName=species,morning,noon,afternoon,evening,diurnal,crepuscular,day,early,late,dawn,dusk,sunrise,sunset)

table(feature_df$morning)
table(feature_df$noon)
table(feature_df$afternoon)

table(feature_df$evening)
table(feature_df$diurnal)
table(feature_df$crepuscular)
table(feature_df$day)
table(feature_df$early)
table(feature_df$late)
table(feature_df$dawn)
table(feature_df$dusk)
table(feature_df$sunrise)
table(feature_df$sunset)

feature_df[,2:7] <- lapply(feature_df[,2:7], function(x) as.integer(x))
feature_df <- feature_df %>%
    dplyr::mutate(featureDiversity=rowSums(across(2:7)))

write.csv(feature_df, "butterflyFeatures_allNA.csv")





##### BugGuide.net Scraper #####
# Base URL
base_url <- "https://www.bugguide.net"

# Valid colors
valid_cols <- c("black", "gray", "white",
                "brown", "red", "orange",
                "yellow", "green", "blue",
                "pink", "purple")

# Valid features
valid_feat <- c("eyespot", "stripe", "checker",
                "tail", "band", "spot")

# Loop over species and put results into a data frame
my_species <- data.table::fread("../data/speciesList.csv") %>%
  dplyr::filter(!is.na(species)) %>%
  dplyr::mutate(species_lookup = gsub(" ", "-", species))

pb <- txtProgressBar(min=0, max=nrow(my_species),
                     style=3, width=50, char=".")

color_list <- list()
feature_list <- list()
for(i in 1:length(my_species$species_lookup)){
  out <- tryCatch({
    webpage <- rvest::read_html(paste0(base_url, my_species$species_lookup[i]))
  }, error=function(e){NA})

  if(is.na(out)){
    print(paste0("Species ", my_species$species_lookup[i], " not found."))
  } else{
    id_class <- rvest::html_elements(out,
                                     '.views-field-field-identification , .views-label-field-identification')[[1]]
    id_text <- rvest::html_text(id_class) %>%
      stringr::str_to_lower()

    color_list[[i]] <- c(species=my_species[i]$species,
                         sapply(valid_cols, function(x) str_detect(id_text, fixed(x))))
    feature_list[[i]] <- c(species=my_species[i]$species,
                           sapply(valid_feat, function(x) str_detect(id_text, fixed(x))))
  }

  Sys.sleep(0.5)
  setTxtProgressBar(pb, i)

}
close(pb)

# Create the final color data frame
color_df <- do.call(rbind, color_list) %>% as.data.frame() %>%
  dplyr::select(scientificName=species, black, gray, white, brown, red,
                orange, yellow, green, blue, pink, purple)
color_df[,2:12] <- lapply(color_df[,2:12], function(x) ifelse(x==FALSE, 0, 1))
color_df <- color_df %>%
  dplyr::mutate(colorDiversity=rowSums(across(2:12)))

unique_colors <- apply(color_df[,2:12], 1, paste, collapse="")
unique_color_table <- table(unique_colors) %>% sort(decreasing=TRUE) %>%
  as.data.frame()

color_df <- color_df %>%
  dplyr::mutate(unique_colors=paste0(black, gray, white, brown, red, orange,
                                     yellow, green, blue, pink, purple)) %>%
  left_join(unique_color_table) %>%
  dplyr::select(-c(14)) %>%
  dplyr::mutate(uniqueColorIndex=scale(Freq)) %>%
  dplyr::select(-Freq) %>%
  dplyr::filter(colorDiversity > 0)
write.csv(color_df, "butterflyColors_allNA.csv")

# Create the final feature data frame
feature_df <- do.call(rbind, feature_list) %>% as.data.frame() %>%
  dplyr::select(scientificName=species, eyespot, stripe, checker,
                tail, band, spot)
feature_df[,2:7] <- lapply(feature_df[,2:7], function(x) ifelse(x==FALSE, 0, 1))
feature_df <- feature_df %>%
  dplyr::mutate(featureDiversity=rowSums(across(2:7)))
write.csv(feature_df, "butterflyFeatures_allNA.csv")

View(feature_df)


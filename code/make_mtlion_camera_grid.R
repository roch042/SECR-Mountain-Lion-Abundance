# Libraries ####
require(tidyverse)

# Read In Data ####

# picdat <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_picdat.csv")
picdat <- data.table::fread("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_picdat.csv", colClasses = 'character',stringsAsFactors = FALSE, data.table = FALSE)

pic_grid <- picdat %>%
  dplyr::group_by(Project, CamID, Lat, Long) %>%
  dplyr::summarise(first_pic = min(Date, na.rm=T),
                   last_pic = max(Date, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Lat = as.numeric(Lat),
                Long = as.numeric(Long))

saveRDS(pic_grid,"mtlion_camera_grid.RDS")

# https://epsg.io/8826#:~:text=NAD83%20%2F%20Idaho%20Transverse%20Mercator%20%2D%20EPSG%3A8826

require(tidyverse)
require(sp)
require(adehabitatHR)
library(slickR)

# Load Data ####
load("spatial_objects.RData") # spatial objects for leaflet maps
readRDS("mtlion_pics.RDS") # mountain lion pictures
DAT <- readr::read_csv("data-2020-12-09.csv") # mountain lion collar data
deploydat <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_deploydat.csv") # camera grid for mountain lions

# GPS locations ####

win_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2019-12-01" & lubridate::date(`Location Date (GMT)`) <= "2020-03-31") %>%
  dplyr::filter(!is.na(`Animal ID`))

sum_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2020-05-01" & lubridate::date(`Location Date (GMT)`) <= "2020-12-31") %>%
  dplyr::filter(!is.na(`Animal ID`))


# Pictures of marked animals ####

mtlion_marked_pics <- picdat[which(picdat$Species == "mountain_lion" & picdat$MarkedAnimal == TRUE),]

picpath <- "T:/Wildlife/WildlifeResearch/Cameras_temp_photo_share/ALL_mountain_lions_R5WINTERSET"

pics <- mtlion_marked_pics[,"File"]

obj_filepath <- purrr::map(pics, function(x){
  picpath <- "T:/Wildlife/WildlifeResearch/Cameras_temp_photo_share/ALL_mountain_lions_R5WINTERSET"
  return(file.path(picpath, x))
})

slickR::slickR(obj = obj_filepath$File, height = "1000px") +
  slickR::settings(dots = T,
                   centerMode = T,
                   centerPadding = '20px',
                   slidesToShow = 1,
                   focusOnSelect = T)

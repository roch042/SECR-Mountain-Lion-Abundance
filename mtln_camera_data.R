# Load mtnlion data####

# data.table::fread(files, colClasses = 'character',stringsAsFactors = FALSE, data.table = FALSE)
# pic_dat <- do.call("bind_rows", lapply(Xdir, FUN=function(files){data.table::fread(files, colClasses = 'character',stringsAsFactors = FALSE, data.table = FALSE)}))

# Libraries ####

library(slickR)
library(tidyverse)


# Read In Data ####

picdat <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_picdat.csv")
deploydat <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_deploydat.csv")
piclocations <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_piclocations.csv")

mtlion_pics <- picdat[which(picdat$Species == "mountain_lion"),]
mtlion_marked_pics <- picdat[which(picdat$Species == "mountain_lion" & picdat$MarkedAnimal == TRUE),]


# Summaries ####

no_pics <- nrow(mtlion_pics)
no_cameras <- length(unique(mtlion_pics$CamID))
no_dates <- length(unique(lubridate::date(mtlion_pics$Date)))
no_months <- length(unique(lubridate::month(mtlion_pics$Date)))

mean_pics_day <- round(no_pics/no_dates, digits=1)

hist(lubridate::hour(mtlion_pics$posix_date_time))
hist(lubridate::month(mtlion_pics$posix_date_time))

# Slideshow ####

picpath <- "T:/Wildlife/WildlifeResearch/Cameras_temp_photo_share/ALL_mountain_lions_R5WINTERSET"

# pics <- mtlion_marked_pics[,"File"]
pics <- mtlion_pics[,"File"]

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

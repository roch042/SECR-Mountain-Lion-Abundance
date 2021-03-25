# Libraries ####

require(slickR)
require(tidyverse)
require(sp)
require(sf)
require(leaflet)
require(RColorBrewer)
require(viridis)

# Read In Data ####

# _picture data ####
# picdat <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_picdat.csv")
picdat <- data.table::fread("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_picdat.csv", colClasses = 'character',stringsAsFactors = FALSE, data.table = FALSE)
deploydat <- readr::read_csv("F:/Images/IMAGESETS/CAMLN2020/FinalData/camln2019_deploydat.csv")

# _spatial data ####
load("data/spatial_objects.RData")

# _gmus/regions ####
# con <- DBI::dbConnect(
#   odbc::odbc(),
#   driver = "SQL Server",
#   database = "IFWIS_WildlifeReporting",
#   uid = "ShinyUserInternal",
#   pwd = "hurt seven sat pupil",
#   server = "164.165.105.241",
#   port = "1433")
# EPMU <- DBI::dbGetQuery(con, paste("SELECT * FROM ","Harvest_MHR_UnitsByDAU",sep="")) %>%
#   dplyr::filter(Unit != "UNK")
load("data/EPMU.RData")

# _collar data ####
# mtlion collar data - 12/01/2019 to 12/01/2020

DAT <- readr::read_csv("data/data-2020-12-09.csv")

mtlion_index <- DAT %>%
  dplyr::select("Animal ID","Sex","Age Class (Capture)","Capture Date") %>%
  dplyr::distinct() %>%
  tidyr::unite(label, `Animal ID`, Sex, `Age Class (Capture)`,sep=", ",remove=FALSE) %>%
  dplyr::mutate(`Animal ID` = as.character(`Animal ID`))

# Filter Picture Data ####

mtlion_pics <- picdat[which(picdat$Species == "mountain_lion"),] %>%
  dplyr::mutate(Hour = lubridate::hour(posix_date_time),
                Min = lubridate::minute(posix_date_time))

mtlion_marked_pics <- mtlion_pics[which(mtlion_pics$MarkedAnimal == TRUE),]

pic_grid <- picdat %>%
  dplyr::group_by(Project, CamID, Lat, Long) %>%
  dplyr::summarise(first_pic = min(Date, na.rm=T),
                   last_pic = max(Date, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Lat = as.numeric(Lat),
                Long = as.numeric(Long))

nopics <- nrow(picdat)

# sf spatial dataframes ####

# _camera grid ####
sp_pic_grid <- pic_grid
coordinates(sp_pic_grid)<- c("Long", "Lat")
proj4string(sp_pic_grid) <- CRS("+init=epsg:4326")
sf_pic_grid <- sf::st_as_sf(sp_pic_grid) # sf object
# sp_pic_grid_idaho <- spTransform(sp_pic_grid, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
# sf_pic_grid_idaho <- sf::st_as_sf(sp_pic_grid_idaho) # sf object

# _ mtlion seen cameras
sp_mtlion_pic_grid <- pic_grid %>%
  dplyr::inner_join(dplyr::distinct(mtlion_pics[,c("Project","CamID","Date","Hour","Min")]), by = c("Project"="Project","CamID"="CamID")) %>%
  dplyr::mutate(Min = str_pad(Min, 2, pad = "0")) %>%
  tidyr::unite("Time", Hour, Min, sep = ":",remove=TRUE)
coordinates(sp_mtlion_pic_grid)<- c("Long", "Lat")
proj4string(sp_mtlion_pic_grid) <- CRS("+init=epsg:4326")
sf_mtlion_pic_grid <- sf::st_as_sf(sp_mtlion_pic_grid) # sf object
# sp_mtlion_pic_grid_idaho <- spTransform(sp_mtlion_pic_grid, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
# sf_mtlion_pic_grid_idaho <- sf::st_as_sf(sp_mtlion_pic_grid_idaho) # sf object

# _ mtlion seen cameras
sp_mtlion_marked_pic_grid <- pic_grid %>%
  dplyr::inner_join(dplyr::distinct(mtlion_marked_pics[,c("Project","CamID","Date","Hour","Min")]), by = c("Project"="Project","CamID"="CamID")) %>%
  dplyr::mutate(Min = str_pad(Min, 2, pad = "0")) %>%
  tidyr::unite("Time", Hour, Min, sep = ":",remove=TRUE)
coordinates(sp_mtlion_marked_pic_grid)<- c("Long", "Lat")
proj4string(sp_mtlion_marked_pic_grid) <- CRS("+init=epsg:4326")
sf_mtlion_marked_pic_grid <- sf::st_as_sf(sp_mtlion_marked_pic_grid) # sf object
# sp_mtlion_marked_pic_grid_idaho <- spTransform(sp_mtlion_marked_pic_grid, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
# sf_mtlion_marked_pic_grid_idaho <- sf::st_as_sf(sp_mtlion_marked_pic_grid_idaho) # sf object

# _state ####
proj4string(geo_state) <- CRS("+init=epsg:4326")
sf_geo_state <- sf::st_as_sf(geo_state) # sf object
# geo_state_idaho <- spTransform(geo_state, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
# sf_geo_state_idaho <- sf::st_as_sf(geo_state_idaho) # sf object

# _gmu ####
proj4string(ID_gmu) <- CRS("+init=epsg:4326")
sf_ID_gmu <- sf::st_as_sf(ID_gmu) # sf object
# ID_gmu_idaho <- spTransform(ID_gmu, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
# sf_ID_gmu_idaho <- sf::st_as_sf(ID_gmu_idaho) # sf object

# save data for map function

to_save <- c("DAT","mtlion_index","sf_ID_gmu","sf_mtlion_pic_grid","sf_mtlion_marked_pic_grid","sf_pic_grid","nopics","mtlion_pics")
save(list=to_save,file="data/mtn_camera_data.RData")

# leaflet maps ####

cats <- c("200019","200020","200027","200020","201382","200023","200024","200026","200004","200016","200029")

# _input ####
source("code/fnc_camera_collar_map.R")

map <- fnc_camera_collar_map(
  start_date = "2019-12-01",
  end_date = "2020-04-20",
  spec_animal = c("201382","200027"),
  homerange = FALSE,
  collarloc = TRUE,
  min_circle_radius = 1,
  max_circle_radius = 5,
  DAT = DAT,
  mtlion_index = mtlion_index,
  mtlion_pics = mtlion_pics,
  sf_ID_gmu = sf_ID_gmu,
  sf_mtlion_pic_grid = sf_mtlion_pic_grid,
  sf_mtlion_marked_pic_grid = sf_mtlion_marked_pic_grid,
  sf_pic_grid = sf_pic_grid
)


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

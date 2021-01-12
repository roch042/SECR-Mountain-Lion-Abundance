# Libraries ####

require(slickR)
require(tidyverse)
require(sp)
require(sf)

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

# Filter Picture Data ####

mtlion_pics <- picdat[which(picdat$Species == "mountain_lion"),]

mtlion_marked_pics <- mtlion_pics[which(mtlion_pics$MarkedAnimal == TRUE),]

pic_grid <- picdat %>%
  dplyr::group_by(Project, CamID, Lat, Long) %>%
  dplyr::summarise(first_pic = min(Date, na.rm=T),
                   last_pic = max(Date, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Lat = as.numeric(Lat),
                Long = as.numeric(Long))

 
# sf spatial dataframes ####

# _camera grid ####
sp_pic_grid <- pic_grid
coordinates(sp_pic_grid)<- c("Long", "Lat")
proj4string(sp_pic_grid) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sp_pic_grid_idaho <- spTransform(sp_pic_grid, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_pic_grid_idaho <- sf::st_as_sf(sp_pic_grid_idaho) # sf object

# _ mtlion seen cameras
sp_mtlion_pic_grid <- pic_grid %>%
  dplyr::inner_join(dplyr::distinct(mtlion_pics[,c("Project","CamID")]), by = c("Project"="Project","CamID"="CamID"))
coordinates(sp_mtlion_pic_grid)<- c("Long", "Lat")
proj4string(sp_mtlion_pic_grid) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sp_mtlion_pic_grid_idaho <- spTransform(sp_mtlion_pic_grid, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_mtlion_pic_grid_idaho <- sf::st_as_sf(sp_mtlion_pic_grid_idaho) # sf object

# _ mtlion seen cameras
sp_mtlion_marked_pic_grid <- pic_grid %>%
  dplyr::inner_join(dplyr::distinct(mtlion_marked_pics[,c("Project","CamID")]), by = c("Project"="Project","CamID"="CamID"))
coordinates(sp_mtlion_marked_pic_grid)<- c("Long", "Lat")
proj4string(sp_mtlion_marked_pic_grid) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sp_mtlion_marked_pic_grid_idaho <- spTransform(sp_mtlion_marked_pic_grid, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_mtlion_marked_pic_grid_idaho <- sf::st_as_sf(sp_mtlion_marked_pic_grid_idaho) # sf object

# _state ####
proj4string(geo_state) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
geo_state_idaho <- spTransform(geo_state, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_geo_state_idaho <- sf::st_as_sf(geo_state_idaho) # sf object

# _gmu ####
proj4string(ID_gmu) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
ID_gmu_idaho <- spTransform(ID_gmu, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_ID_gmu_idaho <- sf::st_as_sf(ID_gmu_idaho) # sf object

# _collar data - points ####

start_date <- "2019-12-01"
end_date <- "2020-04-20"

flt_collar <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= start_date & lubridate::date(`Location Date (GMT)`) <= end_date) %>%
  dplyr::filter(!is.na(`Animal ID`))

flt_collar_sp <- flt_collar
flt_collar_sp <- flt_collar_sp[,c("Animal ID",  "Longitude (Location)", "Latitude (Location)")]
names(flt_collar_sp)<-c("id", "x", "y")
coordinates(flt_collar_sp)<- c("x", "y")
proj4string(flt_collar_sp)<- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
flt_collar_sp_idaho <- spTransform(flt_collar_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
flt_collar_sf_idaho <- sf::st_as_sf(flt_collar_sp_idaho)

# _collar data - kde ####

kernel_ref <- adehabitatHR::kernelUD(flt_collar_sp_idaho, h = "href")  # href = the reference bandwidth
kernel_poly <- adehabitatHR::getverticeshr(kernel_ref, percent = 95, unout = "km2")
sf_kernel_poly <- sf::st_as_sf(kernel_poly)


# maps ####

flt_gmu <- sf_ID_gmu_idaho %>%
  dplyr::filter(NAME %in% c("74","77","75","78"))

ggplot(flt_gmu) +
  geom_sf(color = "dark gray") + 
  geom_sf_text(aes(label = NAME), size = 3) +
  geom_sf(data=sf_pic_grid_idaho, color = "gray", fill = NA, size = 1) +
  geom_sf(data=sf_mtlion_pic_grid_idaho, color = "royalblue2", fill = NA, size = 1.5) +
  geom_sf(data=sf_mtlion_marked_pic_grid_idaho, color = "navy", fill = NA, size = 2) +
  xlab("") +
  ylab("") #+
  # labs(title = paste0(spec_age," ",spec_sex," ",spec_animal," captured on ",spec_capture),
  #      subtitle = paste0("winter = ",round(win_kernel$area, digits=0)," km2, summer = ",round(sum_kernel$area, digits=0)," km2"),
  #      caption = caption_info)


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

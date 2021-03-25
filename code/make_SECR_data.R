require(tidyverse)
require(sp)
require(sf)

mtlion_pics <- readRDS("data/mtlion_pics.RDS")
camera_deploy <- readRDS("data/mtlion_camera_deploy.RDS")
load("data/mountain_lion_kde.RData")
trapgrid_shapefile <- rgdal::readOGR(dsn = "data/shapefiles" , layer = "MD_WinterRange_LionSampling_Union", verbose=FALSE)
DAT <- readr::read_csv("data/data-2020-12-09.csv")
cats_excluded <- c(200003, 200018, 200028, 200002, 200013, 200014, 200034, 200012, 200008, 18108, 19119)
nTelemLocs <- 100
start_date <- "2020-01-01"
end_date <- "2020-04-01"

# _M ####
M <- 500

# start_resight_interval <- lubridate::date(min(camera_deploy$first_pic, na.rm=T))
# end_resight_interval <- lubridate::date(max(camera_deploy$last_pic, na.rm=T))
start_resight_interval <- lubridate::date(start_date)
end_resight_interval <- lubridate::date(end_date)
interval_length <- 7 #days

interval_vec <- seq(start_resight_interval, end_resight_interval, by = interval_length)
start_resight_interval_vec <- interval_vec[-length(interval_vec)]
end_resight_interval_vec <- interval_vec[-1] - 1
resight_interval_name <- 1:length(start_resight_interval_vec)

# RESIGHT CAMERA DATA ####

camera_deploy <- camera_deploy %>%
  dplyr::arrange(CamID)

sp_camera_deploy <- camera_deploy
sp::coordinates(sp_camera_deploy)<- c("Long", "Lat")
sp::proj4string(sp_camera_deploy) <- sp::CRS("+init=epsg:4326")
sp_camera_deploy_idaho <- sp::spTransform(sp_camera_deploy, sp::CRS("+init=epsg:8826"))
sf_camera_deploy <- sf::st_as_sf(sp_camera_deploy_idaho) # sf object

# __X.cam ####
X.cam <- sf::st_coordinates(sf_camera_deploy$geometry)
rownames(X.cam) <- sf_camera_deploy$CamID

# _Unmarked Counts Pictures ####

# collapse max group size by 10 min interval #
mtlion_pics10 <- mtlion_pics %>%
  dplyr::filter(MarkedAnimal == FALSE) %>%
  dplyr::filter(lubridate::date(posix_date_time) >= min(start_resight_interval_vec) & lubridate::date(posix_date_time) <= max(end_resight_interval_vec)) %>%
  tidyr::separate(col = posix_date_time, into = c("cDate","cTime","cZone"), sep = " ", remove = FALSE) %>%
  dplyr::mutate(TimeGroup = substr(cTime, 1, 4)) %>%
  dplyr::group_by(CamID, Date, TimeGroup) %>%
  dplyr::summarise(Count = max(Count, na.rm=T)) # maximum group size within that interval

# __J.resight ####
J.resight <- nrow(camera_deploy) # number of cameras in the grid

camid_yes_mtlion <- camera_deploy$CamID[camera_deploy$CamID %in% unique(mtlion_pics10$CamID)]
camid_no_mtlion <- camera_deploy$CamID[!(camera_deploy$CamID %in% camid_yes_mtlion)] # matrix of all zeros

# assign to resighting interval
for (i in 1:nrow(mtlion_pics10)){
  mtlion_pics10$resight_interval[i] <- resight_interval_name[which(mtlion_pics10$Date[i] <= end_resight_interval_vec & mtlion_pics10$Date[i] >= start_resight_interval_vec)]
}

# __K.resight ####
K.resight <- max(resight_interval_name)

camid_counts_interval <- mtlion_pics10 %>%
  dplyr::mutate(resight_interval = as.character(resight_interval)) %>%
  dplyr::mutate(resight_interval = factor(resight_interval, levels=as.character(resight_interval_name))) %>%
  dplyr::group_by(CamID, resight_interval, .drop=FALSE) %>%
  dplyr::summarize(n = sum(Count))

resight_grid_yes_mtlion <- tidyr::pivot_wider(camid_counts_interval, names_from = resight_interval, values_from = n)

resight_grid_no_mtlion <- matrix(nrow = length(camid_no_mtlion),
                                 ncol = ncol(resight_grid_yes_mtlion), 
                                 data = 0)
colnames(resight_grid_no_mtlion) <- c("CamID",1:K.resight)
resight_grid_no_mtlion <- tidyr::as_tibble(resight_grid_no_mtlion)
resight_grid_no_mtlion[,1] <- camid_no_mtlion

resight_unmarked <- dplyr::bind_rows(resight_grid_yes_mtlion, resight_grid_no_mtlion) %>%
  dplyr::arrange(CamID)

# __ n - matrix of unmarked animals ####
n <- resight_unmarked %>%
  dplyr::ungroup() %>%
  dplyr::select(-CamID) %>%
  as.matrix()
rownames(n) <- resight_unmarked$CamID

# _Marked Animal Pictures ####

# best guess using gps data, can't see number on collar
# 201382 - IDFG2886 2020-01-13 18:0 
# 200023 - IDFG2912 2020-02-22 07:4
# 200023 - IDFG2912 2020-03-16 03:0

# code only works if one animal per pictures marked

marked_mtlion_pics10 <- mtlion_pics %>%
  dplyr::filter(MarkedAnimal == TRUE) %>%
  dplyr::filter(lubridate::date(posix_date_time) >= min(start_resight_interval_vec) & lubridate::date(posix_date_time) <= max(end_resight_interval_vec)) %>%
  tidyr::separate(col = posix_date_time, into = c("cDate","cTime","cZone"), sep = " ", remove = FALSE) %>%
  dplyr::mutate(TimeGroup = substr(cTime, 1, 4)) %>%
  dplyr::select(CamID, Date, TimeGroup) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Count = 1) %>% # seen or not seen
  dplyr::mutate(
    AnimalID = dplyr::case_when(
      CamID == "IDFG2886" & Date == "2020-01-13" & TimeGroup == "18:0" ~ "201382",
      CamID == "IDFG2912" & Date == "2020-02-22" & TimeGroup == "07:4" ~ "200023",
      CamID == "IDFG2912" & Date == "2020-03-16" & TimeGroup == "03:0" ~ "200023"
    ))

# assign to resighting interval
for (i in 1:nrow(marked_mtlion_pics10)){
  marked_mtlion_pics10$resight_interval[i] <- resight_interval_name[which(marked_mtlion_pics10$Date[i] <= end_resight_interval_vec & marked_mtlion_pics10$Date[i] >= start_resight_interval_vec)]
}

marked_mtlion_captures <- mtlion_index %>%
  dplyr::filter(!(`Animal ID` %in% cats_excluded)) %>%
  dplyr::select(`Animal ID`,`Capture Date`) %>%
  dplyr::filter(!is.na(`Animal ID`)) %>%
  dplyr::arrange(`Animal ID`)

AnimalID_vec <- marked_mtlion_captures$`Animal ID`

# __nMarked ####
nMarked <- length(AnimalID_vec)

# __y.resight ####
y.resight <- array(0, c(M, J.resight, K.resight))
resight_marked <- list()

for(i in 1:nMarked){
  
  if(AnimalID_vec[i] %in% marked_mtlion_pics10$AnimalID){
    
    interval <- marked_mtlion_pics10 %>%
      dplyr::filter(AnimalID == AnimalID_vec[i]) %>%
      dplyr::mutate(resight_interval = as.character(resight_interval)) %>%
      dplyr::mutate(resight_interval = factor(resight_interval, levels=as.character(resight_interval_name))) %>%
      dplyr::group_by(CamID, resight_interval, .drop=FALSE) %>%
      dplyr::summarize(n = sum(Count))
    
    camid_yes_mark <- camera_deploy$CamID[camera_deploy$CamID %in% unique(interval$CamID)]
    camid_no_mark <- camera_deploy$CamID[!(camera_deploy$CamID %in% camid_yes_mark)] # matrix of all zeros
    
    yes_mtlion <- tidyr::pivot_wider(interval, names_from = resight_interval, values_from = n)

    no_mtlion <- matrix(nrow = length(camid_no_mark),
                                     ncol = ncol(yes_mtlion),
                                     data = 0)
    colnames(no_mtlion) <- c("CamID",1:K.resight)
    no_mtlion <- tidyr::as_tibble(no_mtlion)
    no_mtlion[,1] <- camid_no_mark

    marked_mtlion <- dplyr::bind_rows(yes_mtlion, no_mtlion) %>%
      dplyr::arrange(CamID)
    
    marked_mtlion_matrix <- marked_mtlion %>%
      dplyr::ungroup() %>%
      dplyr::select(-CamID) %>%
      as.matrix()
    rownames(marked_mtlion_matrix) <- marked_mtlion$CamID
    
    resight_marked[[i]] <- marked_mtlion
    
    y.resight[i,,] <- marked_mtlion_matrix
    
  } else {
    
    resight_marked[[i]] <- NA
    
  }
  
}
names(resight_marked) <- AnimalID_vec
dimnames(y.resight) <- list(c(AnimalID_vec,1:(M-length(AnimalID_vec))), rownames(marked_mtlion_matrix), colnames(marked_mtlion_matrix))

# MARKED ANIMALS ####

# assign to resighting interval
for (i in 1:nrow(marked_mtlion_captures)){
  marked_mtlion_captures$resight_interval_captured[i] <- ifelse(marked_mtlion_captures$`Capture Date`[i] < min(start_resight_interval_vec) , 1, resight_interval_name[which(marked_mtlion_captures$`Capture Date`[i] <= end_resight_interval_vec & marked_mtlion_captures$`Capture Date`[i] >= start_resight_interval_vec)])
}

# create marked.status array
mmc <- marked_mtlion_captures %>%
  dplyr::filter(!is.na(resight_interval_captured)) %>%
  dplyr::select(`Animal ID`, resight_interval_captured) %>%
  dplyr::mutate(resight_interval_captured = factor(resight_interval_captured, levels = 1:K.resight)) %>%
  dplyr::rename(AnimalID = `Animal ID`) %>%
  dplyr::group_by(AnimalID, resight_interval_captured, .drop = FALSE) %>%
  dplyr::summarize(n = n()) %>%
  tidyr::pivot_wider(names_from = resight_interval_captured, values_from = n)

for (i in 1:nrow(mmc)){
  startfill <- which(mmc[i,] == 1)
  endfill <- ncol(mmc)
  total <- length((startfill+1):endfill)
  mmc[i, (startfill+1):endfill] <- as.list(rep(1, total))
}

mmc_rownames <- mmc$AnimalID
mmc <- mmc %>%
  dplyr::ungroup() %>%
  dplyr::select(-AnimalID) %>%
  as.matrix()
rownames(mmc) <- mmc_rownames

imc <- matrix(ncol = K.resight, 
              nrow = M - nMarked,
              data = 0)
colnames(imc) <- 1:K.resight
rownames(imc) <- 1:(M - nMarked)

# __marked.status ####
marked.status <- rbind(mmc, imc)

# Trapping Locations - use camera grid ####

trapgrid_shapefile_idaho <- sp::spTransform(trapgrid_shapefile, sp::CRS("+init=epsg:8826"))
sf_trapgrid_shapefile <- sf::st_as_sf(trapgrid_shapefile_idaho) %>%
  dplyr::mutate(TrapName = as.numeric(PageNumber)) %>%
  dplyr::arrange(TrapName) %>% 
  sf::st_buffer(dist = 0)# sf object
sf_trapgrid_centroid <- sf::st_centroid(sf_trapgrid_shapefile)

# __J.marking ####
J.marking <- length(unique(sf_trapgrid_shapefile$TrapName))

# __X.marking ####
X.marking <- sf::st_coordinates(sf_trapgrid_centroid)

# GPS capture data

marking_locations <- DAT %>%
  dplyr::select(`Animal ID`, `Latitude (Capture)`, `Longitude (Capture)`) %>%
  dplyr::filter(!(`Animal ID` %in% cats_excluded)) %>%
  dplyr::filter(!is.na(`Animal ID`)) %>%
  dplyr::distinct()

sp_marking_locations <- marking_locations
sp::coordinates(sp_marking_locations)<- c("Longitude (Capture)", "Latitude (Capture)")
sp::proj4string(sp_marking_locations) <- sp::CRS("+init=epsg:4326")
sp_marking_locations_idaho <- sp::spTransform(sp_marking_locations, sp::CRS("+init=epsg:8826"))
sf_marking_locations <- sf::st_as_sf(sp_marking_locations_idaho) # sf object

marking_trapgrid_overlap <- sf::st_intersection(sf_marking_locations, sf_trapgrid_shapefile) %>%
  dplyr::mutate(`Animal ID` = as.character(Animal.ID))

marked_mtlion_captures <- marked_mtlion_captures %>%
  dplyr::left_join(marking_trapgrid_overlap[,c("Animal ID","TrapName")]) 

marking_trapname <- marked_mtlion_captures %>%
  dplyr::mutate(TrapName = factor(as.character(TrapName), levels = 1:J.marking)) %>%
  dplyr::select(`Animal ID`, TrapName) %>%
  dplyr::group_by(`Animal ID`, TrapName, .drop=FALSE) %>%
  dplyr::summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = TrapName, values_from = n)

marking_trapname_matrix <- marking_trapname %>%
  dplyr::ungroup() %>%
  dplyr::select(-`Animal ID`) %>%
  as.matrix()
rownames(marking_trapname_matrix) <- marking_trapname$`Animal ID`

extra_trapname_matrix <- matrix(data=0, nrow = (M - nMarked), ncol = J.marking)
colnames(extra_trapname_matrix) <- 1:J.marking
rownames(extra_trapname_matrix) <- 1:(M-nMarked)

# __y.marking ####
y.marking <- rbind(marking_trapname_matrix, extra_trapname_matrix)


# GPS LOCATION DATA ####

cat_locations <- DAT %>%
  dplyr::select(`Animal ID`, `Location Date (LMT)`, `Latitude (Location)`, `Longitude (Location)`) %>%
  dplyr::filter(!(`Animal ID` %in% cats_excluded)) %>%
  dplyr::filter(!is.na(`Animal ID`)) %>%
  dplyr::distinct() %>%
  dplyr::filter(lubridate::date(`Location Date (LMT)`) >= min(start_resight_interval_vec) & lubridate::date(`Location Date (LMT)`) <= max(end_resight_interval_vec))

sp_cat_locations <- cat_locations
sp::coordinates(sp_cat_locations)<- c("Longitude (Location)", "Latitude (Location)")
sp::proj4string(sp_cat_locations) <- sp::CRS("+init=epsg:4326")
sp_cat_locations_idaho <- sp::spTransform(sp_cat_locations, sp::CRS("+init=epsg:8826"))
sf_cat_locations <- sf::st_as_sf(sp_cat_locations_idaho) # sf object

sample_sf_cat_locations <- sf_cat_locations %>%
  dplyr::group_by(`Animal ID`) %>%
  dplyr::sample_n(size = nTelemLocs)

# _telemetry.array ####
telemetry.array <- array(NA, c(nMarked, nTelemLocs, 2))
for (i in 1:nMarked){
  coord <- sample_sf_cat_locations %>%
    dplyr::filter(`Animal ID` == AnimalID_vec[i]) %>%
    sf::st_coordinates()
  telemetry.array[i,,1] <- coord[,"X"]
  telemetry.array[i,,2] <- coord[,"Y"]
}
dimnames(telemetry.array) <- list(AnimalID_vec, c(1:nTelemLocs), c("X","Y"))

# __xlim ####
minx <- min(c(X.marking[,1], X.cam[,1]), na.rm=T) # include telemetry.array data or not?
maxx <- max(c(X.marking[,1], X.cam[,1]), na.rm=T)
xlim <- c(minx, maxx)

# __ylim ####
miny <- min(c(X.marking[,2], X.cam[,2]), na.rm=T) # include telemetry.array data or not?
maxy <- max(c(X.marking[,2], X.cam[,2]), na.rm=T)
ylim <- c(miny, maxy)

secr_data <- c("M","X.cam","J.resight","K.resight","n","nMarked","y.resight","marked.status","J.marking","X.marking","y.marking","telemetry.array","nTelemLocs","xlim","ylim")

info_data <- c("AnimalID_vec","marked_mtlion_captures","sf_camera_deploy", "resight_unmarked","resight_marked","sf_trapgrid_centroid","marking_trapname","sample_sf_cat_locations","marking_trapname","start_resight_interval_vec","end_resight_interval_vec","resight_interval_name")

to_save <- ls()[ls() %in% c(secr_data, info_data)]

# sf_camera_deploy = x.cam, resight_unmarked = n, resight_marked = y.resight, sf_trapgrid_centroid = x.marking,  marking_trapname = y.marking, sample_sf_cat_locations = telemetry.array, marking_trapname = y.marking

save(list = to_save, file = "data/SECR_data.RData")


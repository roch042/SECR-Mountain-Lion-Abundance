require(tidyverse)

load("K:/Wildlife/Shiny_Apps/EAR/BearLakeMountainLions/data/mountain_lion_kde.RData")
load("K:/Wildlife/Shiny_Apps/EAR/BearLakeMountainLions/data/SECR_data.RData")

con1 <- DBI::dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  database = "IFWIS_Wildlife",
  uid = "ShinyUserInternal", # "ShinyUserInternal",
  pwd = "hurt seven sat pupil", # "hurt seven sat pupil",
  server = "164.165.105.241",
  port = "1433")

# sf_camera_deploy = x.cam
# resight_unmarked, mtlion_pics10 = n
# resight_marked, marked_mtlion_pics10 = y.resight
# sf_trapgrid_centroid = x.marking
# marked_mtlion_captures, marking_trapname = y.marking
# sample_sf_cat_locations = telemetry.array
# marked.status = marked_mtlion_captures

# CamID    Date       TimeGroup Count AnimalID resight_interval
# <chr>    <date>     <chr>     <dbl> <chr>               <int>
#   1 IDFG2886 2020-01-13 18:0          1 201382                  2
# 2 IDFG2912 2020-02-22 07:4          1 200023                  8
# 3 IDFG2912 2020-03-16 03:0          1 200023                 11

# 1. Plot the sampled points for each cat ####
# are the gps points reasonable
# - 200027 could be problematic, starts to disperse during timeperiod

flt_gmu <- sf_ID_gmu_idaho %>%
  dplyr::filter(NAME %in% c(73, 74, 75, 76, 77, 78))

AnimalID_vec
# "200004" "200016" "200019" "200020" "200023" "200024" "200026" "200027" "200029" "201382"

tbl <- sample_sf_cat_locations %>%
  dplyr::filter(`Animal ID` == "201382")

(m <- ggplot(flt_gmu) +
  geom_sf(color = "dark gray") + 
  geom_sf_text(aes(label = NAME), size = 3) +
  geom_sf(data=tbl, color = "blue", fill = NA, size = 1.5) +
  xlab("") +
  ylab(""))


# 2. summarized minimum and maximum sampled location data per cat ####
sample_sf_cat_locations %>%
  dplyr::group_by(`Animal ID`) %>%
  dplyr::summarise(min_date = min(`Location Date (LMT)`, na.rm=T),
                   max_date = max(`Location Date (LMT)`, na.rm=T))

# 3. make sure that nimble packaged data is in the same order by plotting on top of original ####
# "200004" "200016" "200019" "200020" "200023" "200024" "200026" "200027" "200029" "201382"

catid <- "201382"

tbl <- sample_sf_cat_locations %>%
  dplyr::filter(`Animal ID` == catid)

cat_index <- which(AnimalID_vec == catid)
tbl_prf <- as.data.frame(telemetry.array[cat_index,,])
names(tbl_prf) <- c("X", "Y")
sp::coordinates(tbl_prf) <- c("X", "Y")
sp::proj4string(tbl_prf) <- sp::CRS("+init=epsg:8826")
tbl_prf <- sf::st_as_sf(tbl_prf) # sf object

(m <- ggplot(flt_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=tbl, color = "blue", fill = NA, size = 1.5) +
    geom_sf(data=tbl_prf, color = "red", fill = NA, size = 1.5) +
    xlab("") +
    ylab(""))

# 4. double-check index of the cameras ####
# sf_camera_deploy = x.cam
camid <- "IDFG2911"
camindex <- which(sf_camera_deploy$CamID == camid)
(sf_camera_deploy[sf_camera_deploy$CamID == camid,])
(X.cam[camindex,])
(m <- ggplot(flt_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=sf_camera_deploy, color = "blue", fill = NA, size = 1.5) +
    xlab("") +
    ylab(""))

# 5. check the correct number of resightings are in the correct camera index (x) ####
camid <- "IDFG2769"
camindex <- which(sf_camera_deploy$CamID == camid)
resight_unmarked[which(resight_unmarked$CamID == camid),]
n[camindex,]
View(mtlion_pics10[which(mtlion_pics10$CamID == camid),])
start_resight_interval_vec
resight_interval_name

# 6. marked.status
catid <- "200019"
catindex <- which(row.names(marked.status) == catid)
marked.status[catindex,]
marked_mtlion_captures[which(marked_mtlion_captures$`Animal ID` == catid),]
start_resight_interval_vec
resight_interval_name

# 7. y.resight
catid <- "200023"
catindex <- which(row.names(y.resight[,,1]) == catid)
y.resight[catindex,,]
View(resight_marked[[catid]])
marked_mtlion_pics10
start_resight_interval_vec
resight_interval_name

# 8. X.marking
trapid <- "38"
trapindex <- which(row.names(X.marking) == trapid)
X.marking[trapindex,]
sf_trapgrid_centroid[sf_trapgrid_centroid$TrapName == "10",]
(m <- ggplot(flt_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=sf_trapgrid_centroid, color = "blue", fill = NA, size = 1.5) +
    xlab("") +
    ylab(""))

# 9. y.marking
catid <- "201382"
catindex <- which(row.names(y.marking) == catid)
y.marking[catindex,]
trapid <- marked_mtlion_captures[marked_mtlion_captures$`Animal ID` == catid,]$TrapName
marked_mtlion_captures[marked_mtlion_captures$`Animal ID` == catid,]
(m <- ggplot(flt_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=sf_trapgrid_centroid, color = "blue", fill = NA, size = 1.5) +
    geom_sf(data=sf_trapgrid_centroid$geometry[sf_trapgrid_centroid$TrapName == trapid,], color = "black", fill = NA, size = 1.5) +
    geom_sf(data=marked_mtlion_captures$geometry[marked_mtlion_captures$`Animal ID` == catid,], color = "red", fill = NA, size = 1.5) +
    xlab("") +
    ylab(""))

# unit 77 - 168095 acres - 680 sq km
# unit 75 - 391107 acres - 1582 sq km
# estimate for area is 1400 km2 - which is roughly unit 77 + half of 75 


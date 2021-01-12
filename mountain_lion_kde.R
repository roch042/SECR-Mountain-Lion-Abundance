require(tidyverse)
require(sp)
require(sf)
require(adehabitatHR)
require(ggtext)

#I think the summary is a great place to start, then we can look at that and decide how to proceed.  Home ranges, home range sizes, home range overlaps of collared cats (if there are any), the summary number of camera detections of all lions, collared lions, potentially different collared lions.  All of that is good for the discussion

# Load Data ####

# state/gmu dataframes - transform

load("data/spatial_objects.RData")

proj4string(geo_state) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
geo_state_idaho <- spTransform(geo_state, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_geo_state_idaho <- sf::st_as_sf(geo_state_idaho) # sf object

proj4string(ID_gmu) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
ID_gmu_idaho <- spTransform(ID_gmu, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_ID_gmu_idaho <- sf::st_as_sf(ID_gmu_idaho) # sf object

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

# mtlion collar data - 12/01/2019 to 12/01/2020

DAT <- readr::read_csv("data/data-2020-12-09.csv")


# Filter Data ####

win_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2019-12-01" & lubridate::date(`Location Date (GMT)`) <= "2020-03-31") %>%
  dplyr::filter(!is.na(`Animal ID`))

sum_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2020-05-01" & lubridate::date(`Location Date (GMT)`) <= "2020-11-30") %>%
  dplyr::filter(!is.na(`Animal ID`))

# a few summer homeranges are weird so censor them

sum_dat_200026 <- sum_dat %>%
  dplyr::filter(`Animal ID` == "200026" & lubridate::date(`Location Date (GMT)`) <= "2020-09-01")

sum_dat_200012 <- sum_dat %>%
  dplyr::filter(`Animal ID` == "200012" & lubridate::date(`Location Date (GMT)`) >= "2020-05-15")

sum_dat_200019 <- sum_dat %>%
  dplyr::filter(`Animal ID` == "200019" & lubridate::date(`Location Date (GMT)`) <= "2020-05-15")

sum_dat_censor <- sum_dat %>%
  dplyr::filter(!`Animal ID` %in% c("200026","200012","200019")) %>%
  dplyr::bind_rows(sum_dat_200026, sum_dat_200012, sum_dat_200019)


# Seasonal Homeranges by Animal ####

col_of_interest <- c("Animal ID",  "Longitude (Location)", "Latitude (Location)")

# _winter ####
win_dat_sp <- win_dat[,col_of_interest]
names(win_dat_sp)<-c("id", "x", "y")
coordinates(win_dat_sp)<- c("x", "y")
proj4string(win_dat_sp)<- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
win_dat_sp_idaho <- spTransform(win_dat_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
win_dat_sf_idaho <- sf::st_as_sf(win_dat_sp_idaho)

kernel.ref <- adehabitatHR::kernelUD(win_dat_sp_idaho, h = "href")  # href = the reference bandwidth
win_pel.kernel.poly <- adehabitatHR::getverticeshr(kernel.ref, percent = 95, unout = "km2")
sf_win_pel.kernel.poly <- sf::st_as_sf(win_pel.kernel.poly)

# _summer ####
sum_dat_sp <- sum_dat_censor[,col_of_interest]
names(sum_dat_sp)<-c("id", "x", "y")
coordinates(sum_dat_sp)<- c("x", "y")
proj4string(sum_dat_sp)<- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sum_dat_sp_idaho <- spTransform(sum_dat_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sum_dat_sf_idaho <- sf::st_as_sf(sum_dat_sp_idaho)

kernel.ref <- adehabitatHR::kernelUD(sum_dat_sp_idaho, h = "href")  # href = the reference bandwidth
sum_pel.kernel.poly <- adehabitatHR::getverticeshr(kernel.ref, percent = 95, unout = "km2")
sf_sum_pel.kernel.poly <- sf::st_as_sf(sum_pel.kernel.poly)

n_animals<-unique(c(sum_dat_sp_idaho$id,win_dat_sp_idaho$id))

mtlion_index <- DAT %>%
  dplyr::select("Animal ID","Sex","Age Class (Capture)","Capture Date") %>%
  dplyr::distinct() %>%
  tidyr::unite(label, `Animal ID`, Sex, `Age Class (Capture)`,sep=", ",remove=FALSE) %>%
  dplyr::mutate(`Animal ID` = as.character(`Animal ID`))

# mountain lions by season ####

mapfun <- function(
  spec_animal=n_animals[i],
  mtlion_index=mtlion_index,
  win_dat_map=win_dat_sf_idaho,
  win_kernel=sf_win_pel.kernel.poly,
  sum_dat_map=sum_dat_sf_idaho,
  sum_kernel=sf_sum_pel.kernel.poly,
  all_gmu = sf_ID_gmu_idaho,
  caption_info = "Winter (12/01/2019 to 03/31/2020) in blue. \n Summer (05/01/2020 to 11/30/2020) in orange.")
{
  
  spec_sex <- na.omit(tolower(unique(mtlion_index$Sex[mtlion_index$`Animal ID`==spec_animal])))
  spec_age <- na.omit(tolower(unique(mtlion_index$`Age Class (Capture)`[mtlion_index$`Animal ID`==spec_animal])))
  spec_capture <- na.omit(lubridate::date(unique(mtlion_index$`Capture Date`[mtlion_index$`Animal ID`==spec_animal])))
  
  win_kernel<-win_kernel[win_kernel$id==spec_animal,]
  sum_kernel<-sum_kernel[sum_kernel$id==spec_animal,]
  
  win_dat_map <- win_dat_map[win_dat_map$id==spec_animal,]
  sum_dat_map <- sum_dat_map[sum_dat_map$id==spec_animal,]
  
  win_gmu <- sf::st_intersection(all_gmu, win_dat_map)
  sum_gmu <- sf::st_intersection(all_gmu, sum_dat_map)
  
  flt_gmu <- all_gmu %>%
    dplyr::filter(NAME %in% unique(na.omit(c(as.character(unique(win_gmu$NAME)),as.character(unique(sum_gmu$NAME))))))
  
  flt_gmu_bbox <- sf::st_bbox(flt_gmu)
  win_dat_bbox <- sf::st_bbox(win_dat_map)
  sum_dat_bbox <- sf::st_bbox(sum_dat_map)  
  win_kernel_bbox <- sf::st_bbox(win_kernel)
  sum_kernel_bbox <- sf::st_bbox(sum_kernel)  
  
  # xmin <- min(c(win_kernel_bbox$xmin, win_dat_bbox$xmin, sum_kernel_bbox$xmin, sum_dat_bbox$xmin))
  # xmax <- max(c(win_kernel_bbox$xmax, win_dat_bbox$xmax, sum_kernel_bbox$xmax, sum_dat_bbox$xmax))
  # ymin <- min(c(win_kernel_bbox$ymin, win_dat_bbox$ymin, sum_kernel_bbox$ymin, sum_dat_bbox$ymin))
  # ymax <- max(c(win_kernel_bbox$ymax, win_dat_bbox$ymax, sum_kernel_bbox$ymax, sum_dat_bbox$ymax))
  
  # xmin <- min(c(flt_gmu_bbox$xmin, win_dat_bbox$xmin, sum_dat_bbox$xmin), na.rm=T)
  # xmax <- max(c(flt_gmu_bbox$xmax, win_dat_bbox$xmax, sum_dat_bbox$xmax), na.rm=T)
  # ymin <- min(c(flt_gmu_bbox$ymin, win_dat_bbox$ymin, sum_dat_bbox$ymin), na.rm=T)
  # ymax <- max(c(flt_gmu_bbox$ymax, win_dat_bbox$ymax, sum_dat_bbox$ymax), na.rm=T)
  
  # xmin <- min(c(win_dat_bbox$xmin, sum_dat_bbox$xmin), na.rm=T)
  # xmax <- max(c(win_dat_bbox$xmax, sum_dat_bbox$xmax), na.rm=T)
  # ymin <- min(c(win_dat_bbox$ymin, sum_dat_bbox$ymin), na.rm=T)
  # ymax <- max(c(win_dat_bbox$ymax, sum_dat_bbox$ymax), na.rm=T)
  
  xmin <- min(c(sum_kernel_bbox$xmin, win_kernel_bbox$xmin, win_dat_bbox$xmin, sum_dat_bbox$xmin), na.rm=T)
  xmax <- max(c(sum_kernel_bbox$xmax, win_kernel_bbox$xmax, win_dat_bbox$xmax, sum_dat_bbox$xmax), na.rm=T)
  ymin <- min(c(sum_kernel_bbox$ymin, win_kernel_bbox$ymin, win_dat_bbox$ymin, sum_dat_bbox$ymin), na.rm=T)
  ymax <- max(c(sum_kernel_bbox$ymax, win_kernel_bbox$ymax, win_dat_bbox$ymax, sum_dat_bbox$ymax), na.rm=T)
  
  # bbox <- sf::st_bbox(dat[dat$id==spec_animal,])
  m <- ggplot(flt_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=win_kernel, color = "blue", fill = NA, size = 1.5) +
    geom_sf(data=win_dat_map, color = "blue", alpha = 0.2) +
    geom_sf(data=sum_kernel, color = "dark orange", fill = NA, size = 1.5) +
    geom_sf(data=sum_dat_map, color = "dark orange", alpha = 0.2) +
    xlab("") +
    ylab("") +
    labs(title = paste0(spec_age," ",spec_sex," ",spec_animal," captured on ",spec_capture),
            subtitle = paste0("winter = ",round(win_kernel$area, digits=0)," km2, summer = ",round(sum_kernel$area, digits=0)," km2"),
            caption = caption_info) #+
    # coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
  print(m)
  
}

pdf(file = "kde_mountain_lions_by_season.pdf",onefile=TRUE)
for (i in 1:length(n_animals)){
  mapfun(spec_animal=n_animals[i], mtlion_index=mtlion_index, win_dat=win_dat_sf_idaho, win_kernel=sf_win_pel.kernel.poly, sum_dat=sum_dat_sf_idaho, sum_kernel=sf_sum_pel.kernel.poly)
}
dev.off()

# Overlapping ranges ####

gmus <- list("Region 1" = c("6"), "Region 2" = c("10A"), "Region 3" =  c("32","32A","24","33"), "Region 5" = c("72","74","75","77"))

mapfun1 <- function(
  mtlion_index=mtlion_index,
  dat_map=win_dat_sf_idaho,
  kernel_map=sf_win_pel.kernel.poly,
  all_gmu = sf_ID_gmu_idaho,
  season = "Winter",
  dates = "12/01/2019 to 03/31/2020",
  gmus = gmus,
  region
){
  
  # sum_dat_map=sum_dat_sf_idaho
  # sum_kernel=sf_sum_pel.kernel.poly
  
  gmus_chosen <- gmus
  # gmus_chosen <- "6"
  
  flt_gmu <- all_gmu %>%
    dplyr::filter(NAME %in% gmus_chosen)
  
  flt_int <- sf::st_intersection(kernel_map, flt_gmu) 
  flt_ids <- unique(flt_int$id)
  
  flt_kernel_map <- kernel_map %>%
    dplyr::filter(id %in% flt_ids) %>%
    dplyr::left_join(mtlion_index, by = c("id" = "Animal ID"))
  
  flt_gmu_bbox <- sf::st_bbox(flt_gmu)
  
  xmin <- min(c(flt_gmu_bbox$xmin), na.rm=T)
  xmax <- max(c(flt_gmu_bbox$xmax), na.rm=T)
  ymin <- min(c(flt_gmu_bbox$ymin), na.rm=T)
  ymax <- max(c(flt_gmu_bbox$ymax), na.rm=T)
  
  # bbox <- sf::st_bbox(dat[dat$id==spec_animal,])
  m <- ggplot(flt_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=flt_kernel_map, aes(color=label), fill = NA, size = 1) +
    xlab("") +
    ylab("") +
    labs(title = paste0(season," - ",region),
         subtitle = dates)
  
  print(m)
  
}

# All Winter Ranges ####

pdf(file = "winter_kde_lions_region.pdf",onefile=TRUE)
for (i in 1:length(gmus)){
  mapfun1(
    mtlion_index=mtlion_index,
    dat_map=win_dat_sf_idaho,
    kernel_map=sf_win_pel.kernel.poly,
    all_gmu = sf_ID_gmu_idaho,
    season = "Winter",
    dates = "12/01/2019 to 03/31/2020",
    gmus = gmus[[i]],
    region = names(gmus)[[i]]
  )
}
dev.off()


# All Summer Ranges ####

pdf(file = "summer_kde_lions_region.pdf",onefile=TRUE)
for (i in 1:length(gmus)){
  mapfun1(
    mtlion_index=mtlion_index,
    dat_map=sum_dat_sf_idaho,
    kernel_map=sf_sum_pel.kernel.poly,
    all_gmu = sf_ID_gmu_idaho,
    season = "Summer",
    dates = "05/01/2019 to 11/30/2020",
    gmus = gmus[[i]],
    region = names(gmus)[[i]]
  )
}
dev.off()

to_save <- c("mtlion_index","sf_sum_pel.kernel.poly","sf_win_pel.kernel.poly","win_dat_sf_idaho","sum_dat_sf_idaho","sf_geo_state_idaho","sf_ID_gmu_idaho")

save(list=to_save, file="mountain_lion_kde.RData")

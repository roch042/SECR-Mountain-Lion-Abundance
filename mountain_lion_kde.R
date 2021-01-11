require(tidyverse)
require(sp)
require(adehabitatHR)


#I think the summary is a great place to start, then we can look at that and decide how to proceed.  Home ranges, home range sizes, home range overlaps of collared cats (if there are any), the summary number of camera detections of all lions, collared lions, potentially different collared lions.  All of that is good for the discussion

# 
load("data/spatial_objects.RData")

DAT <- readr::read_csv("data/data-2020-12-09.csv")

win_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2019-12-01" & lubridate::date(`Location Date (GMT)`) <= "2020-03-31") %>%
  dplyr::filter(!is.na(`Animal ID`))

sum_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2020-05-01" & lubridate::date(`Location Date (GMT)`) <= "2020-12-31") %>%
  dplyr::filter(!is.na(`Animal ID`))

proj4string(geo_state) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
geo_state_idaho <- spTransform(geo_state, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_geo_state_idaho <- sf::st_as_sf(geo_state_idaho) # sf object

proj4string(ID_gmu) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
ID_gmu_idaho <- spTransform(ID_gmu, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
sf_ID_gmu_idaho <- sf::st_as_sf(ID_gmu_idaho) # sf object

col_of_interest <- c("Animal ID",  "Longitude (Location)", "Latitude (Location)")

# winter ####
win_dat_sp <- win_dat[,col_of_interest]
names(win_dat_sp)<-c("id", "x", "y")
coordinates(win_dat_sp)<- c("x", "y")
proj4string(win_dat_sp)<- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
win_dat_sp_idaho <- spTransform(win_dat_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
win_dat_sf_idaho <- sf::st_as_sf(win_dat_sp_idaho)

kernel.ref <- adehabitatHR::kernelUD(win_dat_sp_idaho, h = "href")  # href = the reference bandwidth
win_pel.kernel.poly <- adehabitatHR::getverticeshr(kernel.ref, percent = 95, unout = "km2")
sf_win_pel.kernel.poly <- sf::st_as_sf(win_pel.kernel.poly)

# summer ####
sum_dat_sp <- sum_dat[,col_of_interest]
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
  dplyr::distinct()

# function

# mapfun <- function(n_year=n_year){
#   pelplyyear<-pel.kernel.poly[pel.kernel.poly@data$id==n_year,]
#   plot(lakes, col="paleturquoise3", border=TRUE, main= n_year, cex.main=1.5, ylim=c(42.0,43.5))
#   plot(pelplyyear, border="sienna3" , lwd=2, col="transparent", add=TRUE)
#   points(pel_sp[pel_sp@data$id==n_year,], col=scales::alpha("sienna2", 0.5), pch = 16, cex=0.5)
#   text(x= -113.5, y=43.8, paste("Total Locs:", nrow(pel_sp[pel_sp@data$id==n_year,])), cex=0.8)
# }


mapfun <- function(
  spec_animal=n_animals[i],
  mtlion_index=mtlion_index,
  win_dat=win_dat_sf_idaho,
  win_kernel=sf_win_pel.kernel.poly,
  sum_dat=sum_dat_sf_idaho,
  sum_kernel=sf_sum_pel.kernel.poly,
  all_gmu = sf_ID_gmu_idaho)
{
  
  spec_sex <- unique(mtlion_index$Sex[mtlion_index$`Animal ID`==spec_animal])
  spec_age <- unique(mtlion_index$`Age Class (Capture)`[mtlion_index$`Animal ID`==spec_animal])
  spec_capture <- lubridate::date(unique(mtlion_index$`Capture Date`[mtlion_index$`Animal ID`==spec_animal]))
  
  win_kernel<-win_kernel[win_kernel$id==spec_animal,]
  sum_kernel<-sum_kernel[sum_kernel$id==spec_animal,]
  
  win_dat <- win_dat[win_dat$id==spec_animal,]
  sum_dat <- sum_dat[sum_dat$id==spec_animal,]
  
  win_gmu <- sf::st_intersection(all_gmu,win_dat)
  sum_gmu <- sf::st_intersection(all_gmu, sum_dat)
  
  flt_gmu <- all_gmu %>%
    dplyr::filter(NAME %in% unique(unique(win_gmu$NAME),unique(sum_gmu$NAME)))
  
  flt_gmu_bbox <- sf::st_bbox(flt_gmu)
  win_dat_bbox <- sf::st_bbox(win_dat)
  sum_dat_bbox <- sf::st_bbox(sum_dat)  
  
  # xmin <- min(c(win_kernel_bbox$xmin, win_dat_bbox$xmin, sum_kernel_bbox$xmin, sum_dat_bbox$xmin))
  # xmax <- max(c(win_kernel_bbox$xmax, win_dat_bbox$xmax, sum_kernel_bbox$xmax, sum_dat_bbox$xmax))
  # ymin <- min(c(win_kernel_bbox$ymin, win_dat_bbox$ymin, sum_kernel_bbox$ymin, sum_dat_bbox$ymin))
  # ymax <- max(c(win_kernel_bbox$ymax, win_dat_bbox$ymax, sum_kernel_bbox$ymax, sum_dat_bbox$ymax))
  
  xmin <- min(c(flt_gmu_bbox$xmin, win_dat_bbox$xmin, sum_dat_bbox$xmin), na.rm=T)
  xmax <- max(c(flt_gmu_bbox$xmax, win_dat_bbox$xmax, sum_dat_bbox$xmax), na.rm=T)
  ymin <- min(c(flt_gmu_bbox$ymin, win_dat_bbox$ymin, sum_dat_bbox$ymin), na.rm=T)
  ymax <- max(c(flt_gmu_bbox$ymax, win_dat_bbox$ymax, sum_dat_bbox$ymax), na.rm=T)
  
  # bbox <- sf::st_bbox(dat[dat$id==spec_animal,])
  m <- ggplot(all_gmu) +
    geom_sf(color = "dark gray") + 
    geom_sf_text(aes(label = NAME), size = 3) +
    geom_sf(data=win_kernel, color = "blue", fill = NA, size = 1.5) +
    geom_sf(data=win_dat, color = "blue", alpha = 0.2) +
    geom_sf(data=sum_kernel, color = "dark orange", fill = NA, size = 1.5) +
    geom_sf(data=sum_dat, color = "dark orange", alpha = 0.2) +
    ggtitle(paste0("Lion ",spec_animal,", ",spec_sex,", ",spec_age," at capture on ",spec_capture),
            subtitle = paste0("winter = ",round(win_kernel$area, digits=0)," km2, summer = ",round(sum_kernel$area, digits=0)," km2")) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  
  print(m)
  
}

pdf(file = "kde_mountain_lions.pdf",onefile=TRUE)
for (i in 1:length(n_animals)){
  mapfun(spec_animal=n_animals[i], mtlion_index=mtlion_index, win_dat=win_dat_sf_idaho, win_kernel=sf_win_pel.kernel.poly, sum_dat=sum_dat_sf_idaho, sum_kernel=sf_sum_pel.kernel.poly)
}
dev.off()

require(tidyverse)
require(sp)
require(adehabitatHR)

# 
load("spatial_objects.RData")

DAT <- readr::read_csv("data-2020-12-09.csv")

win_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2019-12-01" & lubridate::date(`Location Date (GMT)`) <= "2020-03-31") %>%
  dplyr::filter(!is.na(`Animal ID`))

sum_dat <- DAT %>%
  dplyr::filter(lubridate::date(`Location Date (GMT)`) >= "2020-05-01" & lubridate::date(`Location Date (GMT)`) <= "2020-12-31") %>%
  dplyr::filter(!is.na(`Animal ID`))

proj4string(geo_state) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
geo_state_idaho <- spTransform(geo_state, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator

# winter ####
win_dat_sp <- win_dat[,c("Animal ID", "Longitude (Location)", "Latitude (Location)")]
names(win_dat_sp)<-c("id", "x", "y")
coordinates(win_dat_sp)<- c("x", "y")
proj4string(win_dat_sp)<- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
win_dat_sp_idaho <- spTransform(win_dat_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
kernel.ref <- adehabitatHR::kernelUD(win_dat_sp_idaho, h = "href")  # href = the reference bandwidth
win_pel.kernel.poly <- adehabitatHR::getverticeshr(kernel.ref, percent = 95)

# summer ####
sum_dat_sp <- sum_dat[,c("Animal ID", "Longitude (Location)", "Latitude (Location)")]
names(sum_dat_sp)<-c("id", "x", "y")
coordinates(sum_dat_sp)<- c("x", "y")
proj4string(sum_dat_sp)<- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
sum_dat_sp_idaho <- spTransform(sum_dat_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
kernel.ref <- adehabitatHR::kernelUD(sum_dat_sp_idaho, h = "href")  # href = the reference bandwidth
sum_pel.kernel.poly <- adehabitatHR::getverticeshr(kernel.ref, percent = 95)

n_animals<-unique(c(sum_dat_sp_idaho$id,win_dat_sp_idaho$id))


# function

# mapfun <- function(n_year=n_year){
#   pelplyyear<-pel.kernel.poly[pel.kernel.poly@data$id==n_year,]
#   plot(lakes, col="paleturquoise3", border=TRUE, main= n_year, cex.main=1.5, ylim=c(42.0,43.5))
#   plot(pelplyyear, border="sienna3" , lwd=2, col="transparent", add=TRUE)
#   points(pel_sp[pel_sp@data$id==n_year,], col=scales::alpha("sienna2", 0.5), pch = 16, cex=0.5)
#   text(x= -113.5, y=43.8, paste("Total Locs:", nrow(pel_sp[pel_sp@data$id==n_year,])), cex=0.8)
# }

mapfun <- function(n_year=n_animals, dat_sp = win_dat_sp_idaho, pel.kernel.poly = win_pel.kernel.poly, season = "Winter"){
  pel.kernel.poly<-pel.kernel.poly[pel.kernel.poly@data$id==n_year,]
  plot(geo_state, col="paleturquoise3", border=TRUE, cex.main=1.5, main = paste0("Animal ID:",n_year,", \n Season: ",season), cex = 0.8)
  plot(pel.kernel.poly, border="sienna3" , lwd=2, col="transparent", add = T, cex = 0.8)
  points(dat_sp[dat_sp@data$id==n_year,], col=scales::alpha("sienna2", 0.5), pch = 16, cex=0.5)
  # text(x= -113.5, y=43.8, paste("Total Locs:", nrow(pel.kernel.poly[pel.kernel.poly@data$id==n_year,])), cex=0.8)
}

mapfun1 <- function(n_year=n_animals, dat_sp = win_dat_sp, pel.kernel.poly = win_pel.kernel.poly, season = "Winter"){
  pel.kernel.poly<-pel.kernel.poly[pel.kernel.poly@data$id==n_year,]
  plot(pel.kernel.poly, border="sienna3" , lwd=2, col="transparent", main = paste0("Animal ID:",n_year,", \n Season: ",season), cex = 0.8)
  points(dat_sp[dat_sp@data$id==n_year,], col=scales::alpha("sienna2", 0.5), pch = 16, cex=0.5)
  # text(x= -113.5, y=43.8, paste("Total Locs:", nrow(pel.kernel.poly[pel.kernel.poly@data$id==n_year,])), cex=0.8)
}

mapfun2 <- function(n_year=n_animals, win_dat_sp = win_dat_sp, sum_dat_sp = sum_dat_sp, plt_win_pel.kernel.poly = win_pel.kernel.poly, plt_sum_pel.kernel.poly = sum_pel.kernel.poly){
  plt_win_pel.kernel.poly<-plt_win_pel.kernel.poly[plt_win_pel.kernel.poly@data$id==n_year,]
  plt_sum_pel.kernel.poly<-plt_sum_pel.kernel.poly[plt_sum_pel.kernel.poly@data$id==n_year,]
min_y <- min(plt_win_pel.kernel.poly@bbox["y","min"],plt_sum_pel.kernel.poly@bbox["y","min"], na.rm=T)
max_y <- max(plt_win_pel.kernel.poly@bbox["y","max"],plt_sum_pel.kernel.poly@bbox["y","max"], na.rm=T)
min_x <- min(plt_win_pel.kernel.poly@bbox["x","min"],plt_sum_pel.kernel.poly@bbox["x","min"], na.rm=T)
max_x <- max(plt_win_pel.kernel.poly@bbox["x","max"],plt_sum_pel.kernel.poly@bbox["x","max"], na.rm=T)
  plot(plt_win_pel.kernel.poly, border="lightblue3" , lwd=2, col="transparent", main = paste0("Animal ID:",n_year), cex = 0.8, ylim=c(min_y,max_y), xlim=c(min_x,max_x))
  points(win_dat_sp[win_dat_sp@data$id==n_year,], col=scales::alpha("lightblue2", 0.5), pch = 16, cex=0.5)

  plot(plt_sum_pel.kernel.poly, border="sienna3" , lwd=2, col="transparent", main = paste0("Animal ID:",n_year), cex = 0.8, add = T)
  points(sum_dat_sp[sum_dat_sp@data$id==n_year,], col=scales::alpha("sienna2", 0.5), pch = 16, cex=0.5)
  # text(x= -113.5, y=43.8, paste("Total Locs:", nrow(pel.kernel.poly[pel.kernel.poly@data$id==n_year,])), cex=0.8)
}


pdf(file = "kde_mountain_lions.pdf")
for (i in 1:length(n_animals)){
  par(mfrow=c(2,2))
mapfun(n_year=n_animals[i], dat_sp = win_dat_sp_idaho, pel.kernel.poly = win_pel.kernel.poly, season = "Winter")
mapfun(n_year=n_animals[i], dat_sp = sum_dat_sp_idaho, pel.kernel.poly = sum_pel.kernel.poly, season = "Summer") 
mapfun1(n_year=n_animals[i], dat_sp = win_dat_sp_idaho, pel.kernel.poly = win_pel.kernel.poly, season = "Winter")
mapfun1(n_year=n_animals[i], dat_sp = sum_dat_sp_idaho, pel.kernel.poly = sum_pel.kernel.poly, season = "Summer") 
}
dev.off()

pdf(file = "kde_mountain_lions_seasons.pdf")
for (i in 1:length(n_animals)){
  mapfun2(n_year=n_animals[i], win_dat_sp = win_dat_sp_idaho, sum_dat_sp = sum_dat_sp_idaho, plt_win_pel.kernel.poly = win_pel.kernel.poly, plt_sum_pel.kernel.poly = sum_pel.kernel.poly) 
}
dev.off()
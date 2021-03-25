fnc_camera_collar_map <- function(
  start_date = "2020-04-01",
  end_date = "2020-04-20",
  spec_animal = NA,
  homerange = FALSE,
  collarloc = TRUE,
  min_circle_radius = 1,
  max_circle_radius = 5,
  DAT = DAT,
  sf_ID_gmu = sf_ID_gmu,
  mtlion_index = mtlion_index,
  mtlion_pics = mtlion_pics,
  sf_mtlion_pic_grid = sf_mtlion_pic_grid,
  sf_mtlion_marked_pic_grid = sf_mtlion_marked_pic_grid,
  sf_pic_grid = sf_pic_grid,
  input = input
)
{
 
  if(collarloc){ 
    # _collar data - points ####

    req(input$flt_spec_animal)
    
    flt_collar <- DAT %>%
      dplyr::filter(lubridate::date(`Location Date (GMT)`) >= start_date & lubridate::date(`Location Date (GMT)`) <= end_date) %>%
      {if ( all(!is.na(spec_animal)) ) dplyr::filter(., `Animal ID` %in% spec_animal ) else . } %>%
      dplyr::filter(!is.na(`Animal ID`))
    
    flt_collar_sp <- flt_collar
    flt_collar_sp <- flt_collar_sp[,c("Animal ID",  "Longitude (Location)", "Latitude (Location)")]
    names(flt_collar_sp)<-c("id", "x", "y")
    coordinates(flt_collar_sp)<- c("x", "y")
    proj4string(flt_collar_sp) <- CRS("+init=epsg:4326")
    flt_collar_sf <- sf::st_as_sf(flt_collar_sp)
    # flt_collar_sp_idaho <- spTransform(flt_collar_sp, CRS("+init=epsg:8826")) # transform to Idaho Transverse Mercator
    # flt_collar_sf_idaho <- sf::st_as_sf(flt_collar_sp_idaho)
    
    # _collar data - locations ####
    
    flt_collar_allcols <- flt_collar
    coordinates(flt_collar_allcols)<- c("Longitude (Location)", "Latitude (Location)")
    proj4string(flt_collar_allcols) <- CRS("+init=epsg:4326")
    flt_collar_allcols_sf <- sf::st_as_sf(flt_collar_allcols)

    if(homerange){
      # _collar data - kde ####
      kernel_ref <- adehabitatHR::kernelUD(flt_collar_sp, h = "href")  # href = the reference bandwidth
      kernel_poly <- adehabitatHR::getverticeshr(kernel_ref, percent = 95, unout = "km2")
      sf_kernel_poly <- sf::st_as_sf(kernel_poly)
    } 
  }
  
  # _filter pictures to dates ####
  
  flt_gmu <- sf_ID_gmu %>%
    dplyr::filter(NAME %in% c("74","77","75","78"))
  
  flt_sf_mtlion_pic_grid <- sf_mtlion_pic_grid %>%
    dplyr::filter(lubridate::date(Date) >= start_date & lubridate::date(Date) <= end_date) %>%
    tidyr::unite("DateTime", Date, Time, sep = " ") %>%
    dplyr::group_by(Project, CamID) %>%
    dplyr::summarise(Date = paste(DateTime, collapse= ", "))
  
  flt_sf_mtlion_pic <- mtlion_pics %>%
    dplyr::filter(lubridate::date(Date) >= start_date & lubridate::date(Date) <= end_date) %>%
    dplyr::filter(CamID %in% flt_sf_mtlion_pic_grid$CamID)
  
  numpics <- flt_sf_mtlion_pic %>%
    dplyr::group_by(CamID) %>%
    dplyr::summarise(numpics = length(unique(File)))

  flt_sf_mtlion_pic_grid <- dplyr::left_join(flt_sf_mtlion_pic_grid, numpics)
  
  flt_sf_mtlion_marked_pic_grid <- sf_mtlion_marked_pic_grid %>%
    dplyr::filter(lubridate::date(Date) >= start_date & lubridate::date(Date) <= end_date) %>%
    tidyr::unite("DateTime", Date, Time, sep = " ") %>%
    dplyr::group_by(Project, CamID) %>%
    dplyr::summarise(Date = paste(DateTime, collapse= ", "))
  
  flt_sf_mtlion_marked_pic <- mtlion_pics %>%
    dplyr::filter(MarkedAnimal == TRUE) %>%
    dplyr::filter(lubridate::date(Date) >= start_date & lubridate::date(Date) <= end_date) %>%
    dplyr::filter(CamID %in% flt_sf_mtlion_marked_pic_grid$CamID)
  
  numpics <- flt_sf_mtlion_marked_pic %>%
    dplyr::group_by(CamID) %>%
    dplyr::summarise(numpics = length(unique(File)))
  
  flt_sf_mtlion_marked_pic_grid <- dplyr::left_join(flt_sf_mtlion_marked_pic_grid, numpics)
  
  # KDE
  if(collarloc){
    if(homerange){
      flt_animal_ids <- sf::st_intersection(sf_kernel_poly,flt_gmu)
      flt_animal_ids <- unique(flt_animal_ids$id)
      
      flt_sf_kernel_poly <- sf_kernel_poly %>%
        dplyr::filter(id %in% flt_animal_ids) %>%
        dplyr::left_join(mtlion_index, by = c("id"="Animal ID"))
      
      color_group_domain <- flt_sf_kernel_poly$label
      
    } else {
      # Locations

      flt_animal_ids <- sf::st_intersection(flt_collar_allcols_sf,flt_gmu)
      flt_animal_ids <- unique(flt_animal_ids$Animal.ID)
      
      flt_collar_sf_gmu <- flt_collar_allcols_sf %>%
        dplyr::filter(`Animal ID` %in% flt_animal_ids) %>%
        dplyr::mutate(`Animal ID` = as.character(`Animal ID`)) %>%
        dplyr::left_join(mtlion_index[,c("Animal ID","label")], by = c("Animal ID"="Animal ID"))
      
      color_group_domain <- flt_collar_sf_gmu$label
      
    }
  }
  
  if(collarloc){
    pal_color_choice <- colorFactor(palette = "viridis",
                                    domain = color_group_domain,
                                    ordered = FALSE,
                                    na.color = "transparent",
                                    alpha = FALSE,
                                    reverse = FALSE)
  }
  
  popup_pic_grid <- paste0(" <span style='font-weight: bold;text-decoration: underline'> ", 
                           "Camera: ", sf_pic_grid$CamID, 
                           " </span> <br> ",
                           "Start of Deployment: ", sf_pic_grid$first_pic, 
                           " <br> ",
                           "End of Deployment: ", sf_pic_grid$last_pic)
  
  popup_mtlion_pic_grid <- paste0(" <span style='font-weight: bold;text-decoration: underline'> ", 
                                  "Camera: ", flt_sf_mtlion_pic_grid$CamID, 
                                  " </span> <br> ",
                                  "Pictures of Lions: ",flt_sf_mtlion_pic_grid$numpics,
                                  "<br>",
                                  "Seen On: ", flt_sf_mtlion_pic_grid$Date)
  
  popup_mtlion_marked_pic_grid <- paste0(" <span style='font-weight: bold;text-decoration: underline'> ", 
                                         "Camera: ", flt_sf_mtlion_marked_pic_grid$CamID, 
                                         " </span> <br> ",
                                         "Pictures of Marked Lions: ", flt_sf_mtlion_marked_pic_grid$numpics,
                                         "<br>",
                                         "Seen On: ", flt_sf_mtlion_marked_pic_grid$Date)
  
  device.map <- leaflet() %>%
    
    addProviderTiles("Esri.WorldGrayCanvas",
                     options = providerTileOptions(attribution = NA), group = "Default") %>%
    
    # satellite
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    
    # add polygons of GMUs
    addPolygons(data=flt_gmu, stroke=TRUE, weight=1, color="black", fillOpacity=0.01, label=flt_gmu$NAME, group = "GMUs") %>%
    
    addCircleMarkers(data = sf_pic_grid,
                     radius = 2, 
                     stroke = FALSE,
                     color = "#808080",
                     fillOpacity = 1,
                     opacity = 1,
                     group = "Deployed Cameras",
                     popup = popup_pic_grid) %>%
    
    addCircleMarkers(data = flt_sf_mtlion_pic_grid,
                     weight = 1,
                     radius = flt_sf_mtlion_pic_grid$numpics/3, 
                     fillColor = "#3297a8",
                     fillOpacity = 1,
                     opacity = 1,
                     group = "Mountain Lion Camera",
                     popup = popup_mtlion_pic_grid) %>%
    
    addCircleMarkers(data = flt_sf_mtlion_marked_pic_grid,
                     weight = 1,
                     radius = flt_sf_mtlion_marked_pic_grid$numpics,
                     fillColor = "#9e32a8",
                     fillOpacity = 1,
                     opacity = 1,
                     group = "Marked Mountain Lion Camera",
                     popup = popup_mtlion_marked_pic_grid)
  
  device.map_grid <- device.map
  
  if(collarloc){
    if(homerange){
      # KDE 
      
      layer.group <- list()
      
      for(i in 1:length(flt_animal_ids)) {
        
        df <- flt_sf_kernel_poly %>%
          dplyr::filter(id == flt_animal_ids[i])
        
        coords <- sf::st_coordinates(df)
        
        group_label <- df$label
        
        device.map <- device.map %>%
          
          # add polygons of homeranges
          addPolygons(device.map,
                      data=df,
                      lng = coords[,"X"], lat = coords[,"Y"],
                      stroke=TRUE,
                      weight=3,
                      color = ~pal_color_choice(label),
                      fillOpacity=0.3,
                      opacity = 1,
                      group = group_label,
                      popup = paste(sep = "<br>",
                                    paste("<b>Animal ID:</b> ", flt_animal_ids[i]),
                                    paste("<b>Sex:</b> ", df$Sex),
                                    paste("<b>Age Class (Capture):</b> ", df$`Age Class (Capture)`),
                                    paste("<b>Capture Date:</b> ", df$`Capture Date`)))
        
        layer.group <- c(layer.group, as.character(group_label))
        
      }
      
    } else {
      
      
      layer.group <- list()

      for(i in 1:length(flt_animal_ids)) {

        df <- flt_collar_sf_gmu %>%
          dplyr::filter(`Animal ID` == flt_animal_ids[i])  %>%
          dplyr::arrange(`Location Date (GMT)`)
        
        coords <- sf::st_coordinates(df)
        
        group_label <- df$label
        
        crcrads <- seq(min_circle_radius,max_circle_radius,length.out=nrow(df))
        
        device.map <- device.map %>%
          
          # add polygons of homeranges
          addPolylines(device.map, 
                       data=df, 
                       lng = coords[,"X"], lat = coords[,"Y"],
                       stroke=TRUE, 
                       color = ~pal_color_choice(label),
                       weight = 1,
                       opacity = .6,
                       fillOpacity = .6,
                       group = group_label) %>%
          
          addCircleMarkers(data=df, 
                           lng = coords[,"X"], lat = coords[,"Y"],
                           color = ~pal_color_choice(label),
                           radius = crcrads,
                           opacity = .6,
                           fillOpacity = .6,
                           group = group_label,
                           popup = paste(sep = "<br>",
                                         paste("<b>Animal ID:</b> ", flt_animal_ids[i]),
                                         paste("<b>Sex:</b> ", df$Sex),
                                         paste("<b>Age Class (Capture):</b> ", df$`Age Class (Capture)`),
                                         paste("<b>Capture Date:</b> ", df$`Capture Date`),
                                         paste("<b>Location Date (GMT):</b> ", df$`Location Date (GMT)`)))
        
        layer.group <- c(layer.group, as.character(group_label))
        
      }
      
    }
    
  }
  
  if(collarloc){
    device.map <- device.map %>%
      
      addLayersControl(
        baseGroups = c("Default","Satellite"),
        overlayGroups = c("GMUs","Deployed Cameras","Mountain Lion Camera","Marked Mountain Lion Camera",layer.group),
        options = layersControlOptions(collapsed = FALSE)
      )
  } else {
    device.map <- device.map %>%
      
      addLayersControl(
        baseGroups = c("Default","Satellite"),
        overlayGroups = c("GMUs","Deployed Cameras","Mountain Lion Camera","Marked Mountain Lion Camera"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }

  if(collarloc){
    if(homerange){
      return(list(device.map_grid=device.map_grid,
                  device.map=device.map, 
                  sf_pic_grid=sf_pic_grid,
                  flt_animal_ids = flt_animal_ids,
                  flt_sf_mtlion_pic_grid=flt_sf_mtlion_pic_grid, 
                  flt_sf_mtlion_marked_pic_grid=flt_sf_mtlion_marked_pic_grid, 
                  flt_sf_mtlion_pic=flt_sf_mtlion_pic, 
                  flt_sf_mtlion_marked_pic=flt_sf_mtlion_marked_pic,
                  min_circle_radius = min_circle_radius,
                  max_circle_radius = max_circle_radius))
    } else {
      return(list(device.map_grid=device.map_grid,
                  device.map=device.map, 
                  sf_pic_grid=sf_pic_grid,
                  flt_animal_ids = flt_animal_ids,
                  flt_sf_mtlion_pic_grid=flt_sf_mtlion_pic_grid, 
                  flt_sf_mtlion_marked_pic_grid=flt_sf_mtlion_marked_pic_grid, 
                  flt_collar_sf_gmu=flt_collar_sf_gmu, 
                  flt_sf_mtlion_pic=flt_sf_mtlion_pic, 
                  flt_sf_mtlion_marked_pic=flt_sf_mtlion_marked_pic,
                  min_circle_radius = min_circle_radius,
                  max_circle_radius = max_circle_radius)) 
    }
  } else {
    return(list(device.map_grid=device.map_grid,
                device.map=device.map, 
                sf_pic_grid=sf_pic_grid, 
                flt_sf_mtlion_pic_grid=flt_sf_mtlion_pic_grid, 
                flt_sf_mtlion_marked_pic_grid=flt_sf_mtlion_marked_pic_grid, 
                flt_sf_mtlion_pic=flt_sf_mtlion_pic, 
                flt_sf_mtlion_marked_pic=flt_sf_mtlion_marked_pic,
                min_circle_radius = min_circle_radius,
                max_circle_radius = max_circle_radius))
  }

  
}